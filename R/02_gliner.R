#' Extract entities with GLiNER (Python backend)
#'
#' @description
#' Runs the GLiNER model for Named Entity Recognition (NER) on text data.
#' Accepts either:
#' \itemize{
#'   \item A data.frame with columns \code{doc_id} and \code{text}, or
#'   \item A data.frame produced by \code{clean_text()} (with columns
#'   \code{sen_id}, \code{doc_idx}, \code{sen_idx}, etc.).
#' }
#'
#' If a \code{clean_text()} data.frame is provided, the function internally
#' renames \code{sen_id -> doc_id} and \code{text_clean -> text} for processing,
#' then restores the schema and merges metadata back after NER.
#'
#' @param df data.frame with either:
#'   \itemize{
#'     \item Columns \code{doc_id} and \code{text}, or
#'     \item Output of \code{clean_text()} (with \code{sen_id}, \code{text_clean}).
#'   }
#' @param labels character vector of candidate labels
#' @param batch_size integer, batch size for Python GLiNER (default 16)
#' @param chunk_factor integer, how many batches per Python call (default 100)
#' @param model character, model ID (default: "urchade/gliner_multi-v2.1")
#' @param env character, conda environment name (default: "r-sentiner")
#' @param script optional path to the Python script; by default uses
#'   the installed copy in `inst/python/gliner_runner.py`
#' @param save_output_dir optional directory to keep JSON output files;
#'   if NULL (default), outputs are deleted after reading
#' @param verbose logical, if TRUE (default) print progress updates,
#'   if FALSE run silently
#' @param seed integer, random seed to pass to Python (default 42)
#'
#' @return A data.table with columns:
#' \itemize{
#'   \item \code{id}: Unique entity identifier: \code{doc_idx} + \code{sen_idx} + \code{ent_idx}.
#'   \item \code{sen_id}: Sentence identifier: \code{doc_idx} + \code{sen_idx}
#'   \item \code{doc_idx}: Document index (if available).
#'   \item \code{sen_idx}: Sentence index (if available).
#'   \item \code{ent_idx}: Entity index within sentence.
#'   \item \code{doc_id_u}: Optional user-provided document identifier.
#'   \item \code{group}: Optional group column if provided.
#'   \item \code{text_clean}: Cleaned text of the sentence.
#'   \item \code{entity_name}: Extracted entity string.
#'   \item \code{label}: Predicted entity label.
#'   \item \code{score}: Prediction confidence score.
#'   \item \code{start}: Character start position of the entity.
#'   \item \code{end}: Character end position of the entity.
#'   \item \code{ner_model}: Model identifier used.
#'   \item \code{ner_detected}: POSIXct UTC timestamp of extraction.
#' }
#'
#' @import data.table
#' @export
gliner_extract <- function(df, labels,
                           batch_size = 16L,
                           chunk_factor = 100L,
                           model = "urchade/gliner_multi-v2.1",
                           env = "r-sentiner",
                           script = NULL,
                           save_output_dir = NULL,
                           verbose = TRUE,
                           seed = 42L) {

  # detect input type
  from_clean_text <- all(c("sen_id", "text_clean") %in% names(df))
  if (from_clean_text) {
    df_gliner <- data.table::data.table(
      doc_id = df$sen_id,
      text   = df$text_clean
    )
  } else {
    stopifnot(all(c("doc_id", "text") %in% names(df)))
    df_gliner <- data.table::as.data.table(df)
  }

  # find script
  if (is.null(script)) {
    script <- system.file("python", "gliner_runner.py", package = "sentiner")
    if (script == "") {
      stop("gliner_runner.py not found in package. Did you install correctly?")
    }
  }

  # chunking
  chunk_size <- batch_size * if (!is.null(chunk_factor)) chunk_factor else nrow(df_gliner)
  n <- nrow(df_gliner)
  chunk_ids <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))
  n_chunks <- length(chunk_ids)

  start_time <- Sys.time()
  if (verbose) {
    cli::cli_process_start("Extracting entities: {n_chunks} chunk(s). Please hang on...")
  }

  run_chunk <- function(i, idx) {
    chunk_df <- df_gliner[idx, ]
    payload <- list(
      doc_ids   = as.character(chunk_df$doc_id),
      texts     = vapply(chunk_df$text, as.character, character(1L)),
      labels    = as.character(labels),
      batch_size = batch_size
    )

    input_file  <- tempfile(sprintf("gliner_in_chunk%0*d_", nchar(n_chunks), i), fileext = ".json")
    output_file <- tempfile(sprintf("gliner_out_chunk%0*d_", nchar(n_chunks), i), fileext = ".json")

    yyjsonr::write_json_file(payload, input_file)
    Sys.setenv(GLINER_SEED = as.character(seed))

    result <- system2(
      "conda",
      c("run", "-n", env, "python", script,
        input_file, output_file, model),
      stdout = TRUE,
      stderr = TRUE
    )
    
    if (!is.null(attr(result, "status")) && attr(result, "status") != 0) {
      stop("GLiNER extraction failed:\n", paste(result, collapse = "\n"))
    }

    res <- yyjsonr::read_json_file(output_file) |>
      data.table::as.data.table()

    if (!is.null(save_output_dir)) {
      if (!dir.exists(save_output_dir)) dir.create(save_output_dir, recursive = TRUE)
      file.copy(output_file, file.path(save_output_dir, basename(output_file)))
    }

    unlink(input_file)
    if (is.null(save_output_dir)) unlink(output_file)

    res
  }

  results <- lapply(seq_along(chunk_ids), function(i) run_chunk(i, chunk_ids[[i]]))
  res <- data.table::rbindlist(results, use.names = TRUE, fill = TRUE)

  # ---- post-processing ----
  # rename cols
  data.table::setnames(res,
                       old = c("doc_id", "entity_count"),
                       new = c("sen_id", "ent_idx"),
                       skip_absent = TRUE
  )

  # add id
  res[, id := paste0(sen_id, "_", ent_idx)]

  # merge metadata if from clean_text
  if (from_clean_text) {
    meta <- data.table::as.data.table(df)[, !c("text_orig"), with = FALSE]
    res <- merge(
      res,
      meta,
      by = "sen_id",
      all.x = TRUE
    )
  }

  # add metadata
  res[, ner_model := model]
  res[, ner_detected := as.POSIXct(Sys.time(), tz = "UTC")]

  # enforce column order
  cols_order <- c(
    "id", "sen_id", "doc_idx", "sen_idx", "ent_idx",
    "doc_id_u", "group", "text_clean",
    "entity_name", "label", "score", "start", "end",
    "ner_model", "ner_detected"
  )
  cols_exist <- intersect(cols_order, names(res))
  res <- res[, ..cols_exist]
  data.table::setcolorder(res, cols_exist)

  # sort
  if (all(c("doc_idx", "sen_idx", "ent_idx") %in% names(res))) {
    data.table::setorder(res, doc_idx, sen_idx, ent_idx)
  }

  total_elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
  if (verbose) {
    cli::cli_process_done(msg_done = paste0("Entity extraction completed in ", total_elapsed, "s"))
  }

  return(res[])
}
