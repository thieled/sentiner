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
#' The GLiNER model is loaded once per session and cached; subsequent calls
#' with the same model reuse the in-memory instance.
#'
#' @param df data.frame with either:
#'   \itemize{
#'     \item Columns \code{doc_id} and \code{text}, or
#'     \item Output of \code{clean_text()} (with \code{sen_id}, \code{text_clean}).
#'   }
#' @param labels character vector of candidate labels
#' @param batch_size integer, batch size for GLiNER inference (default 16)
#' @param chunk_factor integer, how many batches per inference call (default 100)
#' @param threshold numeric, confidence threshold for entity extraction (default 0.1).
#'   Lower values (e.g., 0.1) are more sensitive and extract more entities.
#'   Higher values (e.g., 0.3-0.5) are more conservative.
#' @param model character, model ID (default: "urchade/gliner_multi-v2.1")
#' @param verbose logical, if TRUE (default) print progress updates,
#'   if FALSE run silently
#' @param seed integer, random seed (default 42)
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
                           threshold = 0.1,
                           model = "urchade/gliner_multi-v2.1",
                           verbose = TRUE,
                           seed = 42L) {

  initialize_sentiner()

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

  # load (or reuse cached) GLiNER model
  torch <- reticulate::import("torch")
  if (is.null(.env$gliner_model) || !identical(.env$gliner_model_name, model)) {
    Sys.setenv(
      HF_HUB_DISABLE_PROGRESS_BARS = "1",
      TOKENIZERS_PARALLELISM       = "false"
    )
    device <- if (torch$cuda$is_available()) "cuda" else "cpu"
    if (verbose) cli::cli_alert_info("Loading GLiNER model: {model} on {device}")
    gliner_mod <- reticulate::import("gliner")
    reticulate::import("transformers")$logging$set_verbosity_error()
    m <- gliner_mod$GLiNER$from_pretrained(model)
    m <- m$to(device)
    m$eval()
    .env$gliner_model      <- m
    .env$gliner_model_name <- model
  }
  gliner_model <- .env$gliner_model

  # reproducibility
  seed_i     <- as.integer(seed)
  random_mod <- reticulate::import("random")
  np         <- reticulate::import("numpy")
  random_mod$seed(seed_i)
  np$random$seed(seed_i)
  torch$manual_seed(seed_i)
  if (torch$cuda$is_available()) {
    torch$cuda$manual_seed(seed_i)
    torch$cuda$manual_seed_all(seed_i)
  }
  cudnn <- torch$backends$cudnn
  reticulate::py_set_attr(cudnn, "deterministic", TRUE)
  reticulate::py_set_attr(cudnn, "benchmark", FALSE)

  # chunking
  chunk_size <- batch_size * if (!is.null(chunk_factor)) chunk_factor else nrow(df_gliner)
  n          <- nrow(df_gliner)
  chunk_ids  <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))
  n_chunks   <- length(chunk_ids)

  start_time <- Sys.time()
  if (verbose) {
    cli::cli_process_start("Extracting entities: {n_chunks} chunk(s). Please hang on...")
  }

  labels_py <- reticulate::r_to_py(as.list(as.character(labels)))

  run_chunk <- function(i, idx) {
    chunk_df  <- df_gliner[idx, ]
    texts_r   <- as.character(chunk_df$text)
    doc_ids_r <- as.character(chunk_df$doc_id)

    batch_ents <- gliner_model$inference(
      reticulate::r_to_py(as.list(texts_r)),
      labels_py,
      flat_ner    = TRUE,
      threshold   = threshold,
      multi_label = FALSE,
      batch_size  = as.integer(batch_size)
    )

    rows <- vector("list", length(texts_r))
    for (j in seq_along(texts_r)) {
      ents <- batch_ents[[j]]
      if (length(ents) == 0L) next
      rows[[j]] <- data.table::data.table(
        doc_id       = doc_ids_r[j],
        entity_count = seq_len(length(ents)),
        entity_name  = vapply(ents, function(e) as.character(e[["text"]]),  character(1L)),
        label        = vapply(ents, function(e) {
          v <- e[["label"]]; if (is.null(v)) NA_character_ else as.character(v)
        }, character(1L)),
        score        = vapply(ents, function(e) {
          v <- e[["score"]]; if (is.null(v)) NA_real_ else as.numeric(v)
        }, numeric(1L)),
        start        = vapply(ents, function(e) as.integer(e[["start"]]) + 1L, integer(1L)),
        end          = vapply(ents, function(e) as.integer(e[["end"]])   + 1L, integer(1L))
      )
    }
    data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
  }

  results <- lapply(seq_along(chunk_ids), function(i) run_chunk(i, chunk_ids[[i]]))
  res     <- data.table::rbindlist(results, use.names = TRUE, fill = TRUE)

  # ---- post-processing ----
  data.table::setnames(res,
                       old = c("doc_id", "entity_count"),
                       new = c("sen_id", "ent_idx"),
                       skip_absent = TRUE)

  res[, id := paste0(sen_id, "_", ent_idx)]

  if (from_clean_text) {
    meta <- data.table::as.data.table(df)[, !c("text_orig"), with = FALSE]
    res  <- merge(res, meta, by = "sen_id", all.x = TRUE)
  }

  res[, ner_model    := model]
  res[, ner_detected := as.POSIXct(Sys.time(), tz = "UTC")]

  cols_order <- c(
    "id", "sen_id", "doc_idx", "sen_idx", "ent_idx",
    "doc_id_u", "group", "text_clean",
    "entity_name", "label", "score", "start", "end",
    "ner_model", "ner_detected"
  )
  cols_exist <- intersect(cols_order, names(res))
  res        <- res[, ..cols_exist]
  data.table::setcolorder(res, cols_exist)

  if (all(c("doc_idx", "sen_idx", "ent_idx") %in% names(res))) {
    data.table::setorder(res, doc_idx, sen_idx, ent_idx)
  }

  total_elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
  if (verbose) {
    cli::cli_process_done(msg_done = paste0("Entity extraction completed in ", total_elapsed, "s"))
  }

  return(res[])
}
