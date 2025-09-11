#' Extract entities with GLiNER (Python backend)
#'
#' @param df data.frame with columns 'doc_id' and 'text'
#' @param labels character vector of candidate labels
#' @param batch_size integer, batch size for Python GLiNER
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
#' @return data.table with entity annotations
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
  stopifnot(all(c("doc_id", "text") %in% names(df)))

  if (is.null(script)) {
    script <- system.file("python", "gliner_runner.py", package = "sentiner")
    if (script == "") {
      stop("gliner_runner.py not found in package. Did you install correctly?")
    }
  }

  # chunking
  chunk_size <- batch_size * if (!is.null(chunk_factor)) chunk_factor else nrow(df)
  n <- nrow(df)
  chunk_ids <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))
  n_chunks <- length(chunk_ids)

  start_time <- Sys.time()
  if (verbose) {
    cli::cli_process_start("Extracting entities: {n_chunks} chunk(s). Please hang on...")
  }

  run_chunk <- function(i, idx) {
    chunk_start <- Sys.time()
    if (verbose) {
      cli::cli_alert_info("Processing chunk {i}/{n_chunks} (docs {min(idx)}–{max(idx)})")
    }

    chunk_df <- df[idx, ]
    payload <- list(
      doc_ids   = as.character(chunk_df$doc_id),
      texts     = vapply(chunk_df$text, as.character, character(1L)),
      labels    = as.character(labels),
      batch_size = batch_size
    )

    input_file  <- tempfile(sprintf("gliner_in_chunk%0*d_", nchar(n_chunks), i), fileext = ".json")
    output_file <- tempfile(sprintf("gliner_out_chunk%0*d_", nchar(n_chunks), i), fileext = ".json")

    yyjsonr::write_json_file(payload, input_file)

    # set reproducibility seed for Python
    Sys.setenv(GLINER_SEED = as.character(seed))

    invisible(
      processx::run(
        "conda",
        c("run", "-n", env, "python", script,
          input_file, output_file, model),
        error_on_status = TRUE,
        echo = FALSE
      )
    )

    res <- yyjsonr::read_json_file(output_file) |>
      data.table::as.data.table()

    if (!is.null(save_output_dir)) {
      if (!dir.exists(save_output_dir)) dir.create(save_output_dir, recursive = TRUE)
      file.copy(output_file, file.path(save_output_dir, basename(output_file)))
    }

    unlink(input_file)
    if (is.null(save_output_dir)) unlink(output_file)

    chunk_elapsed <- round(difftime(Sys.time(), chunk_start, units = "secs"), 1)
    if (verbose) {
      cli::cli_alert_success("Finished chunk {i}/{n_chunks} in {chunk_elapsed}s")
    }

    res
  }

  results <- lapply(seq_along(chunk_ids), function(i) run_chunk(i, chunk_ids[[i]]))

  total_elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
  if (verbose) {
    cli::cli_process_done(msg_done = paste0("Entity extraction completed in ", total_elapsed, "s"))
  }

  data.table::rbindlist(results, use.names = TRUE, fill = TRUE)
}
