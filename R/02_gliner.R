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
#'
#' @return data.table with entity annotations
#' @export
gliner_extract <- function(df, labels,
                           batch_size = 16L,
                           chunk_factor = 100L,
                           model = "urchade/gliner_multi-v2.1",
                           env = "r-sentiner",
                           script = NULL,
                           save_output_dir = NULL) {
  stopifnot(all(c("doc_id", "text") %in% names(df)))

  # resolve Python script location inside the package
  if (is.null(script)) {
    script <- system.file("python", "gliner_runner.py", package = "sentiner")
    if (script == "") {
      stop("gliner_runner.py not found in package. Did you install correctly?")
    }
  }

  # compute chunk size
  chunk_size <- batch_size * if (!is.null(chunk_factor)) chunk_factor else nrow(df)

  # split row indices into chunk groups
  n <- nrow(df)
  chunk_ids <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))
  n_chunks <- length(chunk_ids)

  run_chunk <- function(i, idx) {
    chunk_df <- df[idx, ]

    payload <- list(
      doc_ids = as.character(chunk_df$doc_id),
      texts   = vapply(chunk_df$text, as.character, character(1L)),
      labels  = as.character(labels),
      batch_size = batch_size
    )

    # input/output file names
    input_file  <- tempfile(sprintf("gliner_in_chunk%03d_", i), fileext = ".json")
    output_file <- tempfile(sprintf("gliner_out_chunk%03d_", i), fileext = ".json")

    yyjsonr::write_json_file(payload, input_file)

    message(sprintf("\n[gliner_extract] Processing chunk %d/%d (docs %d–%d)...",
                    i, n_chunks, min(idx), max(idx)))

    # run python externally in given conda env
    res <- tryCatch({
      processx::run(
        "conda",
        c("run", "-n", env, "python", script,
          input_file, output_file, model),
        error_on_status = TRUE,
        echo = TRUE
      )
    }, error = function(e) {
      stop(sprintf("Python execution failed on chunk %d/%d: %s", i, n_chunks, e$message))
    })

    # read result
    dt <- yyjsonr::read_json_file(output_file) |>
      data.table::as.data.table()

    # save or cleanup
    if (!is.null(save_output_dir)) {
      if (!dir.exists(save_output_dir)) dir.create(save_output_dir, recursive = TRUE)
      file.copy(output_file, file.path(save_output_dir, basename(output_file)))
    }

    unlink(input_file)
    if (is.null(save_output_dir)) unlink(output_file)

    dt
  }

  results <- lapply(seq_along(chunk_ids), function(i) run_chunk(i, chunk_ids[[i]]))
  data.table::rbindlist(results, use.names = TRUE, fill = TRUE)
}
