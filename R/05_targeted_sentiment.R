#' Get Targeted Sentiment
#' This function performs targeted sentiment analysis on a given dataset using a specified pre-trained model.
#' @param data A data frame or data table containing the text and target entities for sentiment analysis.
#' @param text_col The name of the column in `data` that contains the text
#' to be analyzed. Default is "translation".
#' @param entity_col The name of the column in `data` that contains the target  
#' entities for sentiment analysis. Default is "target".
#' @param id_col The name of the column in `data` that contains unique identifiers
#' for each row. Default is "id".
#' @param conda_env_name The name of the conda environment where the sentiment analysis
#' model is installed. Default is "r-sentiner".
#' @param verbose Logical indicating whether to print progress messages. Default is TRUE.
#' @param model The pre-trained model to use for sentiment analysis. Default is
#' "MoritzLaurer/deberta-v3-base-zeroshot-v2.0".
#' @param batch_size The batch size to use during inference. Default is 32L.
#' @param entailment_index The index of the entailment class in the model's output. Default is 0L.
#' @param contradiction_index The index of the contradiction class in the model's output. Default is 1L.
#' @param fp16 Logical indicating whether to use half-precision floating point
#' during inference. Default is TRUE.
#' @param ... Additional arguments to pass to the `initialize_sentiner` function.
#' @return A data table containing the original data along with the predicted
#' sentiment, sentiment confidence, sentiment model, and sentiment datetime.
#' 
#' @export 
get_targeted_sentiment <- function(data,
                                 text_col = "translation",
                                 entity_col = "target",
                                 id_col = "id",
                                 conda_env_name = "r-sentiner",
                                 verbose = TRUE,
                                 model = "MoritzLaurer/deberta-v3-base-zeroshot-v2.0", 
                                 batch_size = 32L,
                                 entailment_index = 0L,
                                 contradiction_index = 1L,
                                 fp16 = TRUE,

                                 ...){

  vmessage <- function(...) if (verbose) message(...)

  # Assert that data is data frame or table
  assertthat::assert_that(
    is.data.frame(data),
    msg = "Please provide 'data' in data.frame or data.table format."
  )

  data <- data.table::as.data.table(data)

  # 1) Ensure only id_col becomes canonical 'id'
  assertthat::assert_that(!is.null(id_col) && nzchar(id_col), msg = "'id_col' not specified.")
  assertthat::assert_that(id_col %in% names(data), msg = paste0("Column '", id_col, "' (id_col) not found in 'data'."))

  if (!identical(id_col, "id")) {
    data.table::setnames(data, old = id_col, new = "id")
  }

  # 2) Keep full original data (with id = id) for merge-back
  og_data <- data.table::copy(data)

  # 3) If canonical 'text'/'target' exist but user wants different columns, drop them
  assertthat::assert_that(!is.null(text_col) && nzchar(text_col), msg = "'text_col' not specified.")
  assertthat::assert_that(!is.null(entity_col) && nzchar(entity_col), msg = "'entity_col' not specified.")

  assertthat::assert_that(text_col %in% names(data), msg = paste0("Column '", text_col, "' (text_col) not found in 'data'."))
  assertthat::assert_that(entity_col %in% names(data), msg = paste0("Column '", entity_col, "' (entity_col) not found in 'data'."))

  if ("text" %in% names(data) && !identical(text_col, "text")) {
    data[, text := NULL]
  }
  if ("target" %in% names(data) && !identical(entity_col, "target")) {
    data[, target := NULL]
  }

  # 4) Rename specified columns to canonical names (safe now)
  if (!identical(text_col, "text")) {
    data.table::setnames(data, old = text_col, new = "text")
  }
  if (!identical(entity_col, "target")) {
    data.table::setnames(data, old = entity_col, new = "target")
  }

  required_columns <- c("id", "text", "target")
  assertthat::assert_that(
    all(required_columns %in% names(data)),
    msg = "After column handling, not all required columns ('id', 'text', 'target') are present."
  )

  # Subset to what Python needs
  data <- data[, required_columns, with = FALSE]

  # Handling of dot arguments
  dots <- list(...)
  args_init <- dots[names(dots) %in% names(formals(initialize_sentiner))]
  
  # Initialize sentiner conda env
  do.call(initialize_sentiner, c(list(conda_env_name = conda_env_name), args_init)) # Init and pass on ... arguments
  
  # Source the Python script from the package
  reticulate::source_python(
    system.file("python",
                "targ_sentiment.py",
                package = "sentiner",
                mustWork = TRUE)
  )

  start_time <- Sys.time()
  if (verbose) {
    cli::cli_process_start("Classifying targeted sentiment. Please hang on...")
  }

  sen_res <- reticulate::py$classify_sentiments_nli(
    model_path = model, 
    df = data, 
    id_var = "id",
    batch_size = as.integer(batch_size),
    entailment_index = as.integer(entailment_index),
    contradiction_index = as.integer(contradiction_index),
    fp16 = fp16)
  
  # Convert pandas to R
  sen_res <- data.table::as.data.table(reticulate::py_to_r(sen_res))
  
  # Message
  total_elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
  if (verbose) {
    cli::cli_process_done(msg_done = paste0("Targeted sentiment classification completed in ", total_elapsed, "s"))
  }
  
  # Merge back with original data
  res_dt <- 
    merge(
      og_data,
      sen_res[, .(id, sentiment, sentiment_confidence, sentiment_model, sentiment_datetime)],
      by = "id",
      all.x = TRUE
    )

  # Return
  return(res_dt)
  
}