#' Prepare annotated data for NLI fine-tuning
#'
#' Converts annotated text–target–label data into NLI format suitable for
#' fine-tuning a natural language inference model.  Creates hypothesis–premise
#' pairs, splits into train / test, and optionally down-samples the training
#' set to one true + one random false hypothesis per observation.
#'
#' @param data A data frame with **one row per unique document–target pair**.
#' @param doc_id_col  Column name for document IDs.
#' @param text_col    Column name for text.
#' @param target_col  Column name for target entities.
#' @param label_col   Column name for labels (e.g. "positive", "neutral",
#'   "negative").  Values should be human-readable strings because they are
#'   inserted verbatim into the hypothesis.
#' @param hypothesis_template A \code{\link[glue]{glue}}-style template.
#'   Must contain the placeholders \code{{target}} and \code{{label}}.
#' @param train_prop Proportion of unique observations allocated to training.
#' @param downsample_train  If \code{TRUE}, keep 1 true + 1 random false
#'   hypothesis per training observation (2 rows / obs).  The test set always
#'   retains all hypotheses (\code{n_hypotheses} rows / obs).
#' @param seed Random seed for reproducibility.
#'
#' @return A named list:
#'   \describe{
#'     \item{train}{Training data frame in NLI long format.}
#'     \item{test}{Test data frame in NLI long format (all hypotheses per obs,
#'       ordered by \code{idx} then \code{label_hypothesis}).}
#'     \item{label_values}{Sorted character vector of unique label values.}
#'     \item{n_hypotheses}{Integer – number of hypotheses per observation.}
#'   }
#' @export
prepare_nli_data <- function(data,
                             doc_id_col = "doc_id",
                             text_col   = "text",
                             target_col = "target",
                             label_col  = "label",
                             hypothesis_template = "The sentiment towards {target} is {label}.",
                             train_prop = 0.7,
                             downsample_train = TRUE,
                             seed = 42L) {

  set.seed(seed)

  # --- Validate inputs ----------------------------------------------------
  required_cols <- c(doc_id_col, text_col, target_col, label_col)
  missing <- setdiff(required_cols, names(data))
  if (length(missing) > 0) {
    stop("Missing columns in `data`: ", paste(missing, collapse = ", "))
  }
  if (!grepl("\\{target\\}", hypothesis_template) ||
      !grepl("\\{label\\}",  hypothesis_template)) {
    stop("`hypothesis_template` must contain both {target} and {label} placeholders.")
  }

  # --- Standardise column names & add idx ---------------------------------
  df <- data |>
    dplyr::rename(
      doc_id         = dplyr::all_of(doc_id_col),
      text           = dplyr::all_of(text_col),
      target         = dplyr::all_of(target_col),
      label_original = dplyr::all_of(label_col)
    ) |>
    dplyr::mutate(label_original = as.character(label_original)) |>
    dplyr::filter(!is.na(label_original)) |>
    dplyr::distinct(doc_id, .keep_all = TRUE) |>
    dplyr::mutate(idx = dplyr::row_number())

  # --- Unique label values ------------------------------------------------
  label_values  <- sort(unique(df$label_original))
  n_hypotheses  <- length(label_values)

  if (n_hypotheses < 2L) {
    stop("Need >= 2 unique label values, found: ", n_hypotheses)
  }

  # --- Expand: one row per (observation × hypothesis) ---------------------
  df_expanded <- df |>
    dplyr::cross_join(tibble::tibble(label_hypothesis = label_values)) |>
    dplyr::mutate(
      hypothesis = glue::glue(
        hypothesis_template,
        target = target,
        label  = label_hypothesis
      ) |> as.character(),
      label              = ifelse(label_original == label_hypothesis, 0L, 1L),
      label_nli_explicit = ifelse(label == 0L, "True", "Not-True")
    ) |>
    # Consistent ordering: critical for compute_metrics chunking on the
    # test set — rows for the same observation must be contiguous.
    dplyr::arrange(idx, label_hypothesis) |>
    dplyr::select(
      idx, doc_id, text, target,
      hypothesis, label, label_nli_explicit, label_original
    )

  # --- Train / test split by idx ------------------------------------------
  unique_ids <- unique(df_expanded$idx)
  train_ids  <- sample(unique_ids, size = floor(length(unique_ids) * train_prop))

  df_train <- df_expanded |> dplyr::filter(idx %in% train_ids)
  df_test  <- df_expanded |> dplyr::filter(!idx %in% train_ids)

  # --- Down-sample training set: 1 true + 1 random false per obs ----------
  if (downsample_train) {
    df_train <- df_train |>
      dplyr::group_by(idx) |>
      dplyr::filter(
        label == 0L |
          (label == 1L &
             dplyr::row_number() == sample(which(label == 1L), 1))
      ) |>
      dplyr::ungroup()
  }

  list(
    train        = df_train,
    test         = df_test,
    label_values = label_values,
    n_hypotheses = as.integer(n_hypotheses)
  )
}


#' Fine-tune an NLI model via Python
#'
#' Prepares NLI data with \code{\link{prepare_nli_data}}, then calls the Python
#' training function via \pkg{reticulate}.
#'
#' @param data Annotated data frame (one row per document–target pair).
#' @param model_name HuggingFace model name or local path.
#' @param run_name Short name for this training run (used in the output
#'   directory and training logs).  Auto-generated from \code{model_name}
#'   if \code{NULL}.
#' @param output_dir Directory for the fine-tuned model.
#'   Auto-generated as \code{models/<run_name>} if \code{NULL}.
#' @param doc_id_col,text_col,target_col,label_col Column names in \code{data}.
#' @param hypothesis_template \code{\link[glue]{glue}} template with
#'   \code{{target}} and \code{{label}} placeholders.
#' @param train_prop Training proportion.
#' @param downsample_train Down-sample false training examples.
#' @param learning_rate,train_batch_size,eval_batch_size,num_epochs,warmup_ratio,weight_decay
#'   Training hyper-parameters.
#' @param seed Random seed.
#' @param use_fp16 Use mixed-precision training (FP16; requires CUDA).
#' @param entailment_index,contradiction_index NLI output indices.
#'   \code{NULL} (default) triggers auto-detection from the model config.
#' @param conda_env_name The name of the conda environment where the sentiment analysis
#' model is installed. Default is "r-sentiner".
#' @param ... Additional arguments to pass to the `initialize_sentiner` function.
#'
#' @return Path to the saved model directory (character).
#' @export
fine_tune_nli <- function(data,
                          model_name = "MoritzLaurer/deberta-v3-base-zeroshot-v2.0",
                          run_name   = NULL,
                          output_dir = NULL,
                          doc_id_col = "doc_id",
                          text_col   = "text",
                          target_col = "target",
                          label_col  = "label",
                          hypothesis_template = "The sentiment towards {target} is {label}.",
                          train_prop = 0.7,
                          downsample_train = TRUE,
                          learning_rate  = 2e-5,
                          train_batch_size = 16L,
                          eval_batch_size  = 80L,
                          num_epochs  = 3L,
                          warmup_ratio = 0.25,
                          weight_decay = 0.1,
                          seed     = 42L,
                          use_fp16 = FALSE,
                          entailment_index    = NULL,
                          contradiction_index = NULL,
                          conda_env_name = "r-sentiner",
                          ...) {

  initialize_sentiner()
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required.")
  }

  # --- Prepare NLI data
  prepared <- prepare_nli_data(
    data       = data,
    doc_id_col = doc_id_col,
    text_col   = text_col,
    target_col = target_col,
    label_col  = label_col,
    hypothesis_template = hypothesis_template,
    train_prop = train_prop,
    downsample_train = downsample_train,
    seed = seed
  )

  message(
    "Data prepared: ",
    nrow(prepared$train), " train rows (",
    dplyr::n_distinct(prepared$train$idx), " obs \u00d7 ",
    nrow(prepared$train) / dplyr::n_distinct(prepared$train$idx), " rows/obs), ",
    nrow(prepared$test), " test rows (",
    dplyr::n_distinct(prepared$test$idx), " obs \u00d7 ",
    prepared$n_hypotheses, " hyp/obs), ",
    "labels: ", paste(prepared$label_values, collapse = ", ")
  )

  # --- Auto-generate run_name & output_dir 
  if (is.null(run_name)) {
    run_name <- basename(model_name) |>
      stringr::str_replace_all("[[:punct:]]", "-") |>
      paste0("_fine-tuned")
  }
  if (is.null(output_dir)) {
    output_dir <- file.path("models", run_name)
  }
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Initialize Conda env
  dots <- list(...)
  args_init <- dots[names(dots) %in% names(formals(initialize_sentiner))]
  
  # Initialize sentiner conda env
  do.call(initialize_sentiner, c(list(conda_env_name = conda_env_name), args_init)) # Init and pass on ... arguments

  # # --- Source Python script 
  # reticulate::source_python(python_script)

  reticulate::source_python(
    system.file("python",
                "fine_tune_fct.py",
                package = "sentiner",
                mustWork = TRUE)
  )

  # --- Coerce types for Python --------------------------------------------
  seed             <- as.integer(seed)
  train_batch_size <- as.integer(train_batch_size)
  eval_batch_size  <- as.integer(eval_batch_size)
  num_epochs       <- as.integer(num_epochs)
  n_hypotheses     <- as.integer(prepared$n_hypotheses)

  # NULL → Python None  (reticulate handles this natively)
  py_ent <- if (!is.null(entailment_index))    as.integer(entailment_index)    else NULL
  py_con <- if (!is.null(contradiction_index)) as.integer(contradiction_index) else NULL

  # --- Call Python training -----------------------------------------------
  result <- train_nli_model(
    train_data          = prepared$train,
    test_data           = prepared$test,
    n_hypotheses        = n_hypotheses,
    model_name          = model_name,
    run_name            = run_name,
    output_dir          = output_dir,
    learning_rate       = learning_rate,
    train_batch_size    = train_batch_size,
    eval_batch_size     = eval_batch_size,
    num_epochs          = num_epochs,
    warmup_ratio        = warmup_ratio,
    weight_decay        = weight_decay,
    seed                = seed,
    use_fp16            = use_fp16,
    entailment_index    = py_ent,
    contradiction_index = py_con
  )

  result
}

