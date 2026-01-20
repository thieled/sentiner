
#' @title Translate Text with Masked Named Entities
#'
#' @description
#' Translates texts while masking named entities to ensure placeholders
#' are preserved during translation. The function integrates with the
#' \pkg{easieRnmt} back-end and automatically retries translations if
#' entity placeholders are lost. It installs and initializes the Python
#' environment if necessary, masks entities, performs translation, repairs
#' placeholders (only for \code{"[:XXNEXX:]"}), retries failed cases, and
#' finally unmasks entities.
#'
#' @param data A data.frame or data.table containing the texts and named entities.
#' @param text_col Character, name of the text column to translate. Default = `"text_clean"`.
#' @param entity_col Character, name of the column with target entity names.
#' Default = `"entity_name"`.
#' @param placeholder Character, placeholder string to replace entities in the text.
#' Repairing is only applied if this equals (after trimming) \code{"[:XXNEXX:]"}.
#' Default = `"[:XXNEXX:]"`.
#' @param id_col Character, name of the identifier column for unique observations.
#' Default = `"id"`.
#' @param conda_env_name Character, name of the Conda environment to use for
#' Python back-end. Default = `"r-sentiner"`.
#' @param targ_lang Character, target language code for translation.
#' Default = `"en"`.
#' @param n_retries Integer, maximum number of retries if placeholders are not
#' preserved in translation. Default = `3L`.
#' @param seed Integer, random seed for reproducibility. Default = `42L`.
#' @param beam_size Integer, beam size for translation decoding. Default = `1L`.
#' @param deterministic Logical, whether to use deterministic translation
#' settings. Default = `TRUE`.
#' @param prob_threshold Numeric, threshold below which the detected language is replaced
#' by `targ_lang` (or by the user-provided \code{lang_guess}). Default = 0.25.
#' @param restore_threshold Numeric, threshold (share of placeholders successfully preserved in translation)
#'  below which a masked translation is considered unsuccessful; Default = `0.10`.
#' @param verbose Logical, whether to print progress messages. Default = `TRUE`.
#' @param save_dir Character. (Optional) Directory where interim translation results should be saved.
#' @param ... Additional arguments passed to either
#' \code{initialize_sentiner()} or \code{easieRnmt::translate()}. Unknown
#' arguments are ignored with a warning.
#'
#' @details
#' The function follows a multi-step process:
#' \enumerate{
#'   \item Initialize Python back-end with \code{initialize_sentiner()}.
#'   \item Install \code{EasyNMT} and \code{fasttext} if missing.
#'   \item Mask named entities in the text with placeholders.
#'   \item Translate masked texts using \pkg{easieRnmt}.
#'   \item Optionally repair placeholders (only if placeholder is
#'   \code{"[:XXNEXX:]"}).
#'   \item Retry translations if placeholders are dropped, incrementing
#'   seed and beam size, and setting \code{deterministic = FALSE} after
#'   two retries.
#'   \item Unmask entities by replacing placeholders with the original names.
#' }
#'
#' @return A data.table with translations and metadata. Columns include:
#' \itemize{
#'   \item \code{id}: Identifier for each text.
#'   \item \code{translation}: Final translation with entities unmasked.
#'   \item \code{translation_masked}: Translation with placeholders still present.
#'   \item \code{lang}, \code{lang_prob}: Detected language and probability.
#'   \item \code{tl_error}: Any translation errors or retry information.
#'   \item \code{tl_datetime}, \code{tl_model}: Translation metadata.
#'   \item Additional columns preserved from the input data.
#' }
#'
#' @seealso \code{\link{initialize_sentiner}}, \code{\link[easieRnmt]{translate}}
#'
#' @import data.table
#' @export
masked_ent_translate <- function(data,
                                 text_col = "text_clean",
                                 entity_col = "entity_name",
                                 placeholder = "[:XXNEXX:]",
                                 id_col = "id",
                                 conda_env_name = "r-sentiner",
                                 targ_lang = "en",
                                 n_retries = 3L,
                                 seed = 42L,
                                 beam_size = 1L,
                                 deterministic = TRUE,
                                 prob_threshold = 0.25,
                                 restore_threshold = 0.10,
                                 verbose = TRUE,
                                 save_dir = NULL,
                                 ...){

  vmessage <- function(...) if (verbose) message(...)

  if (!requireNamespace("easieRnmt", quietly = TRUE)) {
    stop("Package 'easieRnmt' is required; please install it by pak::pak('thieled/easieRnmt') (Suggests).")
  }

  # Handling of dot arguments
  dots <- list(...)
  args_init <- dots[names(dots) %in% names(formals(initialize_sentiner))]
  args_tl <- dots[names(dots) %in% names(formals(easieRnmt::translate))]

  unknown_args <- setdiff(names(dots),
                          c(names(formals(initialize_sentiner)), names(formals(easieRnmt::translate))))
  if (length(unknown_args) > 0) {
    warning("Ignoring unknown arguments: ", paste(unknown_args, collapse = ", "))
  }

  # Step 1: Python back-end installations -----
  do.call(initialize_sentiner, c(list(conda_env_name = conda_env_name), args_init)) # Init and pass on ... arguments

  # Check if EasyNMT and fasttext are installed
  easynmt_avail <- reticulate::py_module_available("easynmt")
  fasttext_avail <- reticulate::py_module_available("fasttext")

  # Install if EasyNMT or fasttext are not available
  if (!all(c(easynmt_avail, fasttext_avail))) {
    vmessage(paste0("Installing EasyNMT and fasttext in: ", conda_env_name))

    # Remove any "force" from args_init
    args_init_no_force <- args_init[setdiff(names(args_init), "force")]

    # Install easynmt; passing on arguments from '. . .' -- only overriding 'force'
    do.call(easieRnmt::install_easynmt,
            c(args_init_no_force, list(conda_env_name = conda_env_name, force = FALSE)))
  }

  # Try counter
  retry_count <- 1L

  # Prepare save_dir
  if(!is.null(save_dir)){

    save_dir_try <- base::file.path(save_dir, paste0("try_", retry_count - 1))

    if (!base::dir.exists(save_dir_try)) {
      base::dir.create(save_dir_try, recursive = TRUE, showWarnings = FALSE)
    }

  }else{
    save_dir_try = NULL
  }


  # Step 2: Data preprocessing
  masked_data <- mask_target(data,
                             text_col = text_col,
                             entity_col = entity_col,
                             id_col = id_col)

  # Step 3: Call Translation
  easieRnmt::initialize_easynmt(conda_env_name = "r-sentiner")

  # Call translation; (do.call to pass on dot arguments)
  tl_res <- do.call(easieRnmt::translate, c(list(x = masked_data,
                                                 text_col = "text_masked",
                                                 id_col = "id",
                                                 targ_lang = targ_lang,
                                                 check_translation = T,
                                                 n_retries = n_retries,
                                                 seed = seed,
                                                 beam_size = beam_size,
                                                 deterministic = deterministic,
                                                 prob_threshold = prob_threshold,
                                                 save_dir = save_dir_try
  ),
  args_tl))

  # Merge translation to masked datatable
  masked_data_tl <- merge(masked_data,
                          tl_res[,.(id, lang, lang_prob, translation, tl_error, tl_datetime, tl_model)],
                          by = "id", all.x = TRUE
  )

  # Step 4: Placeholder replacement

  # Helper function to repair placeholders
  repair_placeholders <- function(x, placeholder = "[:XXNEXX:]") {
    # cores we accept
    cores <- c("XXNEXX", "XXNE", "XNEX", "NEXX", "XNE")
    core_regex <- paste0("(?:", paste(cores, collapse = "|"), ")")

    # Regex: opening bracket/colon, optional spaces/X,
    # core, optional spaces/X, then closing bracket/colon
    full_regex <- paste0(
      "\\[?[: ]*X*?",     # optional opener with Xs
      core_regex,
      "X*[: ]*\\]?"       # optional trailer with Xs
    )

    stringi::stri_replace_all_regex(
      x,
      pattern     = full_regex,
      replacement = placeholder
    )
  }

  # Fix placeholders
  masked_data_tl[, tl_fixed := repair_placeholders(translation)]

  # Count placeholders in original text and preserved placeholders
  masked_data_tl[, n_og_ph := stringi::stri_count_fixed(
    text_masked,
    pattern = placeholder
  )][, n_tl_ph := stringi::stri_count_fixed(
    tl_fixed,
    pattern = placeholder
  )][,
     n_diff_ph := n_og_ph - n_tl_ph
  ][,
    pct_diff_ph := abs(n_diff_ph) / n_og_ph
  ]

  # Step 5: Retries for failures to preserve placeholder

  # Split data in successfully preserved and failure - Revised: use 10% cutoff instead of raw count;
  # also include obs where more (but less than double) placeholders were preserved
  success_tl_dt  <- masked_data_tl[pct_diff_ph < restore_threshold | (n_diff_ph < 0 &  n_tl_ph / n_og_ph <= 2)]
  failed_tl_dt  <- masked_data_tl[!id %in% success_tl_dt$id]

  n_retry <- nrow(failed_tl_dt)

  ### Save interim results
  if(!is.null(save_dir)){

    saveRDS(success_tl_dt, file = file.path(save_dir_try, paste0(paste0("try_", retry_count - 1, "_success.rds"))))
    saveRDS(failed_tl_dt, file = file.path(save_dir_try, paste0(paste0("try_", retry_count - 1, "_fail.rds"))))

  }


  # Retry counter
  while(nrow(failed_tl_dt) >= 1 && retry_count <= n_retries){

    n_retry <- nrow(failed_tl_dt)

    vmessage(paste0("Retrying translation for n = ", n_retry, " texts where entity-placeholder was not preserved."))

    seed = seed + 1L

    beam_size = beam_size + 1L
    if(beam_size > 2L) beam_size = 2L # cap beam size at 2 to avoid OOM
    prob_threshold = prob_threshold + 0.1
    if(retry_count > 2) deterministic = FALSE

    ## Clean non-latin characters
    latin_idx <- is_latin_script_lang(failed_tl_dt[["lang"]])

    failed_tl_dt[latin_idx,
                 text_masked := clean_latin_text(text_masked)
    ]

    # Prepare save_dir
    if(!is.null(save_dir)){

      save_dir_try <- base::file.path(save_dir, paste0("try_", retry_count))

      if (!base::dir.exists(save_dir_try)) {
        base::dir.create(save_dir_try, recursive = TRUE, showWarnings = FALSE)
      }

    }else{
      save_dir_try = NULL
    }

    # Call translation; (do.call to pass on dot arguments)
    retry_tl_res <- do.call(
      easieRnmt::translate,
      c(
        list(
          x = failed_tl_dt,
          text_col = "text_masked",
          id_col = "id",
          targ_lang = targ_lang,
          check_translation = TRUE,
          n_retries = n_retries,
          seed = seed,
          beam_size = beam_size,
          deterministic = deterministic,
          prob_threshold = prob_threshold,
          save_dir = save_dir_try
        ),
        args_tl
      )
    )

    # Merge translation to masked datatable
    retry_masked_data_tl <- merge(masked_data,
                                  retry_tl_res[,.(id, lang, lang_prob, translation, tl_error, tl_datetime, tl_model)],
                                  by = "id", all.y = TRUE
    )

    # Fix placeholders
    retry_masked_data_tl[, tl_fixed := repair_placeholders(translation)]

    # Count placeholders in original text and preserved placeholders
    retry_masked_data_tl[, n_og_ph := stringi::stri_count_fixed(
      text_masked,
      pattern = placeholder
    )][, n_tl_ph := stringi::stri_count_fixed(
      tl_fixed,
      pattern = placeholder
    )][,
       n_diff_ph := n_og_ph - n_tl_ph
    ][,
      pct_diff_ph := abs(n_diff_ph) / n_og_ph
    ]

    # Split data in successfully preserved and failure - Revised: use user-defined threshold (in pct) instead of raw count
    retry_success_tl_dt  <- retry_masked_data_tl[pct_diff_ph < restore_threshold | (n_diff_ph < 0 &  n_tl_ph / n_og_ph <= 2)]
    failed_tl_dt  <- retry_masked_data_tl[!id %in% retry_success_tl_dt$id]

    # retry_success_tl_dt  <- retry_masked_data_tl[pct_diff_ph < restore_threshold]
    # failed_tl_dt  <- retry_masked_data_tl[pct_diff_ph >= restore_threshold | is.na(pct_diff_ph)]

    # Store retry info for successful ones
    retry_success_tl_dt[, tl_error := paste0("Retries to preserve placeholder: ",
                                             retry_count, "; seed: ", seed, "; beam-size: ", beam_size, "; ", tl_error)]

    # Merge successful retries to success_dt
    success_tl_dt <- data.table::rbindlist(list(success_tl_dt, retry_success_tl_dt), use.names = T, fill = T)

    # Save interim result
    if(!is.null(save_dir)){
      saveRDS(success_tl_dt, file = file.path(save_dir_try, paste0(paste0("try_", retry_count, "_success.rds"))))
      saveRDS(failed_tl_dt, file = file.path(save_dir_try, paste0(paste0("try_", retry_count, "_fail.rds"))))
    }

    # Update count
    retry_count <- retry_count + 1L

  }

  # Store retry info for failed ones
  failed_tl_dt[, tl_error := paste0("Retries to preserve placeholder: ",
                                    retry_count - 1L, "; seed: ", seed, "; beam-size: ", beam_size, "; ", tl_error)]

  n_failures <- sum(failed_tl_dt$n_diff_ph)

  vmessage(paste0(
    "Remaining non-preserved placeholders in n = ",
    nrow(failed_tl_dt),
    " observations."
  ))

  # Merge successful retries to success_dt
  out_dt <- data.table::rbindlist(list(success_tl_dt, failed_tl_dt), use.names = T, fill = T)

  # Set order
  out_dt <- out_dt[order(match(id, masked_data_tl$id))]

  # --- Step 6: Unmasking Named Entities ---

  # Masking: Replace the target by placeholder:
  out_dt[, translation := stringi::stri_replace_all_fixed(
    tl_fixed,
    pattern = placeholder,
    replacement = target,
    vectorize_all = TRUE
  )][, translation_masked := tl_fixed][, c("text_masked", "n_og_ph", "n_tl_ph", "tl_fixed") := NULL]


  # Step 7: Cleaning up columns
  data.table::setnames(out_dt, "n_diff_ph", "n_placeholders_missing")

  # enforce last columns in desired order if they exist
  last_cols <- c("lang", "lang_prob",
                 "translation_masked", "translation",
                 "tl_error", "tl_datetime", "tl_model",
                 "n_placeholders_missing")
  exist_last <- intersect(last_cols, names(out_dt))

  # everything else before
  first_cols <- setdiff(names(out_dt), exist_last)

  data.table::setcolorder(out_dt, c(first_cols, exist_last))

  # return
  return(out_dt)

}



#' @title Mask Target Entities in Text
#'
#' @description
#' Replaces target entities in a text column with a standardized placeholder.
#' If greedy_replacement is FALSE (default), only replaces when the target is a
#' standalone token (not embedded in a larger word).
#'
#' @param data A data.frame or data.table containing the texts and target entities.
#' @param text_col Character, name of the text column. Default = "text_clean".
#' @param entity_col Character, name of the column containing the entity names
#'   to be masked. Default = "entity_name".
#' @param id_col Character, name of the identifier column for each entity. Default = "id".
#' @param placeholder Character, placeholder string to replace entities in the text.
#'   Default = \code{"[:XXNEXX:]"}.
#' @param greedy_replacement Logical. If FALSE, replace only token-like occurrences
#'   (not part of a larger word). If TRUE, replace all occurrences as fixed substrings.
#'   Default = FALSE.
#' @param verbose Logical, whether to print warnings and alerts. Default = TRUE.
#'
#' @return A data.table including text_masked and n_sen.
#' @export
mask_target <- function(data,
                        text_col = "text_clean",
                        entity_col = "entity_name",
                        id_col = "id",
                        placeholder = "[:XXNEXX:]",
                        greedy_replacement = FALSE,
                        verbose = TRUE) {

  data <- data.table::as.data.table(data)
  data <- data.table::copy(data)

  required_columns <- c("text", "target", "id")
  alternative_names <- list(id = id_col, target = entity_col, text = text_col)

  missing_columns <- setdiff(required_columns, names(data))

  for (col in missing_columns) {
    alt_name <- alternative_names[[col]]

    assertthat::assert_that(!is.null(alt_name),
                            msg = paste0("'", col, "' not specified and not found in 'data'.")
    )
    assertthat::assert_that(alt_name %in% names(data),
                            msg = paste0("Alternative name for '", col, "' provided but not found in 'data'.")
    )

    data.table::setnames(data, old = alt_name, new = col)
  }

  data[, n_sen := tokenizers::count_sentences(text)]
  if (max(data$n_sen, na.rm = TRUE) > 3L && isTRUE(verbose)) {
    cli::cli_alert(
      "Alert: Text contains observations of more than 3 sentences. Consider preprocessing with sentiner::clean_text()."
    )
  }

  if (isTRUE(greedy_replacement)) {
    data[, text_masked := stringi::stri_replace_all_fixed(
      text,
      pattern = target,
      replacement = placeholder,
      vectorize_all = TRUE,
      case_insensitive = FALSE
    )]
  } else {
    # Token-like replacement:
    # target must NOT be part of a larger word (letters/digits/underscore).
    # Uses Unicode-aware boundaries.
    data[, text_masked := {
      txt <- text
      pat <- target

      ok <- !(is.na(txt) | txt == "" | is.na(pat) | pat == "")
      out <- txt

      if (any(ok)) {
        txt_ok <- txt[ok]
        pat_ok <- pat[ok]

        pat_esc <- stringi::stri_replace_all_regex(
          pat_ok,
          "([\\\\.^$|?*+()\\[\\]{}])",
          "\\\\$1"
        )

        # Word chars: letters/digits/underscore. Boundaries: start/end or non-word.
        bw <- "[^\\p{L}\\p{N}_]"

        rx <- paste0(
          "(?:(?<=^)|(?<=", bw, "))",
          pat_esc,
          "(?:(?=$)|(?=", bw, "))"
        )

        out_ok <- stringi::stri_replace_all_regex(
          txt_ok,
          pattern = rx,
          replacement = placeholder,
          vectorize_all = TRUE,
          case_insensitive = FALSE
        )

        out[ok] <- out_ok
      }

      out
    }]
  }

  data
}



#' Reconstruct Exact Named-Entity Wording From Source Text
#'
#' Attempts to restore the exact surface form (original casing/punctuation/handle-ish affixes)
#' of a named entity from a source text. This is useful when the entity string in
#' \code{entity_col} has no exact match in \code{text_col} due to casing differences,
#' missing punctuation, tokenization artifacts, or short prefixes/suffixes (e.g., \code{@},
#' underscores, or brief alphanumeric attachments).
#'
#' The function searches each text for regex matches of the entity tokens with
#' Unicode-aware boundaries. For multi-token entities, it allows flexible separators
#' (whitespace and punctuation). Optionally, it also allows a short alphanumeric/handle-like
#' prefix and/or suffix around the entity (controlled by \code{max_prefix} and \code{max_suffix}).
#'
#' If multiple matches are found, the most frequent match is selected; ties are broken by
#' choosing the earliest occurrence in the text.
#'
#' @param dt A \code{data.table} containing the source text and entity columns.
#' @param text_col Character scalar. Name of the column containing the source text.
#' @param entity_col Character scalar. Name of the column containing the entity string to reconstruct.
#' @param out_col Character scalar. Name of the output column to create with the reconstructed entity.
#' @param case_insensitive Logical scalar. If \code{TRUE}, matching ignores case.
#' @param max_prefix Integer scalar >= 0. Maximum number of prefix characters allowed before the
#'   entity core in extended matching mode.
#' @param max_suffix Integer scalar >= 0. Maximum number of suffix characters allowed after the
#'   entity core in extended matching mode.
#'
#' @return A copy of \code{dt} with an added column \code{out_col}. If no match is found or
#'   inputs are empty/NA, \code{out_col} is \code{NA_character_}.
#'
#' @export
reconstruct_target <- function(dt,
                               text_col = "text",
                               entity_col = "target",
                               out_col = "exact_target",
                               case_insensitive = TRUE,
                               max_prefix = 3L,
                               max_suffix = 3L) {
  stopifnot(data.table::is.data.table(dt))
  stopifnot(is.character(text_col), length(text_col) == 1L)
  stopifnot(is.character(entity_col), length(entity_col) == 1L)
  stopifnot(is.character(out_col), length(out_col) == 1L)
  stopifnot(is.logical(case_insensitive), length(case_insensitive) == 1L)
  stopifnot(is.numeric(max_prefix), length(max_prefix) == 1L, max_prefix >= 0L)
  stopifnot(is.numeric(max_suffix), length(max_suffix) == 1L, max_suffix >= 0L)

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required.")
  }
  if (!requireNamespace("stringi", quietly = TRUE)) {
    stop("Package 'stringi' is required.")
  }

  if (!text_col %chin% names(dt)) stop("Column '", text_col, "' not found in dt.")
  if (!entity_col %chin% names(dt)) stop("Column '", entity_col, "' not found in dt.")

  max_prefix <- as.integer(max_prefix)
  max_suffix <- as.integer(max_suffix)

  dt <- data.table::copy(dt)

  build_rx <- function(entity, max_prefix, max_suffix) {
    ent <- stringi::stri_trim_both(entity)
    toks <- stringi::stri_split_regex(ent, "\\s+", simplify = FALSE)[[1L]]
    toks <- toks[nzchar(toks)]

    if (!length(toks)) return(NA_character_)

    toks_esc <- stringi::stri_replace_all_regex(
      toks,
      "([\\\\.^$|?*+()\\[\\]{}])",
      "\\\\$1"
    )

    bw <- "[^\\p{L}\\p{N}_]"
    sep <- "(?:[\\s\\p{P}\\p{Cf}]+)"
    core <- paste(toks_esc, collapse = sep)

    strict <- paste0(
      "(?:(?<=^)|(?<=", bw, "))",
      core,
      "(?:(?=$)|(?=", bw, "))"
    )

    pref_cls <- "[\\p{L}\\p{N}_@]"
    suf_cls  <- "[\\p{L}\\p{N}_]"

    pref_0 <- if (max_prefix > 0L) paste0("(?:", pref_cls, "{0,", max_prefix, "})") else ""
    pref_1 <- if (max_prefix > 0L) paste0("(?:", pref_cls, "{1,", max_prefix, "})") else NULL

    suf_0 <- if (max_suffix > 0L) paste0("(?:", suf_cls, "{0,", max_suffix, "})") else ""
    suf_1 <- if (max_suffix > 0L) paste0("(?:", suf_cls, "{1,", max_suffix, "})") else NULL

    end_boundary <- "(?:(?=[\\s\\p{P}\\p{Cf}])|(?=$))"

    ext_alts <- character(0)

    if (!is.null(pref_1)) {
      ext_alts <- c(ext_alts, paste0(
        "(?:(?<=^)|(?<=", bw, "))",
        pref_1, core, suf_0,
        end_boundary
      ))
    }
    if (!is.null(suf_1)) {
      ext_alts <- c(ext_alts, paste0(
        "(?:(?<=^)|(?<=", bw, "))",
        pref_0, core, suf_1,
        end_boundary
      ))
    }

    if (length(ext_alts)) {
      paste0("(?:", paste(c(strict, ext_alts), collapse = "|"), ")")
    } else {
      strict
    }
  }

  dt[, (out_col) := {
    ent <- .SD[[1L]][1L]
    txt <- .SD[[2L]][1L]

    if (is.na(ent) || !nzchar(ent) || is.na(txt) || !nzchar(txt)) {
      NA_character_
    } else {
      rx <- build_rx(ent, max_prefix = max_prefix, max_suffix = max_suffix)
      if (is.na(rx) || !nzchar(rx)) {
        NA_character_
      } else {
        m <- stringi::stri_extract_all_regex(
          txt,
          rx,
          case_insensitive = case_insensitive,
          omit_no_match = TRUE
        )[[1L]]

        if (!length(m)) {
          NA_character_
        } else {
          tab <- sort(table(m), decreasing = TRUE)
          top <- names(tab)[tab == tab[1L]]

          if (length(top) == 1L) {
            top
          } else {
            starts <- vapply(
              top,
              function(x) stringi::stri_locate_first_fixed(txt, x)[1L],
              integer(1L)
            )
            top[which.min(starts)]
          }
        }
      }
    }
  }, by = c(entity_col, text_col), .SDcols = c(entity_col, text_col)]

  dt
}


