#' @title Replace Emojis with Names
#'
#' @description Replaces all emojis in a character vector with their textual names (in :name: style).
#'
#' @param text_vec Character vector of texts.
#'
#' @return Character vector with emojis replaced by names.
#' @export
replace_emoji_with_name <- function(text_vec) {
  if (!requireNamespace("emoji", quietly = TRUE)) {
    stop("Package 'emoji' is required for this function. Please install it.")
  }
  if (!requireNamespace("stringi", quietly = TRUE)) {
    stop("Package 'stringi' is required for this function. Please install it.")
  }

  emoji_vec <- emoji::emoji_name
  emoji_vec <- emoji_vec[grepl("[[:alpha:]]", names(emoji_vec))]

  # emoji → :name: mapping
  replacement_map <- stats::setNames(paste0(" [:", names(emoji_vec), ":] "), unname(emoji_vec))
  emoji_chars <- names(replacement_map)

  # Filter emojis that actually occur in text_vec (to avoid massive unnecessary replace)
  present_idx <- stringi::stri_detect_fixed(
    rep(paste(text_vec, collapse = " "), length(emoji_chars)),
    emoji_chars
  )
  emoji_chars <- emoji_chars[present_idx]
  replacements <- unname(replacement_map[emoji_chars])

  if (length(emoji_chars) == 0L) {
    return(text_vec)
  }

  # Vectorized replacement: replace all present emojis in one sweep
  text_vec <- stringi::stri_replace_all_fixed(
    str = text_vec,
    pattern = emoji_chars,
    replacement = replacements,
    vectorize_all = FALSE
  )

  text_vec
}


#' @title Clean and Normalize Text
#'
#' @description
#' Cleans and optionally tokenizes raw text data. Handles emoji replacement,
#' removal of unsupported characters, number and punctuation normalization,
#' trimming, squishing whitespace, and limiting text length. Returns either
#' a standardized data.table or a character vector.
#'
#' @param x Character vector or data.frame containing texts to clean.
#' @param text_col Character, name of the text column if \code{x} is a data.frame.
#' Ignored if \code{x} is a character vector. Default = "text".
#' @param id_col Character, optional column name in the input data.frame to
#' preserve as an identifier column named \code{doc_id_u}. Default = NULL.
#' @param group_col Character, optional column name in the input data.frame
#' to preserve as a grouping variable column named \code{group}. Default = NULL.
#' @param replace_emojis Logical, whether to replace emojis with placeholder
#' names. Default = TRUE.
#' @param replace_alphaless Logical, whether to replace strings that contain no
#' alphabetic characters with empty strings. Default = TRUE.
#' @param max_char Integer, maximum number of characters per text. Texts longer
#' than this are truncated. Default = 2000.
#' @param tokenize_sentences Logical, whether to split texts with more than
#' 3 sentences into individual sentences. Default = TRUE.
#' @param return_string Logical, if TRUE, return only the cleaned character
#' vector (\code{text_clean}) instead of a full data.table. Default = FALSE.
#' @param verbose Logical, whether to print progress messages. Default = TRUE.
#'
#' @details
#' The returned data.table contains the following columns in fixed order:
#' \itemize{
#'   \item \code{sen_id}: Unique identifier for each sentence row, constructed
#'   as \code{<doc_idx>_<sen_idx>}.
#'   \item \code{doc_idx}: Row index of the original document in the input.
#'   \item \code{sen_idx}: Sentence index within each document (1 for non-tokenized).
#'   \item \code{doc_id_u}: Optional user-provided identifier, if \code{id_col} is given.
#'   \item \code{group}: Optional grouping column, if \code{group_col} is given.
#'   \item \code{text_orig}: Original text before cleaning.
#'   \item \code{text_clean}: Cleaned and normalized text.
#' }
#'
#' @return Either:
#' \itemize{
#'   \item A data.table with the columns described above.
#'   \item Or a character vector of cleaned texts if \code{return_string = TRUE}.
#' }
#'
#' @import data.table
#' @export
clean_text <- function(x,
                       text_col = "text",
                       id_col = NULL,
                       group_col = NULL,
                       replace_emojis = TRUE,
                       replace_alphaless = TRUE,
                       max_char = 2000,
                       tokenize_sentences = TRUE,
                       return_string = FALSE,
                       verbose = TRUE) {

  vmessage <- function(...) if (verbose) message(...)

  # --- Input handling ---
  if (is.character(x)) {
    dt <- data.table::data.table(doc_idx = seq_along(x), text_orig = x)
  } else if (is.data.frame(x)) {
    if (!text_col %in% names(x)) stop("text_col not found in data.frame")
    dt <- data.table::as.data.table(x)
    dt[, doc_idx := .I]
    data.table::setnames(dt, text_col, "text_orig")
    if (!is.null(id_col) && id_col %in% names(dt)) {
      data.table::setnames(dt, id_col, "doc_id_u")
    }
    if (!is.null(group_col) && group_col %in% names(dt)) {
      data.table::setnames(dt, group_col, "group")
    }
  } else {
    stop("x must be a character vector or data.frame")
  }

  # --- Cleaning pipeline ---
  dt[, text_clean := enc2utf8(text_orig)]
  dt[is.na(text_clean), text_clean := ""]
  if (replace_emojis) dt[, text_clean := replace_emoji_with_name(text_clean)]

  dt[, text_clean := stringi::stri_replace_all_regex(
    text_clean, "[^\\p{L}\\p{N}\\p{P}\\p{Zs}]", " "
  )]

  dt[, text_clean := iconv(text_clean, from = "", to = "UTF-8", sub = " ")]
  dt[nchar(text_clean) > max_char, text_clean := substr(text_clean, 1, max_char)]
  if (replace_alphaless) dt[!grepl("[[:alpha:]]", text_clean), text_clean := ""]
  dt[, text_clean := textclean::replace_curly_quote(text_clean)]

  # normalize numbers, collapse quotes, squish whitespace
  dt[, text_clean := stringr::str_replace_all(text_clean, "(\\d)\\.(?=\\d)", "\\1 ")]
  dt[, text_clean := stringr::str_replace_all(text_clean, "(\\d)\\.(?!\\d)", "\\1 ")]
  dt[, text_clean := stringr::str_replace_all(text_clean, '\"{2,}', '"')]
  dt[, text_clean := stringr::str_replace_all(text_clean, "'{2,}", "'")]
  dt[, text_clean := stringr::str_squish(text_clean)]

  # --- Sentence tokenization ---
  if (tokenize_sentences) {
    vmessage("Tokenizing long texts into sentences...")

    dt[, n_sen := tokenizers::count_sentences(text_clean)]
    dt_long <- dt[n_sen > 3]
    dt_short <- dt[n_sen <= 3]

    if (nrow(dt_long) > 0) {
      tokenized_list <- tokenizers::tokenize_sentences(dt_long$text_clean)

      tokenized_dt <- data.table::rbindlist(
        lapply(seq_along(tokenized_list), function(i) {
          sents <- unlist(tokenized_list[[i]])
          data.table::data.table(
            doc_idx = dt_long$doc_idx[i],
            sen_idx = seq_along(sents),
            text_clean = sents
          )
        }),
        use.names = TRUE, fill = TRUE
      )

      meta_cols <- setdiff(names(dt_long), c("text_clean", "n_sen"))
      tokenized_dt <- merge(
        tokenized_dt,
        dt_long[, ..meta_cols],
        by = "doc_idx",
        all.x = TRUE
      )

      if (nrow(dt_short) > 0) {
        dt_short[, sen_idx := 1L]
      }

      dt <- data.table::rbindlist(list(dt_short, tokenized_dt), use.names = TRUE, fill = TRUE)
    } else {
      dt[, sen_idx := 1L]
    }

    dt[, n_sen := NULL]

  } else {
    dt[, sen_idx := 1L]
  }

  # --- Create sen_id ---
  dt[, sen_id := paste0(doc_idx, "_", sen_idx)]

  # --- Column order ---
  cols_order <- c("sen_id", "doc_idx", "sen_idx", "doc_id_u", "group", "text_orig", "text_clean")
  cols_exist <- intersect(cols_order, names(dt))
  dt <- dt[, ..cols_exist]
  data.table::setcolorder(dt, cols_exist)

  # --- Sorting ---
  data.table::setorder(dt, doc_idx, sen_idx)

  if (return_string) {
    return(dt$text_clean)
  } else {
    return(dt[])
  }
}





