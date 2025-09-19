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


#' Add Sentence Context to an Entity-Level Data Table
#'
#' @description
#' Reduces an entity-level data table to sentence-level observations and
#' reconstructs the surrounding context of each sentence. For every
#' `(doc_idx, sen_idx)` pair, the function identifies the sentence text,
#' extracts the preceding and succeeding sentence text (if available),
#' and constructs a new context column consisting of
#' `previous + current + next`. The enriched context is then merged back
#' to the original data table.
#'
#' @param dt A [data.table::data.table()] with at least the columns
#'   `"doc_idx"`, `"sen_idx"`, and the column specified in `text_col`.
#'   Each row should represent one entity.
#' @param text_col Character scalar. The name of the column containing
#'   the sentence text. Defaults to `"text"`.
#'
#' @return A [data.table::data.table()] with the same number of rows
#'   as the input, enriched by a new column named
#'   `paste0(text_col, "_context")`, which contains the surrounding
#'   sentence context for each row.
#'
#' @details
#' - Context columns `<text_col>_prev` and `<text_col>_next` are created
#'   internally to build the context but are removed before returning.
#' - The context column concatenates the previous sentence (if present),
#'   the current sentence, and the next sentence (if present), with
#'   extra whitespace removed.
#' - If multiple rows exist per `(doc_idx, sen_idx)` pair, only the first
#'   sentence text is used.
#'
#' @export
#' @import data.table
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' dt <- data.table(
#'   id = 1:4,
#'   doc_idx = c(1, 1, 1, 2),
#'   sen_idx = c(1, 1, 2, 1),
#'   ent_idx = 1:4,
#'   text = c("Hello world.", "Hello world.", "Next sentence.", "Another doc.")
#' )
#' contextualize_sentences(dt, text_col = "text")
#' }
contextualize_sentences <- function(dt, text_col = "text") {
  if (!data.table::is.data.table(dt)) dt <- data.table::as.data.table(dt)
  req <- c("doc_idx", "sen_idx", text_col)
  if (!all(req %in% names(dt))) {
    stop("dt must contain columns: ", paste(req, collapse = ", "))
  }

  # keep original order for restoration
  tmp <- data.table::copy(dt)
  tmp[, `__rowid__` := seq_len(.N)]

  # unique sentences per (doc_idx, sen_idx)
  sent_dt <- tmp[, .(txt = data.table::first(get(text_col))),
                 by = .(doc_idx, sen_idx)]
  data.table::setnames(sent_dt, "txt", text_col)
  data.table::setorder(sent_dt, doc_idx, sen_idx)

  prev_col    <- paste0(text_col, "_prev")
  next_col    <- paste0(text_col, "_next")
  context_col <- paste0(text_col, "_context")

  # previous and next sentence text within each document
  sent_dt[, (prev_col) := data.table::shift(get(text_col), type = "lag"),  by = doc_idx]
  sent_dt[, (next_col) := data.table::shift(get(text_col), type = "lead"), by = doc_idx]

  # combined context
  sent_dt[, (context_col) := {
    p  <- get(prev_col)
    c0 <- get(text_col)
    n  <- get(next_col)
    s  <- paste(
      data.table::fifelse(is.na(p), "", p),
      c0,
      data.table::fifelse(is.na(n), "", n)
    )
    base::trimws(base::gsub("\\s+", " ", s))
  }]

  # restrict to merge columns
  merge_cols <- c("doc_idx", "sen_idx", prev_col, next_col, context_col)
  sent_small <- sent_dt[, ..merge_cols]

  # left join back to original rows
  out <- merge(
    tmp, sent_small,
    by = c("doc_idx", "sen_idx"),
    all.x = TRUE,
    sort = FALSE
  )
  data.table::setorder(out, `__rowid__`)
  out[, `__rowid__` := NULL]

  # remove helper columns
  out[, c(prev_col, next_col) := NULL]

  # safety check
  if (nrow(out) != nrow(dt)) {
    stop("Row count changed after merge; check uniqueness of (doc_idx, sen_idx).")
  }

  out[]
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
#' @param max_words Integer, maximum number of words per sentence or chunk.
#' Long sentences are split into chunks of this size. Default = 30.
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
#'   \item \code{sen_idx}: Sentence/chunk index within each document.
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
                       max_words = 30,
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

  # --- Sentence + chunk tokenization ---
  if (tokenize_sentences) {
    vmessage("Tokenizing long texts into sentences and chunks...")

    tokenized_list <- tokenizers::tokenize_sentences(dt$text_clean)

    tokenized_dt <- data.table::rbindlist(
      lapply(seq_along(tokenized_list), function(i) {
        sents <- unlist(tokenized_list[[i]])

        # further split long sentences by max_words
        sents_split <- unlist(lapply(seq_along(sents), function(j) {
          if (tokenizers::count_words(sents[j]) > max_words) {
            unlist(tokenizers::chunk_text(sents[j], doc_id = paste0(i, "_", j), chunk_size = max_words))
          } else {
            sents[j]
          }
        }))

        data.table::data.table(
          doc_idx = dt$doc_idx[i],
          sen_idx = seq_along(sents_split),
          text_clean = sents_split
        )
      }),
      use.names = TRUE, fill = TRUE
    )

    meta_cols <- setdiff(names(dt), "text_clean")
    tokenized_dt <- merge(
      tokenized_dt,
      dt[, ..meta_cols],
      by = "doc_idx",
      all.x = TRUE
    )

    dt <- tokenized_dt
  } else {
    dt[, sen_idx := 1L]
  }

  # --- Create sen_id ---
  dt[, sen_id := paste0(doc_idx, "_", sen_idx)]

  # --- Contextualize sentences: Previous and next sentences ---
  dt <- contextualize_sentences(dt, text_col = "text_clean")

  # --- Column order ---
  cols_order <- c("sen_id", "doc_idx", "sen_idx", "doc_id_u", "group", "text_orig", "text_clean", "text_clean_context")
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




