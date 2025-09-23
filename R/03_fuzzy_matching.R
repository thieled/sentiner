#' @title Split and Clean Label Strings
#'
#' @description
#' Splits a label column into long format, applies trimming, removes duplicates,
#' and optionally disambiguates labels globally or within groups.
#'
#' @param dt A data.frame or data.table with at least an ID and label column.
#' @param id_col Character. Name of the label-ID column. Default is "id".
#' @param label_col Character. Name of the label column. Default is "label".
#' @param group_col Character or NULL. Optional group column name to be used
#' in disambiguation. Default is NULL.
#' @param pattern Character regex pattern to split labels. Default is "\\|".
#' @param keep_orig Logical. Keep the original labels as rows. Default is TRUE.
#' @param keep_first Logical. Keep only the first split label per row. Default is FALSE.
#' @param disambiguate Logical. Remove labels linked to more than one unique ID
#'   (globally or within groups). Default is TRUE.
#'
#' @return A data.table with columns `id` (user-specified name), `label`,
#'   and optionally the user-specified group column.
#'
#' @import data.table
#' @export
split_labels <- function(dt,
                         id_col = "id",
                         label_col = "label",
                         group_col = NULL,
                         pattern = "\\|",
                         keep_orig = TRUE,
                         keep_first = FALSE,
                         disambiguate = TRUE) {
  # Ensure data.table
  dt <- data.table::as.data.table(dt)
  dt <- data.table::copy(dt)

  # Standardize column names internally
  cn <- list(id = id_col, label = label_col, group = group_col)
  setnames(
    dt,
    old = c(cn$id, cn$label, if (!is.null(cn$group)) cn$group),
    new = c("..id", "..label", if (!is.null(cn$group)) "..group"),
    skip_absent = TRUE
  )

  # Split labels into long format with numbering
  dt_long <- dt[, {
    parts <- unlist(strsplit(as.character(..label), pattern, perl = TRUE))
    parts <- trimws(parts)
    parts <- parts[!(is.na(parts) | parts == "")]
    if (length(parts) == 0L) {
      list(label = character(0), split_no = integer(0))
    } else {
      list(label = parts, split_no = seq_along(parts))
    }
  }, by = c("..id", if (!is.null(cn$group)) "..group", "..label")]

  # keep only first split if requested
  if (keep_first) {
    dt_long <- dt_long[split_no == 1]
  }

  # drop helper
  dt_long[, split_no := NULL]

  # add original if requested
  if (keep_orig) {
    if (is.null(cn$group)) {
      dt_orig <- unique(dt[, .(..id, label = trimws(..label))])
    } else {
      dt_orig <- unique(dt[, .(..id, label = trimws(..label), ..group)])
    }
    dt_orig <- dt_orig[!(is.na(label) | label == "")]
    dt_long <- data.table::rbindlist(list(dt_long, dt_orig), use.names = TRUE, fill = TRUE)
  }

  # Deduplicate
  dt_long <- unique(dt_long, by = c("..id", "label", if (!is.null(cn$group)) "..group"))

  # Disambiguation
  if (disambiguate) {
    if (is.null(cn$group)) {
      dt_long <- dt_long[, if (uniqueN(..id) == 1) .SD, by = label]
    } else {
      dt_long <- dt_long[, if (uniqueN(..id) == 1) .SD, by = .(..group, label)]
    }
  }

  # Restore user column names
  old_names <- c("..id", if (!is.null(cn$group)) "..group", "label")
  new_names <- c(cn$id, if (!is.null(cn$group)) cn$group, "label")
  setnames(dt_long, old = old_names, new = new_names, skip_absent = TRUE)

  # Keep only relevant cols
  keep_cols <- c(cn$id, if (!is.null(cn$group)) cn$group, "label")
  return(dt_long[, ..keep_cols])
}





#' Fuzzy-match strings against a reference table (optionally: within groups)
#'
#' @description
#' Matches a (messy) text column in `input` to a (cleaned) label column in `target`,
#' optionally returning IDs from both sides. If `group_vars` are specified, matching
#' is constrained within group columns shared by both data frames (e.g., country).
#' Exact matches are attempted first, followed by optional fuzzy matching.
#' Poor matches, where the similarit distance exeeds `threshold` are flagged and have
#' their label/target ID set to `NA`. Additional options allow deduplication
#' by input (keeping `best_by_input_id`) or by target IDs (flagging the worse duplicate
#' match as poor, `best_by_target_id`). The function is useful to match IDs
#' of known entities to an input data.frame that lacks these IDs.
#'
#' @param input A data frame containing the messy text to be matched.
#' @param target A data frame containing the harmonized labels.
#' @param input_col A string giving the column name in `input` with messy text values.
#' @param target_col A string giving the column name in `target` with harmonized labels.
#' @param input_id_col A string naming an optional ID column in `input` to pass through.
#'   Default `NULL`.
#' @param target_id_col A string naming an optional ID column in `target` to return for matches.
#'   Default `NULL`.
#' @param group_vars A character vector of column names present in both `input` and `target`
#'   that define matching groups. Matching is only performed within groups.
#'   Default `NULL`.
#' @param fuzzy Logical. Whether to perform fuzzy matching for non-exact rows.
#'   Default `TRUE`.
#' @param stopwords A character vector of tokens to remove before distance calculation.
#'   Default `NULL`.
#' @param method A string giving the string distance method passed to `stringdist`.
#'   Default `"jw"`.
#' @param p A numeric tuning parameter for the distance method (e.g., Jaro–Winkler).
#'   Default `0.2`.
#' @param threshold A numeric distance cutoff in range 0-1. Matches with distance greater
#'   than this are considered poor and set to `NA`. Default `0.20`.
#' @param normalize Either `"none"` or `"length"`. Whether to normalize distances.
#'   Default `"none"`.
#' @param tolower Logical. If `TRUE`, text is converted to lowercase before matching.
#'   Default `TRUE`.
#' @param verbose Logical. If `TRUE`, prints a brief summary of matches. Default `TRUE`.
#' @param best_by_input_id Logical. If `TRUE` and `input_id_col` is provided, deduplicates
#'   the output by input ID, keeping only the row with the lowest distance (exact matches
#'   preferred, `NA` treated as worst). Default `FALSE`.
#' @param best_by_target_id Logical. If `TRUE` and `target_id_col` is provided, flags
#'   duplicate matches by target ID. For each target ID, the match with the lowest distance
#'   is kept as-is, and all others are flagged with `poor_match = TRUE`. No rows are dropped.
#'   Default `FALSE`.
#'
#' @return A data frame with the following columns:
#'   * `group_vars` (if supplied),
#'   * `input_id` (if supplied),
#'   * `input` (original values from input),
#'   * `matched_target` (best match from target),
#'   * `target_id` (if supplied),
#'   * `exact_match` (logical, whether exact match),
#'   * `distance` (string distance to match),
#'   * `poor_match` (logical, TRUE if flagged as poor or duplicate).
#' @importFrom stats setNames
#' @import data.table
#' @export
fuzzy_match_df <- function(
    input,
    target,
    input_col,
    target_col,
    input_id_col  = NULL,
    target_id_col = NULL,
    group_vars = NULL,
    fuzzy = TRUE,
    stopwords = NULL,
    method = "jw",
    p = 0.2,
    threshold = 0.20,
    normalize = c("none","length"),
    tolower = TRUE,
    verbose = TRUE,
    best_by_input_id = FALSE,
    best_by_target_id = FALSE
) {
  if (!requireNamespace("stringdist", quietly = TRUE)) {
    stop("Package 'stringdist' is required; please install it (Suggests).")
  }
  if (!requireNamespace("fastmatch", quietly = TRUE)) {
    stop("Package 'fastmatch' is required; please install it (Suggests).")
  }

  input  <- base::as.data.frame(input,  stringsAsFactors = FALSE)
  target <- base::as.data.frame(target, stringsAsFactors = FALSE)
  normalize <- base::match.arg(normalize)

  if (!is.data.frame(input))  stop("'input' must be a data.frame.")
  if (!is.data.frame(target)) stop("'target' must be a data.frame.")
  if (!input_col %in% names(input))   stop("`input_col` not found in `input`.")
  if (!target_col %in% names(target)) stop("`target_col` not found in `target`.")
  if (!is.null(input_id_col)  && !input_id_col  %in% names(input))  stop("`input_id_col` not found in `input`.")
  if (!is.null(target_id_col) && !target_id_col %in% names(target)) stop("`target_id_col` not found in `target`.")

  if (is.null(group_vars)) group_vars <- character(0)
  if (length(group_vars)) {
    miss_in  <- setdiff(group_vars, names(input))
    miss_tar <- setdiff(group_vars, names(target))
    if (length(miss_in))  stop("Group vars missing in `input`: ", paste(miss_in, collapse = ", "))
    if (length(miss_tar)) stop("Group vars missing in `target`: ", paste(miss_tar, collapse = ", "))
  }

  norm_text <- function(x) if (tolower) base::tolower(x) else x
  strip_tokens <- function(vec, sw) {
    if (is.null(sw)) return(vec)
    base::vapply(
      vec,
      function(s) base::paste(base::setdiff(base::strsplit(s, "\\s+")[[1]], sw), collapse = " "),
      FUN.VALUE = character(1)
    )
  }

  input_key <- if (length(group_vars)) {
    base::interaction(input[, group_vars, drop = FALSE], drop = TRUE, lex.order = TRUE)
  } else factor(rep(1L, nrow(input)))
  target_key <- if (length(group_vars)) {
    base::interaction(target[, group_vars, drop = FALSE], drop = TRUE, lex.order = TRUE)
  } else factor(rep(1L, nrow(target)))

  out_input          <- input[[input_col]]
  out_input_id       <- if (is.null(input_id_col)) base::rep(NA_character_, nrow(input)) else input[[input_id_col]]
  out_matched_target <- base::rep(NA_character_, nrow(input))
  out_target_id      <- base::rep(NA_character_, nrow(input))
  out_exact          <- base::rep(FALSE,        nrow(input))
  out_distance_raw   <- base::rep(NA_real_,     nrow(input))
  out_poor           <- base::rep(NA,           nrow(input))

  input_split_idx  <- split(seq_len(nrow(input)),  input_key, drop = TRUE)
  target_split_idx <- split(seq_len(nrow(target)), target_key, drop = TRUE)

  for (g in names(input_split_idx)) {
    ii <- input_split_idx[[g]]
    tt <- target_split_idx[[g]]
    if (is.null(tt) || length(tt) == 0L) next

    in_vec_raw  <- input[[input_col]][ii]
    tar_vec_raw <- target[[target_col]][tt]
    id_vec_raw  <- if (is.null(target_id_col)) rep(NA_character_, length(tt)) else target[[target_id_col]][tt]

    in_norm  <- norm_text(in_vec_raw)
    tar_norm <- norm_text(tar_vec_raw)

    exact_idx <- fastmatch::fmatch(in_norm, tar_norm, nomatch = NA_integer_)
    out_exact[ii] <- !is.na(exact_idx)

    match_idx <- exact_idx
    dist_vec  <- base::rep(NA_real_, length(ii))

    need_fuzzy <- is.na(exact_idx)
    if (any(need_fuzzy) && isTRUE(fuzzy)) {
      in_need   <- in_norm[need_fuzzy]
      tar_cands <- tar_norm
      in_need2   <- strip_tokens(in_need,   stopwords)
      tar_cands2 <- strip_tokens(tar_cands, stopwords)

      if (length(tar_cands2) > 0) {
        dm <- stringdist::stringdistmatrix(in_need2, tar_cands2, method = method, p = p)
        dm <- as.matrix(dm)  # ensure it's always a 2D matrix

        if (ncol(dm) > 0) {
          min_idx <- apply(dm, 1L, which.min)
          min_dst <- apply(dm, 1L, min)

          # ensure correct length
          if (length(min_idx) == length(in_need2)) {
            match_idx[need_fuzzy] <- min_idx
            dist_vec[need_fuzzy]  <- min_dst
          }
        }
      } else {
        # No candidates → leave NA
        match_idx[need_fuzzy] <- NA_integer_
        dist_vec[need_fuzzy]  <- NA_real_
      }
    }



    dist_vec[!is.na(exact_idx)] <- 0

    have_match <- !is.na(match_idx)
    if (any(have_match)) {
      out_matched_target[ii[have_match]] <- tar_vec_raw[match_idx[have_match]]
      if (!is.null(target_id_col)) out_target_id[ii[have_match]] <- id_vec_raw[match_idx[have_match]]
      out_distance_raw[ii] <- dist_vec
    }

    if (normalize == "length") {
      denom <- base::rep(1, length(ii))
      if (any(have_match)) {
        mt <- out_matched_target[ii]
        denom <- base::ifelse(!is.na(mt), base::pmin(base::sqrt(base::nchar(mt)), 5), 1)
        denom[denom == 0 | base::is.na(denom)] <- 1
      }
      out_distance_raw[ii] <- dist_vec / denom
    } else {
      out_distance_raw[ii] <- dist_vec
    }

    pmask <- !is.na(out_distance_raw[ii]) & have_match
    poor  <- base::rep(NA, length(ii))
    poor[pmask] <- out_distance_raw[ii][pmask] > threshold

    if (any(poor %in% TRUE)) {
      idx_bad <- ii[which(poor %in% TRUE)]
      out_matched_target[idx_bad] <- NA_character_
      if (!is.null(target_id_col)) out_target_id[idx_bad] <- NA_character_
    }

    out_poor[ii] <- poor
  }

  out <- base::data.frame(
    input_id       = out_input_id,
    input          = out_input,
    matched_target = out_matched_target,
    target_id      = if (is.null(target_id_col)) base::rep(NA_character_, nrow(input)) else out_target_id,
    exact_match    = out_exact,
    distance       = out_distance_raw,
    poor_match     = out_poor,
    stringsAsFactors = FALSE
  )

  if (length(group_vars)) {
    out <- base::cbind(input[, group_vars, drop = FALSE], out, stringsAsFactors = FALSE)
  }

  # Deduplicate by input_id (keep best)
  dedup_best <- function(dt, id_var) {
    if (!id_var %in% names(dt)) return(dt)
    if (all(is.na(dt[[id_var]]))) return(dt)

    dt <- data.table::as.data.table(dt)
    dt[, .dist := fifelse(is.na(distance), Inf, distance)]
    dt[, .exact := fifelse(exact_match, 0L, 1L)]

    # arrange by id, then exactness, then distance
    data.table::setorder(dt, ..id_var, .exact, .dist)

    # keep first per id_var
    dt <- dt[, .SD[1L], by = ..id_var]

    dt[, c(".dist", ".exact") := NULL]
    data.table::setDF(dt) # return as data.frame for compatibility
    dt
  }

  # best_by_target_id -> flag worse duplicates instead of dropping
  if (isTRUE(best_by_target_id) && !is.null(target_id_col) && "target_id" %in% names(out)) {
    dt <- data.table::as.data.table(out)

    dt[, .adj_dist := fifelse(is.na(distance), Inf, distance)]

    dt[, .best_dist := suppressWarnings(min(.adj_dist, na.rm = TRUE)),
       by = target_id]

    dt[, .flag_worse := !is.na(target_id) & (.adj_dist > .best_dist)]

    dt[, poor_match := fifelse(.flag_worse, TRUE, poor_match)]

    dt[, c(".adj_dist", ".best_dist", ".flag_worse") := NULL]

    out <- data.table::setDF(dt)  # back to data.frame
  }

  if (isTRUE(verbose)) {
    n_total   <- nrow(input)
    n_exact   <- sum(out$exact_match, na.rm = TRUE)
    n_goodfz  <- sum(!out$exact_match & !is.na(out$distance) & out$poor_match %in% FALSE, na.rm = TRUE)
    n_poor    <- sum(out$poor_match %in% TRUE, na.rm = TRUE)
    n_nomatch <- sum(is.na(out$matched_target))
    base::message("\nFuzzy Match Summary:")
    base::message("  - Exact matches: ", n_exact, "/", n_total)
    base::message("  - Fuzzy matches (good): ", n_goodfz, "/", n_total)
    if (n_poor > 0)    base::message("  - Poor matches (set to NA or flagged): ", n_poor)
    if (n_nomatch > 0) base::message("  - Unmatched total: ", n_nomatch)
    if (isTRUE(best_by_input_id) && !is.null(input_id_col)) {
      base::message("  - Deduplicated by input_id")
    }
    if (isTRUE(best_by_target_id) && !is.null(target_id_col)) {
      base::message("  - Flagged worse duplicates by target_id (kept all rows)")
    }
  }

  out
}


# fuzzy_match_df <- function(
#     input,
#     target,
#     input_col,
#     target_col,
#     input_id_col  = NULL,
#     target_id_col = NULL,
#     group_vars = NULL,
#     fuzzy = TRUE,
#     stopwords = NULL,
#     method = "jw",
#     p = 0.2,
#     threshold = 0.20,
#     normalize = c("none","length"),
#     tolower = TRUE,
#     verbose = TRUE,
#     best_by_input_id = FALSE,
#     best_by_target_id = FALSE
# ) {
#   if (!requireNamespace("stringdist", quietly = TRUE)) {
#     stop("Package 'stringdist' is required; please install it (Suggests).")
#   }
#   if (!requireNamespace("fastmatch", quietly = TRUE)) {
#     stop("Package 'fastmatch' is required; please install it (Suggests).")
#   }
#
#   input  <- base::as.data.frame(input,  stringsAsFactors = FALSE)
#   target <- base::as.data.frame(target, stringsAsFactors = FALSE)
#   normalize <- base::match.arg(normalize)
#
#   if (!is.data.frame(input))  stop("'input' must be a data.frame.")
#   if (!is.data.frame(target)) stop("'target' must be a data.frame.")
#   if (!input_col %in% names(input))   stop("`input_col` not found in `input`.")
#   if (!target_col %in% names(target)) stop("`target_col` not found in `target`.")
#   if (!is.null(input_id_col)  && !input_id_col  %in% names(input))  stop("`input_id_col` not found in `input`.")
#   if (!is.null(target_id_col) && !target_id_col %in% names(target)) stop("`target_id_col` not found in `target`.")
#
#   if (is.null(group_vars)) group_vars <- character(0)
#   if (length(group_vars)) {
#     miss_in  <- setdiff(group_vars, names(input))
#     miss_tar <- setdiff(group_vars, names(target))
#     if (length(miss_in))  stop("Group vars missing in `input`: ", paste(miss_in, collapse = ", "))
#     if (length(miss_tar)) stop("Group vars missing in `target`: ", paste(miss_tar, collapse = ", "))
#   }
#
#   norm_text <- function(x) if (tolower) base::tolower(x) else x
#   strip_tokens <- function(vec, sw) {
#     if (is.null(sw)) return(vec)
#     base::vapply(
#       vec,
#       function(s) base::paste(base::setdiff(base::strsplit(s, "\\s+")[[1]], sw), collapse = " "),
#       FUN.VALUE = character(1)
#     )
#   }
#
#   input_key <- if (length(group_vars)) {
#     base::interaction(input[, group_vars, drop = FALSE], drop = TRUE, lex.order = TRUE)
#   } else factor(rep(1L, nrow(input)))
#   target_key <- if (length(group_vars)) {
#     base::interaction(target[, group_vars, drop = FALSE], drop = TRUE, lex.order = TRUE)
#   } else factor(rep(1L, nrow(target)))
#
#   out_input          <- input[[input_col]]
#   out_input_id       <- if (is.null(input_id_col)) base::rep(NA_character_, nrow(input)) else input[[input_id_col]]
#   out_matched_target <- base::rep(NA_character_, nrow(input))
#   out_target_id      <- base::rep(NA_character_, nrow(input))
#   out_exact          <- base::rep(FALSE,        nrow(input))
#   out_distance_raw   <- base::rep(NA_real_,     nrow(input))
#   out_poor           <- base::rep(NA,           nrow(input))
#
#   input_split_idx  <- split(seq_len(nrow(input)),  input_key, drop = TRUE)
#   target_split_idx <- split(seq_len(nrow(target)), target_key, drop = TRUE)
#
#   for (g in names(input_split_idx)) {
#     ii <- input_split_idx[[g]]
#     tt <- target_split_idx[[g]]
#     if (is.null(tt) || length(tt) == 0L) next
#
#     in_vec_raw  <- input[[input_col]][ii]
#     tar_vec_raw <- target[[target_col]][tt]
#     id_vec_raw  <- if (is.null(target_id_col)) rep(NA_character_, length(tt)) else target[[target_id_col]][tt]
#
#     in_norm  <- norm_text(in_vec_raw)
#     tar_norm <- norm_text(tar_vec_raw)
#
#     exact_idx <- fastmatch::fmatch(in_norm, tar_norm, nomatch = NA_integer_)
#     out_exact[ii] <- !is.na(exact_idx)
#
#     match_idx <- exact_idx
#     dist_vec  <- base::rep(NA_real_, length(ii))
#
#     need_fuzzy <- is.na(exact_idx)
#     if (any(need_fuzzy) && isTRUE(fuzzy)) {
#       in_need   <- in_norm[need_fuzzy]
#       tar_cands <- tar_norm
#       in_need2   <- strip_tokens(in_need,   stopwords)
#       tar_cands2 <- strip_tokens(tar_cands, stopwords)
#
#       dm <- stringdist::stringdistmatrix(in_need2, tar_cands2, method = method, p = p)
#       min_idx <- base::apply(dm, 1L, base::which.min)
#       min_dst <- base::apply(dm, 1L, base::min)
#
#       match_idx[need_fuzzy] <- min_idx
#       dist_vec[need_fuzzy]  <- min_dst
#     }
#
#     dist_vec[!is.na(exact_idx)] <- 0
#
#     have_match <- !is.na(match_idx)
#     if (any(have_match)) {
#       out_matched_target[ii[have_match]] <- tar_vec_raw[match_idx[have_match]]
#       if (!is.null(target_id_col)) out_target_id[ii[have_match]] <- id_vec_raw[match_idx[have_match]]
#       out_distance_raw[ii] <- dist_vec
#     }
#
#     if (normalize == "length") {
#       denom <- base::rep(1, length(ii))
#       if (any(have_match)) {
#         mt <- out_matched_target[ii]
#         denom <- base::ifelse(!is.na(mt), base::pmin(base::sqrt(base::nchar(mt)), 5), 1)
#         denom[denom == 0 | base::is.na(denom)] <- 1
#       }
#       out_distance_raw[ii] <- dist_vec / denom
#     } else {
#       out_distance_raw[ii] <- dist_vec
#     }
#
#     pmask <- !is.na(out_distance_raw[ii]) & have_match
#     poor  <- base::rep(NA, length(ii))
#     poor[pmask] <- out_distance_raw[ii][pmask] > threshold
#
#     if (any(poor %in% TRUE)) {
#       idx_bad <- ii[which(poor %in% TRUE)]
#       out_matched_target[idx_bad] <- NA_character_
#       if (!is.null(target_id_col)) out_target_id[idx_bad] <- NA_character_
#     }
#
#     out_poor[ii] <- poor
#   }
#
#   out <- base::data.frame(
#     input_id       = out_input_id,
#     input          = out_input,
#     matched_target = out_matched_target,
#     target_id      = if (is.null(target_id_col)) base::rep(NA_character_, nrow(input)) else out_target_id,
#     exact_match    = out_exact,
#     distance       = out_distance_raw,
#     poor_match     = out_poor,
#     stringsAsFactors = FALSE
#   )
#
#   if (length(group_vars)) {
#     out <- base::cbind(input[, group_vars, drop = FALSE], out, stringsAsFactors = FALSE)
#   }
#
#   # Deduplicate by input_id (keep best)
#   dedup_best <- function(dt, id_var) {
#     if (!id_var %in% names(dt)) return(dt)
#     if (all(is.na(dt[[id_var]]))) return(dt)
#
#     dt <- data.table::as.data.table(dt)
#     dt[, .dist := fifelse(is.na(distance), Inf, distance)]
#     dt[, .exact := fifelse(exact_match, 0L, 1L)]
#
#     # arrange by id, then exactness, then distance
#     data.table::setorder(dt, ..id_var, .exact, .dist)
#
#     # keep first per id_var
#     dt <- dt[, .SD[1L], by = ..id_var]
#
#     dt[, c(".dist", ".exact") := NULL]
#     data.table::setDF(dt) # return as data.frame for compatibility
#     dt
#   }
#
#   # best_by_target_id -> flag worse duplicates instead of dropping
#   if (isTRUE(best_by_target_id) && !is.null(target_id_col) && "target_id" %in% names(out)) {
#     dt <- data.table::as.data.table(out)
#
#     dt[, .adj_dist := fifelse(is.na(distance), Inf, distance)]
#
#     dt[, .best_dist := suppressWarnings(min(.adj_dist, na.rm = TRUE)),
#        by = target_id]
#
#     dt[, .flag_worse := !is.na(target_id) & (.adj_dist > .best_dist)]
#
#     dt[, poor_match := fifelse(.flag_worse, TRUE, poor_match)]
#
#     dt[, c(".adj_dist", ".best_dist", ".flag_worse") := NULL]
#
#     out <- data.table::setDF(dt)  # back to data.frame
#   }
#
#   if (isTRUE(verbose)) {
#     n_total   <- nrow(input)
#     n_exact   <- sum(out$exact_match, na.rm = TRUE)
#     n_goodfz  <- sum(!out$exact_match & !is.na(out$distance) & out$poor_match %in% FALSE, na.rm = TRUE)
#     n_poor    <- sum(out$poor_match %in% TRUE, na.rm = TRUE)
#     n_nomatch <- sum(is.na(out$matched_target))
#     base::message("\nFuzzy Match Summary:")
#     base::message("  - Exact matches: ", n_exact, "/", n_total)
#     base::message("  - Fuzzy matches (good): ", n_goodfz, "/", n_total)
#     if (n_poor > 0)    base::message("  - Poor matches (set to NA or flagged): ", n_poor)
#     if (n_nomatch > 0) base::message("  - Unmatched total: ", n_nomatch)
#     if (isTRUE(best_by_input_id) && !is.null(input_id_col)) {
#       base::message("  - Deduplicated by input_id")
#     }
#     if (isTRUE(best_by_target_id) && !is.null(target_id_col)) {
#       base::message("  - Flagged worse duplicates by target_id (kept all rows)")
#     }
#   }
#
#   out
# }



#' Filter and merge fuzzy matches with NER output
#'
#' @description
#' Combines Named Entity Recognition (NER) results with fuzzy matching against a
#' target dictionary. Internally calls [fuzzy_match_df()] to perform exact and/or
#' fuzzy matching, then merges matches back into the NER results. If a
#' `target_id_col` is supplied, the output preserves this exact column name.
#'
#' @inheritParams fuzzy_match_df
#' @param input A data.frame or data.table containing NER output, typically from
#'   [gliner_extract()]. Must include a column matching `input_id_col`
#'   (default `"ent_id"`).
#' @param keep_na Logical. Keep rows where `fuzzy_match_df()` returned `NA`
#'   in column `input`? Default FALSE.
#'
#' @return A data.table with columns:
#'   \itemize{
#'     \item `<input_id_col>` – entity ID from `input` (default `ent_id`).
#'     \item `matched_target` – best-matched label from `target`.
#'     \item `<target_id_col>` – preserved if supplied.
#'     \item `match_distance` – string distance to the matched label.
#'     \item all columns from `input`.
#'   }
#'
#' @seealso [gliner_extract()], [fuzzy_match_df()]
#' @import data.table
#' @export
fuzzy_filter <- function(
    input,
    target,
    input_col = "entity_name",
    input_id_col  = "ent_id",
    target_col,
    target_id_col = NULL,
    group_vars = NULL,
    fuzzy = TRUE,
    stopwords = NULL,
    method = "jw",
    p = 0.2,
    threshold = 0.02,
    normalize = "none",
    tolower = TRUE,
    verbose = TRUE,
    best_by_input_id = TRUE,
    best_by_target_id = FALSE,
    keep_na = FALSE
) {
  # Ensure data.table
  input <- data.table::as.data.table(input)
  target <- data.table::as.data.table(target)
  input <- data.table::copy(input)
  target <- data.table::copy(target)

  # Argument checks
  if (!input_id_col %in% names(input)) {
    stop("`input` must contain column '", input_id_col, "'.")
  }
  if (!input_col %in% names(input)) {
    stop("`input_col` not found in `input`.")
  }
  if (!target_col %in% names(target)) {
    stop("`target_col` not found in `target`.")
  }
  if (!is.null(target_id_col) && !target_id_col %in% names(target)) {
    stop("`target_id_col` not found in `target`.")
  }

  # Fuzzy-match
  f_match_res <- fuzzy_match_df(
    input = input,
    input_col = input_col,
    input_id_col = input_id_col,
    target = target,
    target_col = target_col,
    target_id_col = target_id_col,
    group_vars = group_vars,
    stopwords = stopwords,
    threshold = threshold,
    tolower = tolower,
    normalize = normalize,
    best_by_input_id = best_by_input_id,
    best_by_target_id = best_by_target_id,
    verbose = verbose
  ) |> data.table::as.data.table()

  # Drop poor matches
  if (!keep_na) {
    f_match_res <- f_match_res[poor_match == FALSE]
  } else {
    f_match_res <- f_match_res[poor_match == FALSE | is.na(input)]
  }

  # Select only needed columns depending on target_id_col
  if (is.null(target_id_col)) {
    f_match_res_sub <- f_match_res[, .(
      ..input_id_col = input_id,
      matched_target,
      match_distance = distance
    )]
    data.table::setnames(f_match_res_sub, "..input_id_col", input_id_col)
  } else {
    f_match_res_sub <- f_match_res[, .(
      ..input_id_col = input_id,
      matched_target,
      match_distance = distance,
      target_id = .SD[["target_id"]]
    )]
    data.table::setnames(f_match_res_sub, c("..input_id_col", "target_id"),
                         c(input_id_col, target_id_col))
  }

  # Perform merge (always subset input to matched IDs)
  out <- merge(
    input[get(input_id_col) %in% f_match_res$input_id],
    f_match_res_sub,
    by = input_id_col,
    all.x = TRUE
  )

  return(out[])
}

