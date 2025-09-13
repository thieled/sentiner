#' @title Split and Clean Label Strings
#'
#' @description
#' Splits a label column into long format, applies trimming, removes duplicates,
#' and optionally disambiguates labels globally or within groups.
#'
#' @param dt A data.frame or data.table with at least an ID and label column.
#' @param id_col Character. Name of the ID column. Default is "id".
#' @param label_col Character. Name of the label column. Default is "label".
#' @param group_col Character or NULL. Optional group column name. Default is NULL.
#' @param pattern Character regex pattern to split labels. Default is "\\|".
#' @param keep_orig Logical. Keep the original labels as rows. Default is TRUE.
#' @param keep_first Logical. Keep only the first split label per row. Default is FALSE.
#' @param disambiguate Logical. Remove labels that appear in multiple IDs
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
