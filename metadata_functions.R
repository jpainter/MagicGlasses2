# Metadata functions

# Cleans up strings for conversion to excel-
#   - removes bad characters
#   - truncates long strings

.invalid_regex <- "(?:[\\p{Cc}&&[^\\t\\n\\r]]|[\\x{007F}-\\x{009F}]|\\u00AD|\\u200B|\\u200C|\\u200D|\\u2060|\\uFEFF)"

clean_invalid_characters <- function(df) {
  DT <- as.data.table(df)  # shallow; modifies by reference if df is already a data.table
  char_cols <- names(DT)[vapply(DT, is.character, logical(1))]
  if (!length(char_cols)) return(DT)

  for (col in char_cols) {
    x <- DT[[col]]
    # only clean cells that actually contain invalids
    bad <- stri_detect_regex(x, .invalid_regex)
    if (any(bad, na.rm = TRUE)) {
      x_bad <- x[bad]
      x_bad <- stri_replace_all_regex(x_bad, .invalid_regex, "")
      # set back in place
      set(DT, i = which(bad), j = col, value = x_bad)
    }
  }
  invisible(DT)
}

truncate_df_chars <- function(df, limit = 500L, suffix = " …[TRUNC]") {
  DT <- as.data.table(df)  # shallow wrapper; in-place if already a data.table
  char_cols <- names(DT)[vapply(DT, is.character, logical(1))]
  if (!length(char_cols)) {
    message("No character columns found.")
    return(invisible(DT))
  }

  # normalize suffix
  if (is.null(suffix)) suffix <- ""
  suffix <- as.character(suffix[1L])
  suffix_len <- stri_length(suffix)
  keep <- max(0L, as.integer(limit) - suffix_len)  # chars to keep before adding suffix

  report <- vector("list", length(char_cols))

  for (i in seq_along(char_cols)) {
    col <- char_cols[i]
    x <- DT[[col]]

    n <- stri_length(x)
    over <- !is.na(n) & n > limit
    n_over <- sum(over)

    if (n_over) {
      # Truncate only those rows
      head_part <- if (keep > 0L) stri_sub(x[over], 1L, keep) else rep("", n_over)
      truncated <- if (suffix_len > 0L) paste0(head_part, suffix) else head_part
      # Ensure character vector (never NULL)
      truncated <- as.character(truncated)
      set(DT, i = which(over), j = col, value = truncated)
    }

    report[[i]] <- data.frame(
      column = col,
      cells_over_limit = n_over,
      stringsAsFactors = FALSE
    )
  }

  attr(DT, "truncate_report") <- do.call(rbind, report)
  invisible(DT)
}
