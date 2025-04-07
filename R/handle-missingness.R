library(data.table)
library(mice)
library(furrr)

set.seed(910)

find_missing_data <- function(x) {
  data_type <- class(x)[1]

  na_lookup <- list(
    character = NA_character_,
    integer = NA_integer_,
    numeric = NA_real_,
    logical = NA,
    IDate = data.table::as.IDate(NA)
  )

  na_value <- na_lookup[[data_type]]

  is_missing <- switch(
    data_type,
    character = x == "" |
      is.na(x) |
      grepl("missing|not stated|declined", x, ignore.case = TRUE),
    IDate = is.na(x),
    is.na(x)
  )

  data.table::fifelse(is_missing, na_value, x)
}


dt <- data.table::fread("data/selected-data.csv")

dt <- dt[, lapply(.SD, find_missing_data), .SDcols = c(names(dt)[3:ncol(dt)])]

char_cols <- names(dt)[vapply(dt, is.character, logical(1))]

dt[, (char_cols) := lapply(.SD, factor), .SDcols = char_cols]

imp <- mice::futuremice(dt, m = 1, n.core = 1)
