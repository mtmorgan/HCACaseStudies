.is_null_or_colname <- function(.data, x)
    is.null(x) || (.is_scalar_character(x) && x %in% colnames(.data))
