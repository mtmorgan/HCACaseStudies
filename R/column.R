#' @rdname column
#'
#' @title Tests on Column Content
#'
#' @description `column_is_not_all_NA()` tests whether all elements in
#'     column `x` are NA (undefined).
#'
#' @param x any _R_ vector for which `is.na()` is defined.
#'
#' @return `column_is_not_all_NA()` returns `TRUE` if all elements in
#'     `x` are not `NA`, or `FALSE` otherwise.
#'
#' @examples
#' example_tbl <- dplyr::tibble(
#'     x = letters,
#'     y = c(NA, tail(letters, -1)),
#'     z = rep(NA, length(letters)),
#'     w = "A"
#' )
#' example_tbl |>
#'     dplyr::select(where(column_is_not_all_NA))     # x, y, w
#'
#' @export
column_is_not_all_NA <-
    function(x)
{
    !all(is.na(x))
}

#' @rdname column
#'
#' @description `column_is_informative()` returns `TRUE` when the column
#'     `x` contains values that differ across rows, and hence are
#'     informative about the status of the row.
#'
#' @examples
#' example_tbl |>
#'     dplyr::select(where(column_is_informative))    # x, y
#'
#' @export
column_is_informative <-
    function(x)
{
    !all(is.na(x)) && length(unique(x)) > 1L
}

#' @rdname column
#'
#' @description `column_is_experimentwide()` returns `TRUE` when the
#'     column `x` is not all NA, and all values in the column are
#'     unique.
#'
#' @examples
#' example_tbl |>
#'     dplyr::select(where(column_is_experimentwide)) # w
#'
#' @export
column_is_experimentwide <-
    function(x)
{
    !all(is.na(x)) && length(unique(x)) == 1L
}
