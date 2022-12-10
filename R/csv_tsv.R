count_lines_in_files <-
    function(.data, file_id = "fileId", file_path = "local.filePath")
{
    file_id <- pull(.data, file_id)
    pull(.data, file_path) |>
        vapply(count_lines_in_file, integer(1))
}

#' @rdname csv_tsv
#'
#' @title Summarize Tab- or Comma-Separated Files
#'
#' @description `csv_tsv_count_lines()` counts the number of lines
#'     in one or several comma- or tab-delimited files.
#'
#' @param .data `tibble` containing a column with `file_id` and
#'     `file_path` columns.
#'
#' @param file_id `character(1)` a column name in `.data` containing
#'     (unique) identifier for the files for which line counts are
#'     requested.
#'
#' @param file_path `character(1)` a column name in `.data` containing
#'     file paths to the file for which lines will be counted.
#'
#' @return `csv_tsv_count_lines()` returns the original tibble
#'     `.data` with an additional column `local.lines` containing the
#'     number of lines in `file_path`.
#'
#' @importFrom dplyr bind_cols
#'
#' @export
csv_tsv_count_lines <-
    function(.data, file_id = "fileId", file_path = "local.filePath")
{
    stopifnot(
        .is_scalar_character(file_id),
        .is_scalar_character(file_path),
        file_id %in% colnames(.data),
        file_path %in% colnames(.data)
    )

    lines <- count_lines_in_files(.data, file_id, file_path)

    bind_cols(.data, local.lines = lines)
}
