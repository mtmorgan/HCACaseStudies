#' @importFrom dplyr pull tibble left_join
#'
#' @export
geo_cell_count <-
    function(.data, sep = "\\t")
{
    stopifnot(
        inherits(.data, "files_tbl_hca"),
        "fileId" %in% colnames(.data),
        "local.filePath" %in% colnames(.data)
    )

    file_id <- pull(.data, "fileId")
    cell_count <-
        pull(.data, "local.filePath") |>
        vapply(function(file) {
            first <- readLines(file, n = 1)
            length(strsplit(first, sep)[[1]])
        }, integer(1))

    tbl <- tibble(fileId = file_id, local.cellCount = unname(cell_count) - 1L)

    left_join(.data, tbl, by = "fileId")
}

#' @export
geo_gene_count <-
    function(.data)
{
    stopifnot(
        inherits(.data, "files_tbl_hca"),
        "fileId" %in% colnames(.data),
        "local.filePath" %in% colnames(.data)
    )

    file_id <- pull(.data, "fileId")
    gene_count <-
        pull(.data, "local.filePath") |>
        vapply(count_lines_in_gzfile, integer(1))

    tbl <- tibble(fileId = file_id, local.geneCount = unname(gene_count) - 1L)
    left_join(.data, tbl, by = "fileId")
}

#' @importFrom utils object.size
#'
#' @importFrom methods as
#'
#' @importFrom Matrix sparseMatrix
#'
#' @export
geo_count_matrix_gsms <-
    function(.data, reader_function = readr::read_tsv)
{
    stopifnot(
        inherits(.data, "files_tbl_hca"),
        "local.filePath" %in% colnames(.data)
    )

    old_vroom_connection_size <- Sys.getenv("VROOM_CONNECTION_SIZE")
    on.exit({
        if (nzchar(old_vroom_connection_size)) {
            Sys.setenv(VROOM_CONNECTION_SIZE = old_vroom_connection_size)
        } else {
            Sys.unsetenv("VROOM_CONNECTION_SIZE")
        }
    })
    Sys.setenv(VROOM_CONNECTION_SIZE = 8 * 131072L) # determined empricially


    file_paths <- pull(.data, "local.filePath")
    i <- j <- x <- integer()
    row_names <- character()
    col_names <- character()

    for (file in file_paths) {
        csv <- reader_function(file, show_col_types = FALSE)
        matrix <- as.matrix(csv[,-1])
        ## a fast way to get i, j?
        sparse_matrix <- as(matrix, "dgTMatrix")

        row_names <- union(row_names, csv[[1]])
        i_names_index <- match(csv[[1]], row_names)
        i <- c(i, i_names_index[sparse_matrix@i + 1L])

        col_offset <- length(col_names)
        col_names <- c(col_names, colnames(matrix))
        j <- c(j, col_offset + sparse_matrix@j + 1L)

        x <- c(x, sparse_matrix@x)
        message(
            format(object.size(list(i, j, x)), units = "auto"), " after ",
            basename(file)
        )
    }

    sparseMatrix(
        i, j, x = x,
        dims = c(length(row_names), length(col_names)),
        dimnames = list(row_names, col_names)
    )
}
