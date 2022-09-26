#' @rdname geo
#'
#' @title Summarize GEO Files
#'
#' @description `geo_gsm_cell_count()` parses a GEO GSM file for the
#'     number of cells present in the file.
#'
#' @param .data a `files_tbl_hca` object (e.g., created from
#'     `hca::files()`) containing `fileId` and `local.filePath`
#'     columns. All rows in `.data` must refer to GSM files.
#'
#' @param sep character(1) single character used to separate fields in
#'     the GSM file. Typically tab (`'\\t"`, default) or comma `","`.
#'
#' @details `geo_gsm_cell_count()` parses the first line of the GSM
#'     file, assuming that the line is a 'header' with gene as first
#'     column and cell labels as subsequent columns. The number of
#'     cells is the number of columns in the file minus 1.
#'
#' @return `geo_gsm_cell_count() returna `.data` augmented by a column
#'     `local.cellCount` reporting the number of cells in the file.
#'
#' @importFrom dplyr pull tibble left_join
#'
#' @export
geo_gsm_cell_count <-
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

#' @rdname geo
#'
#' @description `geo_gsm_gene_count()` parses a GEO GSM file for the
#'     number of genes present in the file.
#'
#' @details `geo_gsm_gene_count()` counts the number of lines in the
#'     file, and reports the number of genes as this number minus one,
#'     assuming the first line is a 'header' line.
#'
#' @return `geo_gsm_gene_count()` returns `.data` augmented by a
#'     column of gene counts, `"local.geneCount"`.
#'
#' @export
geo_gsm_gene_count <-
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

#' @rdname geo
#'
#' @description `geo_gsm_count_matrix()` returns a sparse matrix of
#'     gene x cell counts.
#'
#' @param reader_function a function to read each GSM file. The
#'     function should come from the readr package. The default
#'     `read_tsv` is appropriate for tab-delimited CSV files; a common
#'     alternative is `read_csv` for comma-separated GSM files.
#'
#' @details `geo_gsm_count_matrix()` parses each GEO GSM file in turn,
#'     assuming that the file contains a dense matrix. The dense
#'     matrix is converted to a sparse representation. As each file is
#'     processed, new genes are appended to the original `i` index,
#'     and cells are appended to the `j` index. The final result is
#'     the concatenation of all sparse matrix
#'     representations. Progress reports the amount of memory
#'     currently consumed by the cummulating object.
#'
#' @return `geo_gsm_count_matrix()` returns a sparse matrix with rows
#'     (genes) equal to the unique genes in all input files, and
#'     columns (cells) equal to the total cell count in the GSM
#'     files. Samples are in the order present in `.data`.
#'
#' @importFrom utils object.size
#'
#' @importFrom methods as
#'
#' @importFrom Matrix sparseMatrix
#'
#' @export
geo_gsm_count_matrix <-
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
    Sys.setenv(VROOM_CONNECTION_SIZE = 8 * 131072L) # determined emprically


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

count_lines_in_gzfile <-
    function(fl)
{
    if (identical(.Platform$OS.type, "unix")) {
        value <- system2("gunzip", c("-c", fl, "| wc -l"), stdout = TRUE)
        count <- as.integer(value)
    } else {
        count <- 0L
        newline <- charToRaw("\n")
        con <- gzfile(fl, open = "rb")
        on.exit(close(con))
        repeat {
            b <- readBin(con, raw(), n = 1000000L)
            if (length(b) == 0L)
                break
            count <- count + sum(b == newline)
        }
    }
    count
}
