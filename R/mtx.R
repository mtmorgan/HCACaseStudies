#' @rdname mtx
#'
#' @title Summarize 'MatrixMart' files
#'
#' @description `mtx_count_matrix()` parses one or several MatrixMart
#'     files with identical dimensions into a single `dgCMatrix`
#'     object.
#'
#' @param .data `tibble` containing a column with the value of the
#'     `mtx_path` parameter and, if non-NULL, columns with the value
#'     of parameters `genes_path` and `barcodes_path`.
#'
#' @param mtx_path `character()` paths to MatrixMart files on disk.
#'
#' @param genes_path `character()` (optional) paths to files
#'     containing gene (row) labels (e.g., ENSEMBL gene
#'     identifiers). The row labels must be in the first column of the
#'     file; fields are separated by the parameter `sep`; there can be
#'     no column headers in the file.
#'
#' @param barcodes_path `character()` (optional) paths to files
#'     containing cell (column) labels (e.g., barcodes). The column
#'     labels must be in the first column of the file; fields are
#'     separated by the parameter `sep`; there can be no column
#'     headers in the file.
#'
#' @param sep `character(1)`, default `"\t"` (tab). Field separator in
#'     `genes_path` and `barcodes_path` files.
#'
#' @return `mtx_count_matrix()` returns a `dgCMatrix` sparse matrix
#'     with rows (genes) equal to the unique genes in all input files,
#'     and columns (cells) equal to the total cell count in the
#'     MatrixMart files. Samples (columns) are in the order present in
#'     `.data`.
#'
#' @importFrom utils head
#' @importFrom Matrix readMM
#'
#' @importMethodsFrom Matrix coerce
#'
#' @export
mtx_count_matrix <-
    function(.data, mtx_path,
             genes_path = NULL, barcodes_path = NULL, sep = "\t")
{
    stopifnot(
        .is_scalar_character(mtx_path),
        mtx_path %in% colnames(.data),
        .is_null_or_colname(.data, genes_path),
        .is_null_or_colname(.data, barcodes_path)
    )

    ## paths to matrix, genes, and barcodes
    mtx_paths <- pull(.data, mtx_path)
    if (!is.null(genes_path))
        genes_paths <- pull(.data, genes_path)
    if (!is.null(barcodes_path))
        barcodes_paths <- pull(.data, barcodes_path)

    ## build overall matrix by iterating through files
    sparse_matrix <- NULL
    for (idx in seq_along(mtx_paths)) {
        ## read one matrix file
        sparse_matrix1 <- as(readMM(mtx_paths[[idx]]), "dgCMatrix")

        ## add dimnames, if available
        dimnames <- list(NULL, NULL)
        if (!is.null(genes_path)) {
            lines <- strsplit(readLines(genes_paths[[idx]]), sep)
            dimnames[[1]] <- vapply(lines, head, character(1), 1L)
        }
        if (!is.null(barcodes_path)) {
            lines <- strsplit(readLines(barcodes_paths[[idx]]), sep)
            dimnames[[2]] <- vapply(lines, head, character(1), 1L)
        }
        dimnames(sparse_matrix1) <- dimnames

        ## accumulate sparse matrix
        if (is.null(sparse_matrix)) {
            sparse_matrix <- sparse_matrix1
        } else {
            sparse_matrix <- cbind(sparse_matrix, sparse_matrix1)
        }

        ## progress
        message(
            "(", idx, "/", length(mtx_paths), ") ",
            format(object.size(sparse_matrix), units = "auto"), " after ",
            basename(mtx_paths[[idx]])
        )
    }

    sparse_matrix
}
