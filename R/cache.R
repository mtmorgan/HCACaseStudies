cache_object_id <-
    function(project_id, object_name)
{
    paste(project_id, object_name, sep = "-")
}

#' @importFrom tools R_user_dir
#'
#' @importFrom hca .is_scalar_logical .is_scalar_character
#'
#' @importFrom BiocFileCache BiocFileCache
#' @export
cache <-
    function(create = TRUE)
{
    stopifnot(.is_scalar_logical(create))

    cache <- R_user_dir("hcaCaseStudies", "cache")
    if (create && !dir.exists(cache))
        dir.create(cache, recursive = TRUE)
    BiocFileCache(cache)
}

#' @importFrom BiocFileCache bfcquery
#'
#' @export
cache_exists <-
    function(project_id, object_name)
{
    stopifnot(
        .is_scalar_character(project_id),
        .is_scalar_character(object_name)
    )

    cache <- cache()
    object_id <- cache_object_id(project_id, object_name)
    !identical(NROW(bfcquery(cache, object_id, "rname")), 0L)
}

#' @importFrom BiocFileCache bfcnew
#'
#' @export
cache_add <-
    function(object, project_id, object_name, writer_function = saveRDS,
             ..., overwrite = FALSE)
{
    stopifnot(
        .is_scalar_logical(overwrite),
        .is_scalar_character(project_id),
        .is_scalar_character(object_name)
    )

    if (!overwrite && cache_exists(project_id, object_name)) {
        stop(
            "\n  object already exists and overwrite = FALSE",
            "\n    project_id: '", project_id, "'",
            "\n    object_name: '", object_name, "'"
        )
    } else if (cache_exists(project_id, object_name)) {
        force(object) # make sure we have an object to save...
        cache_remove(project_id, object_name)
    } else {
        force(object)
    }

    cache <- cache()
    object_id <- cache_object_id(project_id, object_name)
    file_path <- bfcnew(cache, object_id)
    writer_function(object, file_path, ...)

    invisible(unname(file_path))
}

#' @importFrom BiocFileCache bfcrpath
#'
#' @export
cache_read <-
    function(project_id, object_name, reader_function = readRDS, ...)
{
    stopifnot(
        .is_scalar_character(project_id),
        .is_scalar_character(object_name),
        cache_exists(project_id, object_name)
    )

    cache <- cache()
    object_id <- cache_object_id(project_id, object_name)
    path <- bfcrpath(cache, object_id)
    reader_function(path, ...)
}

#' @importFrom BiocFileCache bfcremove
#'
#' @export
cache_remove <-
    function(project_id, object_name)
{
    stopifnot(
        .is_scalar_character(project_id),
        .is_scalar_character(object_name)
    )

    if (!cache_exists(project_id, object_name)) {
        warning(
            "\n  object not in cache so cannot be removed",
            "\n    project_id: '", project_id, "'",
            "\n    object_name: '", object_name, "'"
        )
        return(invisible(FALSE))
    }

    cache <- cache()
    object_id <- cache_object_id(project_id, object_name)
    bfcid <- names(bfcrpath(cache, object_id))
    bfcremove(cache, bfcid)

    return(invisible(TRUE))
}
