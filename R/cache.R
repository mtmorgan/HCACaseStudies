cache_object_id <-
    function(project_id, object_name)
{
    paste(project_id, object_name, sep = "-")
}

#' @rdname cache
#'
#' @title Cache of Computationally Expensive Objects for Reuse
#'
#' @description `cache()` creates and / or returns a 'BiocFileCache'
#'     object containing information about objects in the
#'     package-specific cache.
#'
#' @param create logical(1) create the cache if it does not exist?
#'     Default: `TRUE`.
#'
#' @return `cache()` returns a `BiocFileCache` object that can be
#'     manipulated using the interface defined in the BiocFileCache
#'     package.
#'
#' @examples
#' cache()
#'
#' @importFrom tools R_user_dir
#'
#' @importFrom hca .is_scalar_logical .is_scalar_character
#'
#' @importFrom BiocFileCache BiocFileCache
#'
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

#' @rdname cache
#'
#' @description `cache_exists()` tests whether an object exists in the
#'     cache. The object is identified by the concatentation of the
#'     `project_id` and `object_name` argument.
#'
#' @inheritParams project_information
#'
#' @param object_name character(1) arbitrary object identifier. Cached
#'     objects created in Case Study vignettes are prefixed
#'     `"local."`, e.g., `"local.countMatrix"`.
#'
#' @return `cache_exists()` returns a logical(1) value, TRUE when the
#'     object identified by `projectId` and `object_name` exists in
#'     the cache.
#'
#' @examples
#' faux_project_id <- "faux-project-id"
#' faux_object_name <- "local.fauxObject"
#'
#' cache_exists(faux_project_id, faux_object_name)
#'
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

#' @rdname cache
#'
#' @description `cache_add()` adds `object` to the cache, using
#'     `project_id` and `object_name` to create an identifier. The
#'     `writer_function` is used to save the object to the cache.
#'
#' @param object Any _R_ object.
#'
#' @param writer_function a function taking as it's first argument
#'     `object`, a second argument the cache file path (determined
#'     internally) to store the object, and any additional arguments
#'     (`...` in `cache_add()`) required to write the object.
#'
#' @param ... in `cache_add()`, `...` are arguments passed to
#'     `writer_function()`.
#'
#' @param overwrite logical(1) overwrite an existing object in the
#'     cache? With the default `FALSE`, attempts to write the object
#'     result in an error.
#'
#' @return `cache_add()` returns, invisibly, the cache file path where
#'     the object is stored.
#'
#' @examples
#' cache_exists(faux_project_id, faux_object_name)       # FALSE
#' cache_add(letters, faux_project_id, faux_object_name)
#' cache_exists(faux_project_id, faux_object_name)       # TRUE
#'
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

#' @rdname cache
#'
#' @description `cache_read()` retrieves an object from the cache,
#'     using `project_id` and `object_name` to create an
#'     identifier. The `reader_function` is used to read the object
#'     into _R_.
#'
#' @param reader_function a function taking as it's first argument the
#'     cache file path (determined internally) of the object, and any
#'     additional arguments (`...` in `cache_add()`) required to write
#'     the object.
#'
#' @param ... in `cache_read()`, `...` are arguments passed to
#'     'reader_function()`.
#'
#' @return `cache_read()` returns the value returned by
#'     `reader_function()`.
#'
#' @examples
#' cache_read(faux_project_id, faux_object_name)
#'
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

#' @rdname cache
#'
#' @description `cache_remove()` removes an object from the cache.
#'
#' @return `cache_remove()` warns if the object is not in the cache,
#'     and returns `FALSE` invisibly, or removes the object and
#'     returns `TRUE` invisibly.
#'
#' @examples
#' cache_remove(faux_project_id, faux_object_name)
#' cache_exists(faux_project_id, faux_object_name)       # FALSE
#'
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
