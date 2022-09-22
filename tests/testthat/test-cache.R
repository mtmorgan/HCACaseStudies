test_that("cache works", {
    ## approximately unique...
    project_id <- basename(tempfile())
    object_name <- basename(tempfile())

    ## cache
    expect_s4_class(cache(), "BiocFileCache")
    expect_true(dir.exists(BiocFileCache::bfccache(cache())))
    count0 <- BiocFileCache::bfccount(cache())

    ## cache_exists
    expect_false(cache_exists(project_id, object_name))

    ## cache_add
    path0 <- cache_add(letters, project_id, object_name)
    expect_true(file.exists(path0))
    expect_true(cache_exists(project_id, object_name))
    expect_error(cache_add(letters, project_id, object_name))

    path1 <- cache_add(letters, project_id, object_name, overwrite = TRUE)
    expect_false(file.exists(path0))
    expect_true(file.exists(path1))

    ## cache_read
    expect_identical(cache_read(project_id, object_name), letters)
    expect_error(cache_read(project_id, basename(tempfile)))

    ## cache_remove
    expect_true(cache_remove(project_id, object_name))
    expect_false(file.exists(path1))
    expect_warning(cache_remove(project_id, object_name))

    ## all clean?
    expect_identical(BiocFileCache::bfccount(cache()), count0)
})
