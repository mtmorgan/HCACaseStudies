test_that("use_template() works", {
    vignette_directory_path <- tempfile()
    dir.create(vignette_directory_path)

    project_id <- "PROJECT_ID"
    ## 'PROJECT_ID' does not exist so template cannot be created
    expect_error(use_template(project_id, vignette_directory_path))

    project_id <- "3c9d586e-bd26-4b46-8690-3faaa18ccf38"
    project_title <- project_title(project_id)

    expected_rendered_path <-
        file.path(vignette_directory_path, paste0("hca-", project_id, ".Rmd"))
    rendered_path <- use_template(project_id, vignette_directory_path)

    expect_identical(rendered_path, expected_rendered_path)
    expect_error(use_template(project_id, vignette_directory_path))
    expect_identical(
        use_template(project_id, vignette_directory_path, overwrite = TRUE),
        expected_rendered_path
    )

    rendered_template <- readLines(expected_rendered_path)

    ## project_id injected into project_id <- ...
    expect_identical(sum(grepl(project_id, rendered_template)), 1L)
    ## project_title injected into Title: and VignetteIndexEntry
    expect_identical(sum(grepl(project_title, rendered_template)), 2L)
    ## other injections
    doc_date <- BiocStyle::doc_date()
    pkg_ver <- BiocStyle::pkg_ver("hcaCaseStudies")
    expect_identical(sum(grepl(doc_date, rendered_template)), 1L)
    expect_identical(sum(grepl(pkg_ver, rendered_template)), 1L)
})
