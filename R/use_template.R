#' @importFrom whisker whisker.render
#'
#' @export
use_template <-
    function(project_id, vignette_directory_path, overwrite = FALSE)
{
    stopifnot(
        .is_scalar_character(project_id),
        .is_scalar_character(vignette_directory_path),
        dir.exists(vignette_directory_path),
        .is_scalar_logical(overwrite)
    )

    rendered_path <- file.path(
        vignette_directory_path,
        paste0("hca-", project_id, ".Rmd")
    )
    if (!overwrite && file.exists(rendered_path)) {
        stop(
            "path to rendered file exists and 'overwrite = FALSE'\n",
            "    project_id: '", project_id, "'\n",
            "    vignette_directory_path: '", vignette_directory_path, "'\n",
            "    rendered file path: '", rendered_path, "'"
        )
    }

    project_title <- project_title(project_id)
    if (identical(length(project_title), 0L))
        stop(
            "'project_title(project_id)' has length 0, is 'project_id' correct?",
            "\n    project_id: '", project_id, "'"
        )

    doc_date <- BiocStyle::doc_date()
    pkg_ver <- BiocStyle::pkg_ver("hcaCaseStudies")

    template_path <- system.file(
        package = "hcaCaseStudies", "template", "hca-project.Rmd"
    )
    template <- readLines(template_path)
    rendered <- whisker.render(template)

    writeLines(rendered, rendered_path)

    invisible(rendered_path)
}
