#' @rdname use_template
#'
#' @title Templates For New Case Studies
#'
#' @description `use_template()` creates a new _R_ markdown file as a
#'     starting point for a case study. The file requires manual
#'     editing to customize for the specific project.
#'
#' @inheritParams hca::project_information
#'
#' @param case_study_path character(1) path to the directory in which
#'     the case study will be created.
#'
#' @param overwrite logical(1) overwrite a case study created by this
#'     function with the same `project_id` and `case_study_path`?
#'     Default: `FALSE`.
#'
#' @details `use_template()` queries the HCA data portal for the
#'     project title associated with `project_id`, and uses this for
#'     the title and vignette index entry of the rendered markdown
#'     doeucment. The `project_id` is injected into the document, so
#'     that references to `project_id` are defined. Rendering the
#'     template also injects the current date, package name, and
#'     package version.
#'
#' @return `use_template()` returns the path to the rendered template
#'     (in `case_study_path`) with approriate substitution of
#'     variables.
#'
#' @examples
#' project_id <- "3c9d586e-bd26-4b46-8690-3faaa18ccf38"
#' case_study_path <- tempfile(); dir.create(case_study_path)
#' rmd <- use_template(project_id, case_study_path)
#' readLines(rmd) |>
#'     head() |>
#'     noquote()
#'
#' @importFrom whisker whisker.render
#'
#' @importFrom hca project_title
#'
#' @export
use_template <-
    function(project_id, case_study_path, overwrite = FALSE)
{
    stopifnot(
        .is_scalar_character(project_id),
        .is_scalar_character(case_study_path),
        dir.exists(case_study_path),
        .is_scalar_logical(overwrite)
    )

    rendered_path <- file.path(
        case_study_path,
        paste0("hca-", project_id, ".Rmd")
    )
    if (!overwrite && file.exists(rendered_path)) {
        stop(
            "path to rendered file exists and 'overwrite = FALSE'\n",
            "    project_id: '", project_id, "'\n",
            "    case_study_path: '", case_study_path, "'\n",
            "    rendered file path: '", rendered_path, "'"
        )
    }

    project_title <- project_title(project_id)
    if (identical(length(project_title), 0L))
        stop(
            "'project_title(project_id)' is length 0, is 'project_id' correct?",
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
