#' @rdname case_study
#'
#' @title Summarize Case Studies
#'
#' @description `case_study_summary()` visits each case study in the
#'     'vignettes' directory of the package, summarizing the study and
#'     outcome in a tibble.
#'
#' @param rmd_directory `character(1)` directory in which case study
#'     vignette source (`hca-.*\\.Rmd`) are located.
#'
#' @importFrom hca project_title
#'
#' @importFrom DT datatable formatStyle
#'
#' @export
case_study_summary <-
    function(rmd_directory = "vignettes")
{
    stopifnot(
        .is_scalar_character(rmd_directory),
        dir.exists(rmd_directory)
    )

    rmds <- dir(rmd_directory, pattern = "hca-.*Rmd", full.names = TRUE)
    project_id <- sub("hca-(.*)\\.Rmd", "\\1", basename(rmds))
    project_title <- vapply(project_id, project_title, character(1))
    vignette_link <- paste0("./hca-", project_id, ".html")
    notes <- lapply(rmds, function(rmd) {
        rmd_lines <- readLines(rmd)
        sections <- grepl("^# ", rmd_lines)
        anomaly_lines <- rmd_lines[cumsum(sections) == 3]
        paste(anomaly_lines[nzchar(anomaly_lines)][-1], collapse = " ")
    })

    tbl <- tibble(
        project_id,
        project_title = paste0(
            "<a href='", vignette_link, "'>", project_title, "</a>"
        ),
        notes = notes
    )
    tbl |>
        datatable(
            escape = FALSE,
            width = '100%', options = list(scrollX = TRUE)
        ) |>
        formatStyle(1:3, `vertical-align` = "top")
}
