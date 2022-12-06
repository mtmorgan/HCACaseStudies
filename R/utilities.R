.is_null_or_colname <- function(.data, x)
    is.null(x) || (.is_scalar_character(x) && x %in% colnames(.data))

.caseStudySummary <- 
    function()
{
    summary_tbl <- tibble('proj_id' = character(), 'title' = character(), 
        'vignette_link' = character(), 'notes' = character())
    rmds <- dir("vignettes", pattern = ".*Rmd", full.names = TRUE)
    rmd_info <- lapply(rmds[1:length(rmds)-1], function(rmd) {
        rmd_name <- basename(rmd)
        no_ext <- strsplit(rmd_name, "[.]")[[1]][1]
        project_id <- strsplit(no_ext, "hca-")[[1]][2]
        project_info <- project_information(project_id)
        project_title <- project_info$projectTitle

        vignette_link <- paste0("./", no_ext, ".html")

        rmd_lines <- readLines(rmd)
        sections <- grepl("^# ", rmd_lines)
        anomaly_lines <- rmd_lines[cumsum(sections) == 3]
        note <- paste(anomaly_lines[nzchar(anomaly_lines)][-1], collapse = " ")
        
        new_row <- c('proj_id' = project_id,
            'title' = project_title,
            'vignette_link' = vignette_link,
            'note' = note)
    })
    summary_tbl <- dplyr::bind_rows(rmd_info)
    summary_tbl
}
