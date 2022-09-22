project_information_strwrap <-
    function(x)
{
    paste(strwrap(x, indent = 2, exdent = 2), collapse = "\n")
}

project_information_title_preclean <-
    function(title)
{
    sub("\\.$", "", title)
}

project_information_title_clean <-
    function(title)
{
    title <- project_information_title_preclean(title)
    project_information_strwrap(title)
}

project_information_contributors_clean <-
    function(contributors)
{        
    clean0 <- sub(",,", " ", contributors[[1]])
    project_information_strwrap(paste(clean0, collapse = ", "))
}

project_information_description_clean <-
    function(description)
{
    project_information_strwrap(description)
}


#' @importFrom hca filters projects
#'
#' @importFrom dplyr mutate
#'
#' @export
project_information <-
    function(project_id)
{
    stopifnot(
        .is_scalar_character(project_id)
    )

    filter <- filters(projectId = list(is = project_id))
    columns <- c(
        projectId = "hits[*].projects[*].projectId",
        projectTitle = "hits[*].projects[*].projectTitle",
        projectDescription = "hits[*].projects[*].projectDescription",
        contributors = "hits[*].projects[*].contributors[*].contactName",
        doi = "hits[*].projects[*].publications[*].doi",
        url = "hits[*].projects[*].publications[*].publicationUrl"
    )
               
    project <- projects(filter, columns = columns)
    class(project) <- c("project_information", class(project))
    project
}

#' @export
print.project_information <-
    function(x, ...)
{
    cat(
        "Title\n",
        project_information_title_clean(x$projectTitle), "\n",
        "Contributors (unknown order; any role)\n",
        project_information_contributors_clean(x$contributors), "\n",
        "Description\n",
        project_information_description_clean(x$projectDescription), "\n",
        "doi: ", x$doi, "\n",
        "url: ", x$url, "\n",
        sep = ""
    )
}

#' @export
project_title <-
    function(project_id)
{
    stopifnot(
        .is_scalar_character(project_id)
    )

    project_information(project_id) |>
        pull(projectTitle) |>
        project_information_title_preclean()
}
