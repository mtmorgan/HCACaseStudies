#' @export
column_is_not_all_NA <-
    function(x)
{
    !all(is.na(x))
}

#' @export
column_is_informative <-
    function(x)
{
    !any(is.na(x)) && length(unique(x)) > 1L
}

#' @export
column_is_experimentwide <-
    function(x)
{
    !any(is.na(x)) && length(unique(x)) == 1L
}

count_lines_in_gzfile <-
    function(fl)
{
    if (identical(.Platform$OS.type, "unix")) {
        value <- system2("gzcat", c(fl, "| wc -l"), stdout = TRUE)
        count <- as.integer(value)
    } else {
        count <- 0L
        newline <- charToRaw("\n")
        con <- gzfile(fl, open = "rb")
        on.exit(close(con))
        repeat {
            b <- readBin(con, raw(), n = 1000000L)
            if (length(b) == 0L)
                break
            count <- count + sum(b == newline)
        }
    }
    count
}
