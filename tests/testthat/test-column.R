test_that("column selection works", {
    example_tbl <- dplyr::tibble(
        x = letters,
        y = c(NA, tail(letters, -1)),
        z = rep(NA, length(letters)),
        w = "A"
    )

    observed_names <- 
        example_tbl |> 
        dplyr::select(where(column_is_not_all_NA)) |>
        names()
    expect_identical(observed_names, c("x", "y", "w"))

    observed_names <- 
        example_tbl |> 
        dplyr::select(where(column_is_informative)) |>
        names()
    expect_identical(observed_names, c("x", "y"))

    observed_names <- 
        example_tbl |> 
        dplyr::select(where(column_is_experimentwide)) |>
        names()
    expect_identical(observed_names, "w")
})
