#' @title Prepare TRUE/FALSE/Not reported counts for waffle plots
#' @description
#' Selects yes/no columns of the case-study table, harmonises their
#' values to `"TRUE"`, `"FALSE"` and `"Not reported"` and counts the
#' case studies in each category.
#' @param data_source
#' The case-study table (`case_studies_clean.csv`).
#' @param vec_columns
#' Named character vector. Names are the short variable names used in the
#' output, values are the column names in `data_source`.
#' @param vec_values_not_reported
#' Character vector of raw values that should be treated as
#' `"Not reported"` (in addition to `NA`). Defaults to none.
#' @return
#' A tibble with columns `variable` (factor, ordered as `vec_columns`),
#' `value` (character) and `count` (integer). Every variable has all three
#' values, with zero counts filled in.
#' @details
#' Raw values are squished before matching, so `"TRUE\r\n"` or `"\nTRUE"`
#' count as `"TRUE"` unless listed in `vec_values_not_reported`. The typo
#' `"FASLE"` counts as `"FALSE"`. Any other value aborts.
#' @export
prepare_waffle_data <- function(
    data_source,
    vec_columns,
    vec_values_not_reported = base::character(0)) {
  assertthat::assert_that(
    base::is.data.frame(data_source),
    msg = "`data_source` must be a data frame."
  )
  assertthat::assert_that(
    base::is.character(vec_columns),
    !base::is.null(base::names(vec_columns)),
    msg = "`vec_columns` must be a named character vector."
  )
  assertthat::assert_that(
    base::all(vec_columns %in% base::colnames(data_source)),
    msg = "All `vec_columns` must be columns of `data_source`."
  )
  assertthat::assert_that(
    base::is.character(vec_values_not_reported),
    msg = "`vec_values_not_reported` must be a character vector."
  )

  vec_value_levels <- c("TRUE", "FALSE", "Not reported")

  data_values <-
    data_source |>
    dplyr::select(dplyr::all_of(vec_columns)) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::everything(),
        .fns = base::as.character
      )
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "variable",
      values_to = "value_raw"
    ) |>
    dplyr::mutate(
      value_squished = stringr::str_squish(value_raw),
      value = dplyr::case_when(
        value_raw %in% vec_values_not_reported ~ "Not reported",
        base::is.na(value_raw) ~ "Not reported",
        value_squished == "TRUE" ~ "TRUE",
        value_squished %in% c("FALSE", "FASLE") ~ "FALSE",
        .default = NA_character_
      )
    )

  vec_values_unknown <-
    data_values |>
    dplyr::filter(base::is.na(value)) |>
    dplyr::pull(value_raw) |>
    base::unique()

  if (
    base::length(vec_values_unknown) > 0
  ) {
    cli::cli_abort(
      "Unexpected values in waffle columns: {.val {vec_values_unknown}}"
    )
  }

  res_waffle <-
    data_values |>
    dplyr::count(
      variable,
      value,
      name = "count"
    ) |>
    tidyr::complete(
      variable = base::names(vec_columns),
      value = vec_value_levels,
      fill = base::list(count = 0L)
    ) |>
    dplyr::mutate(
      variable = base::factor(
        variable,
        levels = base::names(vec_columns)
      )
    ) |>
    dplyr::arrange(variable, value)

  return(res_waffle)
}
