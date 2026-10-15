#' @title Prepare harmonised region names of case studies
#' @description
#' Cleans the free-text `region` column of the case-study table and
#' harmonises the region names to the six continents used in all figures.
#' @param data_source
#' A data frame with columns `id` and `region` (raw free text, multiple
#' regions separated by a comma).
#' @param method
#' Character. `"split"` (default) returns one row per case study and
#' region. `"collapse"` returns one row per case study, with
#' `"Multiple regions"` for case studies focusing on more than one region.
#' @return
#' A tibble with columns `id` and `region`.
#' @details
#' "Middle East" is merged into "Asia", "Latin America" into
#' "South America" and "Australia And Oceania" is renamed to "Oceania".
#' Missing regions are labelled "Not Available".
#' @export
prepare_region_names <- function(
    data_source,
    method = c("split", "collapse")) {
  method <- base::match.arg(method)

  assertthat::assert_that(
    base::is.data.frame(data_source),
    msg = "`data_source` must be a data frame."
  )
  assertthat::assert_that(
    base::all(c("id", "region") %in% base::colnames(data_source)),
    msg = "`data_source` must contain columns `id` and `region`."
  )

  data_regions_split <-
    data_source |>
    dplyr::select(id, region) |>
    dplyr::mutate(
      region = stringr::str_squish(region) |>
        stringr::str_to_title()
    ) |>
    tidyr::separate_longer_delim(
      cols = region,
      delim = ","
    ) |>
    dplyr::mutate(
      region = stringr::str_trim(region),
      region = dplyr::case_when(
        base::is.na(region) ~ "Not Available",
        region == "Middle East" ~ "Asia",
        region == "Latin America" ~ "South America",
        region == "Australia And Oceania" ~ "Oceania",
        .default = region
      )
    ) |>
    dplyr::distinct(id, region)

  if (
    method == "split"
  ) {
    res_regions <- data_regions_split
  } else {
    res_regions <-
      data_regions_split |>
      dplyr::group_by(id) |>
      dplyr::summarise(
        region = dplyr::if_else(
          dplyr::n() > 1,
          "Multiple regions",
          dplyr::first(region)
        ),
        .groups = "drop"
      )
  }

  return(res_regions)
}
