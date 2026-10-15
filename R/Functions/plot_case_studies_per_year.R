#' @title Plot the number of case studies published per year
#' @description
#' Horizontal stacked bar chart of the number of case studies per
#' publication year, split by a grouping column (e.g. region).
#' @param data_counts
#' A data frame with columns `year`, `n` and the column named in
#' `col_fill`.
#' @param col_fill
#' Character. Name of the grouping column used for the bar colours.
#' @param palette
#' Named character vector of colours. Names must cover all values of
#' `col_fill`; their order sets the stacking and legend order.
#' @return
#' A ggplot object.
#' @export
plot_case_studies_per_year <- function(
    data_counts,
    col_fill,
    palette) {
  assertthat::assert_that(
    base::is.data.frame(data_counts),
    assertthat::is.string(col_fill),
    base::all(
      c("year", "n", col_fill) %in% base::colnames(data_counts)
    ),
    msg = "`data_counts` must have columns `year`, `n` and `col_fill`."
  )
  assertthat::assert_that(
    base::all(
      base::unique(data_counts[[col_fill]]) %in% base::names(palette)
    ),
    msg = "`palette` must name a colour for every value of `col_fill`."
  )

  data_plot <-
    data_counts |>
    dplyr::mutate(
      year = base::factor(year),
      fill_group = base::factor(
        .data[[col_fill]],
        levels = base::rev(base::names(palette))
      )
    )

  n_max_per_year <-
    data_plot |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      n_total = base::sum(n),
      .groups = "drop"
    ) |>
    dplyr::pull(n_total) |>
    base::max()

  res_plot <-
    ggplot2::ggplot(
      data = data_plot,
      mapping = ggplot2::aes(
        x = n,
        y = year,
        fill = fill_group
      )
    ) +
    ggplot2::scale_x_continuous(
      breaks = base::seq(0, n_max_per_year, by = 1),
      expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    ggplot2::scale_fill_manual(
      values = palette,
      breaks = base::names(palette)
    ) +
    ggplot2::labs(
      x = "Number of case studies",
      y = NULL
    ) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = "bottom"
    ) +
    ggplot2::geom_col(
      width = 0.8
    )

  return(res_plot)
}
