#' @title Plot a faceted waffle chart of case-study answers
#' @description
#' Draws one waffle (one dot per case study) per variable, coloured by
#' the answer (`"TRUE"`, `"FALSE"`, `"Not reported"`).
#' @param data_waffle
#' Output of `prepare_waffle_data()`: columns `variable`, `value` and
#' `count`.
#' @param vec_facet_labels
#' Named character vector. Names are the variables to plot (in the order
#' of the facets), values are the facet labels.
#' @param palette
#' Named character vector of colours for each value. Its order sets the
#' legend order.
#' @param n_col
#' Integer. Number of facet columns.
#' @param point_size
#' Numeric. Size of each dot (ggplot2 size units).
#' @param vec_fill_order
#' Character vector of values in the order in which dots are filled,
#' from the bottom-left dot upwards. Defaults to `names(palette)`.
#' @param title
#' Character or `NULL` (default). Section title, centred over the
#' waffles.
#' @param n_dots_per_row
#' Integer. Number of dots in each waffle row. Defaults to 10.
#' @param label_width
#' Integer. Width (in characters) at which facet labels are wrapped.
#' @param flag_show_legend
#' Logical. If `TRUE`, the ggplot colour legend is shown. Defaults to
#' `FALSE`, as the figures use `plot_colour_legend()` instead.
#' @return
#' A ggplot object.
#' @details
#' Dots are drawn with `geom_point()` on a precomputed grid rather than
#' with icon fonts, so the plot does not need `showtext`.
#' @export
plot_waffle <- function(
    data_waffle,
    vec_facet_labels,
    palette,
    n_col,
    point_size,
    vec_fill_order = base::names(palette),
    title = NULL,
    n_dots_per_row = 10L,
    label_width = 30L,
    flag_show_legend = FALSE) {
  assertthat::assert_that(
    base::is.data.frame(data_waffle),
    base::all(
      c("variable", "value", "count") %in% base::colnames(data_waffle)
    ),
    msg = "`data_waffle` must have columns `variable`, `value`, `count`."
  )
  assertthat::assert_that(
    base::is.character(vec_facet_labels),
    !base::is.null(base::names(vec_facet_labels)),
    msg = "`vec_facet_labels` must be a named character vector."
  )
  assertthat::assert_that(
    base::all(
      base::names(vec_facet_labels) %in% data_waffle[["variable"]]
    ),
    msg = "All names of `vec_facet_labels` must be in `data_waffle`."
  )
  assertthat::assert_that(
    base::is.character(palette),
    base::all(
      base::unique(data_waffle[["value"]]) %in% base::names(palette)
    ),
    msg = "`palette` must name a colour for every value."
  )
  assertthat::assert_that(
    base::setequal(vec_fill_order, base::names(palette)),
    msg = "`vec_fill_order` must contain the same values as `palette`."
  )
  assertthat::assert_that(
    assertthat::is.count(n_col),
    assertthat::is.count(n_dots_per_row),
    assertthat::is.number(point_size),
    assertthat::is.flag(flag_show_legend),
    msg = "Invalid `n_col`, `n_dots_per_row`, `point_size` or legend flag."
  )

  vec_labels_wrapped <-
    stringr::str_wrap(
      vec_facet_labels,
      width = label_width
    ) |>
    rlang::set_names(base::names(vec_facet_labels))

  data_dots <-
    data_waffle |>
    dplyr::mutate(
      variable = base::as.character(variable)
    ) |>
    dplyr::filter(variable %in% base::names(vec_facet_labels)) |>
    dplyr::mutate(
      value = base::factor(
        value,
        levels = vec_fill_order
      )
    ) |>
    dplyr::arrange(variable, value) |>
    tidyr::uncount(weights = count) |>
    dplyr::group_by(variable) |>
    dplyr::mutate(
      index_dot = dplyr::row_number() - 1L,
      x = index_dot %% n_dots_per_row + 1L,
      y = index_dot %/% n_dots_per_row + 1L
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      facet_label = base::factor(
        vec_labels_wrapped[variable],
        levels = vec_labels_wrapped
      )
    )

  n_rows_max <-
    data_dots |>
    dplyr::pull(y) |>
    base::max()

  res_plot <-
    ggplot2::ggplot(
      data = data_dots,
      mapping = ggplot2::aes(
        x = x,
        y = y,
        colour = value
      )
    ) +
    ggplot2::facet_wrap(
      facets = ggplot2::vars(facet_label),
      ncol = n_col
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0.5, n_dots_per_row + 0.5),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0.5, n_rows_max + 0.5),
      expand = c(0, 0)
    ) +
    ggplot2::scale_colour_manual(
      values = palette,
      limits = base::names(palette),
      drop = FALSE
    ) +
    ggplot2::coord_fixed() +
    ggplot2::labs(
      title = title
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(
        override.aes = base::list(size = 2.5)
      )
    ) +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(vjust = 0),
      strip.clip = "off",
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.title.position = "panel",
      legend.position = "bottom"
    ) +
    # Legend is switched off at the layer, so that a theme set later on
    #    the whole composite cannot bring it back.
    ggplot2::geom_point(
      size = point_size,
      show.legend = flag_show_legend
    )

  return(res_plot)
}
