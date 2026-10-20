#' @title Plot a treemap of databases used by the case studies
#' @description
#' Draws a treemap with one box per database, sized by the number of
#' case studies and coloured by database group. Databases of one group
#' are laid out together and outlined by a thicker border, so the treemap
#' shows the group structure (e.g. Neotoma and its constituent
#' databases).
#' @param data_databases
#' A data frame with columns `database`, `group` and `n`.
#' @param palette
#' Named character vector of colours for each `group`.
#' @param title
#' Character or `NULL` (default). Plot title.
#' @param font_size
#' Numeric. Label size in pt. Defaults to `font_size_small` (config).
#' @param font_size_min
#' Numeric. Smallest label size in pt. Labels that do not fit at this
#' size are left out. Defaults to 6.
#' @param n_label_min
#' Integer. Only databases used by at least this many case studies are
#' labelled, so that all smaller boxes are consistently unlabelled
#' (rather than labelled only where the text happens to fit). Defaults
#' to 2.
#' @return
#' A ggplot object.
#' @export
plot_treemap <- function(
    data_databases,
    palette,
    title = NULL,
    font_size = font_size_small,
    font_size_min = 6,
    n_label_min = 2L) {
  assertthat::assert_that(
    base::is.data.frame(data_databases),
    base::all(
      c("database", "group", "n") %in% base::colnames(data_databases)
    ),
    msg = "`data_databases` must have columns `database`, `group`, `n`."
  )
  assertthat::assert_that(
    base::is.character(palette),
    base::all(
      base::unique(data_databases[["group"]]) %in% base::names(palette)
    ),
    msg = "`palette` must name a colour for every `group`."
  )
  assertthat::assert_that(
    assertthat::is.number(font_size),
    assertthat::is.number(font_size_min),
    assertthat::is.count(n_label_min),
    msg = "Invalid `font_size`, `font_size_min` or `n_label_min`."
  )

  # Non-breaking spaces keep "(n = x)" on one line when labels reflow.
  space_non_breaking <- base::intToUtf8(160)

  data_plot <-
    data_databases |>
    dplyr::mutate(
      label = dplyr::if_else(
        n >= n_label_min,
        stringr::str_glue(
          "{database}\n",
          "(n{space_non_breaking}={space_non_breaking}{n})"
        ),
        ""
      ),
      group = base::factor(group, levels = base::names(palette))
    )

  # Labels are white on dark boxes and black on light ones, based on the
  #    relative luminance of the fill colour.
  vec_luminance <-
    grDevices::col2rgb(palette) |>
    base::t() |>
    tibble::as_tibble() |>
    dplyr::mutate(
      luminance = (0.2126 * red + 0.7152 * green + 0.0722 * blue) / 255
    ) |>
    dplyr::pull(luminance) |>
    rlang::set_names(base::names(palette))

  data_plot_coloured <-
    data_plot |>
    dplyr::mutate(
      text_colour = dplyr::if_else(
        vec_luminance[base::as.character(group)] < 0.45,
        "white",
        "black"
      )
    )

  res_plot <-
    ggplot2::ggplot(
      data = data_plot_coloured,
      mapping = ggplot2::aes(
        area = n,
        fill = group,
        subgroup = group,
        label = label,
        colour = text_colour
      )
    ) +
    ggplot2::scale_fill_manual(
      values = palette
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::labs(
      title = title
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.title.position = "panel",
      legend.position = "none"
    ) +
    treemapify::geom_treemap(
      colour = "white"
    ) +
    # treemapify still calls the border width `size`.
    treemapify::geom_treemap_subgroup_border(
      colour = "white",
      size = 3
    ) +
    treemapify::geom_treemap_text(
      place = "centre",
      reflow = TRUE,
      size = font_size,
      min.size = font_size_min,
      padding.x = grid::unit(0.5, "mm"),
      padding.y = grid::unit(0.5, "mm")
    )

  return(res_plot)
}
