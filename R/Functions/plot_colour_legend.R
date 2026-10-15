#' @title Plot a legend made of coloured words
#' @description
#' Builds a legend in which each category name is written in bold in its
#' own colour (e.g. "**TRUE** **FALSE** **Not reported**"), to be placed
#' as a row of a patchwork layout or inside a plot with
#' `patchwork::inset_element()`.
#' @param palette
#' Named character vector of colours. Names are the words shown, in
#' this order.
#' @param title
#' Character or `NULL` (default). Bold black text placed before the
#' words, e.g. `"Regions:"`.
#' @param direction
#' Character. `"horizontal"` (default) puts all words on one centred
#' line, `"vertical"` puts each word on its own left-aligned line.
#' @param font_size
#' Numeric. Font size in pt. Defaults to `font_size_base` (config).
#' @param flag_inset_box
#' Logical. If `TRUE`, the text is drawn in a white box with a dashed
#' outline, the style of all insets (`colour_inset_outline`, config).
#' Defaults to `FALSE`.
#' @param hjust
#' Numeric between 0 and 1, or `NULL` (default). Horizontal position of
#' the legend within its area (0 = left, 0.5 = centre, 1 = right); the
#' words stay left-aligned inside the legend. `NULL` centres horizontal
#' legends and puts vertical legends on the left.
#' @return
#' A patchwork-wrapped grob that can be used like a plot.
#' @seealso [build_colour_legend_grob()]
#' @export
plot_colour_legend <- function(
    palette,
    title = NULL,
    direction = c("horizontal", "vertical"),
    font_size = font_size_base,
    flag_inset_box = FALSE,
    hjust = NULL) {
  grob_legend <-
    build_colour_legend_grob(
      palette = palette,
      title = title,
      direction = direction,
      font_size = font_size,
      flag_inset_box = flag_inset_box,
      hjust = hjust
    )

  res_legend <-
    patchwork::wrap_elements(
      full = grob_legend
    )

  return(res_legend)
}
