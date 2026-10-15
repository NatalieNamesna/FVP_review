#' @title Build the grob of a legend made of coloured words
#' @description
#' Builds a grid grob in which each category name is written in bold in
#' its own colour (e.g. "**TRUE** **FALSE** **Not reported**"). Used by
#' `plot_colour_legend()`, and directly where a grob is needed (e.g. as
#' the header of `build_anchored_inset()`).
#' @param palette
#' Named character vector of colours. Names are the words shown, in
#' this order.
#' @param title
#' Character or `NULL` (default). Bold black text placed before the
#' words, e.g. `"Regions:"`.
#' @param direction
#' Character. `"horizontal"` (default) puts all words on one line,
#' `"vertical"` puts each word on its own left-aligned line.
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
#' @param vjust
#' Numeric between 0 and 1, or `NULL` (default). Vertical position of
#' the legend within its area (0 = bottom, 1 = top). `NULL` centres
#' horizontal legends and puts vertical legends at the top.
#' @return
#' A grid grob.
#' @export
build_colour_legend_grob <- function(
    palette,
    title = NULL,
    direction = c("horizontal", "vertical"),
    font_size = font_size_base,
    flag_inset_box = FALSE,
    hjust = NULL,
    vjust = NULL) {
  direction <- base::match.arg(direction)

  assertthat::assert_that(
    base::is.character(palette),
    !base::is.null(base::names(palette)),
    msg = "`palette` must be a named character vector of colours."
  )
  assertthat::assert_that(
    base::is.null(title) || assertthat::is.string(title),
    assertthat::is.number(font_size),
    assertthat::is.flag(flag_inset_box),
    msg = "Invalid `title`, `font_size` or `flag_inset_box`."
  )
  assertthat::assert_that(
    base::is.null(hjust) ||
      (assertthat::is.number(hjust) && hjust >= 0 && hjust <= 1),
    base::is.null(vjust) ||
      (assertthat::is.number(vjust) && vjust >= 0 && vjust <= 1),
    msg = "`hjust` and `vjust` must be NULL or numbers between 0 and 1."
  )

  vec_words <-
    stringr::str_glue(
      "<span style='color:{palette}'>**{base::names(palette)}**</span>"
    )

  # gridtext does not know `&nbsp;`, so non-breaking spaces are used.
  separator <-
    base::ifelse(
      direction == "horizontal",
      stringr::str_c(" ", base::strrep(base::intToUtf8(160), 3), " "),
      "<br>"
    )

  text_legend <-
    c(
      if (
        !base::is.null(title)
      ) {
        stringr::str_glue("**{title}**")
      },
      vec_words
    ) |>
    stringr::str_c(collapse = separator)

  position_x <-
    hjust %||% base::ifelse(direction == "horizontal", 0.5, 0)

  position_y <-
    vjust %||% base::ifelse(direction == "horizontal", 0.5, 1)

  res_grob <-
    gridtext::richtext_grob(
      text = text_legend,
      x = grid::unit(position_x, "npc"),
      y = grid::unit(position_y, "npc"),
      hjust = position_x,
      vjust = position_y,
      halign = 0,
      gp = grid::gpar(fontsize = font_size, lineheight = 1.1),
      box_gp = grid::gpar(
        fill = base::ifelse(flag_inset_box, "white", NA),
        col = base::ifelse(flag_inset_box, colour_inset_outline, NA),
        lty = "dashed"
      ),
      padding = grid::unit(c(1, 1.5, 1, 1.5), "mm"),
      r = grid::unit(0, "mm")
    )

  return(res_grob)
}
