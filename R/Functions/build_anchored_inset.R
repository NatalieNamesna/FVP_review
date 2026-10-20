#' @title Build an inset anchored to a corner of its area
#' @description
#' Wraps a fixed-aspect plot (e.g. a map or a waffle) so that, used with
#' `patchwork::inset_element()`, it is drawn as large as possible within
#' the inset area and pushed into one of its corners. By default patchwork
#' centres fixed-aspect plots, so their edges cannot be aligned with the
#' axes of the main plot. An optional header (e.g. a legend) can sit
#' directly on top of the inset box, centred over it.
#' @param plot
#' A ggplot object.
#' @param aspect_ratio
#' Numeric. Height divided by width of the inset box.
#' @param just
#' Character vector of length 2: horizontal (`"left"`, `"centre"`,
#' `"right"`) and vertical (`"bottom"`, `"centre"`, `"top"`) position of
#' the inset within its area. Defaults to `c("right", "bottom")`.
#' @param flag_inset_box
#' Logical. If `TRUE` (default), the inset area is drawn as a white box
#' with a dashed outline (`colour_inset_outline`, config). The box spans
#' the anchored area exactly, so its edges align with the main plot.
#' @param grob_header
#' A grid grob or `NULL` (default). Drawn in a row of height
#' `header_height` directly above the inset box, with the same width as
#' the box. Position it with its own `hjust`/`vjust` (e.g. a legend from
#' `build_colour_legend_grob()` with `vjust = 0` sits on the box).
#' @param header_height
#' A grid unit or `NULL` (default). Height of the header row. `NULL`
#' uses the height of `grob_header` itself, so the header sits exactly
#' on the inset box.
#' @return
#' A patchwork-wrapped grob to pass to `patchwork::inset_element()`.
#' @export
build_anchored_inset <- function(
    plot,
    aspect_ratio,
    just = c("right", "bottom"),
    flag_inset_box = TRUE,
    grob_header = NULL,
    header_height = NULL) {
  assertthat::assert_that(
    ggplot2::is_ggplot(plot),
    msg = "`plot` must be a ggplot object."
  )
  assertthat::assert_that(
    assertthat::is.number(aspect_ratio),
    aspect_ratio > 0,
    msg = "`aspect_ratio` must be a positive number."
  )
  assertthat::assert_that(
    base::is.character(just),
    base::length(just) == 2,
    msg = "`just` must be a character vector of length 2."
  )
  assertthat::assert_that(
    assertthat::is.flag(flag_inset_box),
    msg = "`flag_inset_box` must be TRUE or FALSE."
  )
  assertthat::assert_that(
    base::is.null(grob_header) || grid::is.grob(grob_header),
    base::is.null(header_height) || grid::is.unit(header_height),
    msg = "`grob_header` must be a grob or NULL; `header_height` a unit."
  )

  flag_header <- !base::is.null(grob_header)

  header_height_used <-
    if (
      flag_header
    ) {
      header_height %||% grid::grobHeight(grob_header)
    }

  # The inset box is a respected "null" cell, which keeps its aspect
  #    ratio; the optional header row has a fixed height. The layout
  #    justification pushes both into the requested corner.
  vec_heights <-
    if (
      flag_header
    ) {
      grid::unit.c(header_height_used, grid::unit(aspect_ratio, "null"))
    } else {
      grid::unit(aspect_ratio, "null")
    }

  index_row_box <- base::ifelse(flag_header, 2L, 1L)

  layout_anchored <-
    grid::grid.layout(
      nrow = base::length(vec_heights),
      ncol = 1,
      widths = grid::unit(1, "null"),
      heights = vec_heights,
      respect = TRUE,
      just = just
    )

  grob_box <-
    grid::rectGrob(
      gp = grid::gpar(
        fill = base::ifelse(flag_inset_box, "white", NA),
        col = base::ifelse(flag_inset_box, colour_inset_outline, NA),
        lty = "dashed"
      )
    )

  grob_box_content <-
    grid::grobTree(
      grob_box,
      ggplot2::ggplotGrob(plot),
      vp = grid::viewport(layout.pos.row = index_row_box, layout.pos.col = 1)
    )

  list_children <-
    if (
      flag_header
    ) {
      grid::gList(
        grid::grobTree(
          grob_header,
          vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1)
        ),
        grob_box_content
      )
    } else {
      grid::gList(grob_box_content)
    }

  grob_anchored <-
    grid::gTree(
      children = list_children,
      vp = grid::viewport(layout = layout_anchored)
    )

  res_inset <-
    patchwork::wrap_elements(
      full = grob_anchored
    )

  return(res_inset)
}
