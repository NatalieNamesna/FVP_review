#' @title Save a composite figure as PNG and PDF
#' @description
#' Saves a (patchwork) plot with `ggview::save_ggplot()` at the journal
#' width set in the config file.
#' @param plot
#' A ggplot or patchwork object.
#' @param file_name
#' Character. File name without extension, e.g. `"Figure_01"`.
#' @param height
#' Numeric. Figure height in `units`.
#' @param dir_output
#' Character. Output directory. Defaults to `dir_figures` from the config.
#' @param width
#' Numeric. Figure width. Defaults to `figure_width` from the config.
#' @param units
#' Character. Units of `width` and `height`. Defaults to `figure_units`.
#' @param dpi
#' Numeric. Resolution of the PNG. Defaults to `figure_dpi`.
#' @param vec_formats
#' Character vector of file extensions to save. Defaults to PNG and PDF.
#' @param verbose
#' Logical. If `TRUE` (default), the saved paths are printed via `cli`.
#' @return
#' Invisibly, a character vector of the saved file paths.
#' @export
save_figure <- function(
    plot,
    file_name,
    height,
    dir_output = dir_figures,
    width = figure_width,
    units = figure_units,
    dpi = figure_dpi,
    vec_formats = c("png", "pdf"),
    verbose = TRUE) {
  assertthat::assert_that(
    ggplot2::is_ggplot(plot),
    msg = "`plot` must be a ggplot or patchwork object."
  )
  assertthat::assert_that(
    assertthat::is.string(file_name),
    assertthat::is.number(height),
    assertthat::is.number(width),
    assertthat::is.flag(verbose),
    msg = "Invalid `file_name`, `height`, `width` or `verbose`."
  )

  base::dir.create(
    dir_output,
    showWarnings = FALSE,
    recursive = TRUE
  )

  res_paths <-
    base::file.path(
      dir_output,
      stringr::str_glue("{file_name}.{vec_formats}")
    )

  purrr::walk(
    .x = res_paths,
    .f = ~ ggview::save_ggplot(
      plot = plot,
      file = .x,
      width = width,
      height = height,
      units = units,
      dpi = dpi,
      bg = "white"
    )
  )

  if (
    verbose
  ) {
    cli::cli_inform("Saved {.file {res_paths}}")
  }

  return(base::invisible(res_paths))
}
