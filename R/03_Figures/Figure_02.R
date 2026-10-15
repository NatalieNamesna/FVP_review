#----------------------------------------------------------#
#
#
#                 The FVP review project
#
#                        Figure 2
#                  - pollen records metadata
#
#           N. Namesna, S. Flantua, O. Mottl
#
#                         2026
#
#----------------------------------------------------------#

#  (a) Time coverage of each case study by region,
#  (b) waffle plots of how pollen records were chosen, sourced and
#    processed.


#----------------------------------------------------------#
# 1. Set up -----
#----------------------------------------------------------#

source(
  here::here("R/00_Config_file.R")
)

data_case_studies <-
  readr::read_csv(
    file = path_data_clean,
    show_col_types = FALSE
  )


#----------------------------------------------------------#
# 2. Time coverage -----
#----------------------------------------------------------#

#--------------------------------------------------#
## 2.1. Prepare data -----
#--------------------------------------------------#

data_time_coverage <-
  data_case_studies |>
  dplyr::select(
    id,
    time_young = time_period_period_young_kyr,
    time_old = time_period_period_old_kyr
  ) |>
  dplyr::left_join(
    prepare_region_names(
      data_source = data_case_studies,
      method = "collapse"
    ),
    by = dplyr::join_by(id),
    relationship = "one-to-one",
    unmatched = "error"
  )

# Group case studies by region, top to bottom in the same order as the
#    region legend (palette order), and sort them by the oldest time
#    within each region.
vec_region_order <-
  base::intersect(
    base::names(palette_regions),
    data_time_coverage[["region"]]
  )

data_time_coverage_sorted <-
  data_time_coverage |>
  dplyr::mutate(
    region = base::factor(region, levels = vec_region_order)
  ) |>
  dplyr::arrange(region, dplyr::desc(time_old)) |>
  dplyr::mutate(
    id = base::factor(id, levels = base::rev(id))
  )

#--------------------------------------------------#
## 2.2. Break the time axis -----
#--------------------------------------------------#

# Almost all case studies are younger than 50 kyr, so the time axis is
#    broken between 50 and 120 kyr. Each window is drawn as its own facet
#    (free width), with segments clipped to the window. This replaces
#    `ggbreak`, which does not work inside patchwork.
time_max <-
  data_time_coverage_sorted |>
  dplyr::pull(time_old) |>
  base::max()

data_time_windows <-
  tibble::tibble(
    window = base::factor(
      c("old", "young"),
      levels = c("old", "young")
    ),
    window_lwr = c(120, 0),
    window_upr = c(time_max, 50)
  )

data_time_segments <-
  data_time_coverage_sorted |>
  dplyr::cross_join(data_time_windows) |>
  dplyr::filter(
    time_old >= window_lwr,
    time_young <= window_upr
  ) |>
  dplyr::mutate(
    x_start = base::pmax(time_young, window_lwr),
    x_end = base::pmin(time_old, window_upr)
  )

data_time_points <-
  data_time_coverage_sorted |>
  tidyr::pivot_longer(
    cols = c(time_young, time_old),
    names_to = "end",
    values_to = "time"
  ) |>
  dplyr::cross_join(data_time_windows) |>
  dplyr::filter(
    time >= window_lwr,
    time <= window_upr
  )

data_epochs <-
  tibble::tibble(
    epoch = c("Holocene", "LGM", "Pleistocene"),
    epoch_start = c(0, 19, 11.7),
    epoch_end = c(11.7, 26.5, time_max),
    fill = c("#D5D5D5", "#BFEFFF", "#8A9497"),
    alpha = c(0.2, 0.4, 0.15)
  )

data_epochs_clipped <-
  data_epochs |>
  dplyr::cross_join(data_time_windows) |>
  dplyr::filter(
    epoch_end >= window_lwr,
    epoch_start <= window_upr
  ) |>
  dplyr::mutate(
    xmin = base::pmax(epoch_start, window_lwr),
    xmax = base::pmin(epoch_end, window_upr)
  )

data_epoch_labels <-
  tibble::tibble(
    window = base::factor(
      c("old", "young", "young"),
      levels = c("old", "young")
    ),
    x = c(125, 22.75, 5.85),
    label = c("Pleistocene", "LGM", "Holocene")
  )

# Grid lines every 10 kyr, kept per window so they do not stretch the
#    free x-axis of either facet.
data_time_gridlines <-
  tibble::tibble(
    time = base::seq(0, time_max, by = 10)
  ) |>
  dplyr::cross_join(data_time_windows) |>
  dplyr::filter(
    time >= window_lwr,
    time <= window_upr
  )

#--------------------------------------------------#
## 2.3. Plot -----
#--------------------------------------------------#

plot_time_coverage <-
  ggplot2::ggplot(
    data = data_time_segments,
    mapping = ggplot2::aes(y = id)
  ) +
  ggplot2::facet_grid(
    cols = ggplot2::vars(window),
    scales = "free_x",
    space = "free_x"
  ) +
  ggplot2::scale_x_reverse(
    breaks = c(0, 10, 20, 30, 40, 120, 130),
    expand = ggplot2::expansion(add = 0.5)
  ) +
  ggplot2::scale_y_discrete(
    expand = ggplot2::expansion(add = c(1, 5))
  ) +
  ggplot2::scale_fill_identity() +
  ggplot2::scale_alpha_identity() +
  ggplot2::scale_colour_manual(
    values = palette_regions,
    breaks = base::names(palette_regions),
    limits = base::levels(data_time_coverage_sorted[["region"]])
  ) +
  ggplot2::labs(
    tag = "(a)",
    x = "Time (kyr)",
    y = NULL
  ) +
  ggplot2::theme(
    axis.text.y = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    strip.text = ggplot2::element_blank(),
    panel.spacing.x = ggplot2::unit(1.5, "mm"),
    legend.position = "none"
  ) +
  ggplot2::geom_rect(
    data = data_epochs_clipped,
    mapping = ggplot2::aes(
      xmin = xmin,
      xmax = xmax,
      ymin = -Inf,
      ymax = Inf,
      fill = fill,
      alpha = alpha
    ),
    inherit.aes = FALSE
  ) +
  # Drawn over the epoch shading, which would otherwise hide them.
  ggplot2::geom_vline(
    data = data_time_gridlines,
    mapping = ggplot2::aes(xintercept = time),
    colour = "grey85",
    linewidth = 0.3
  ) +
  ggplot2::geom_text(
    data = data_epoch_labels,
    mapping = ggplot2::aes(
      x = x,
      y = Inf,
      label = label
    ),
    inherit.aes = FALSE,
    vjust = 1.5,
    size = font_size_geom_small,
    fontface = "bold"
  ) +
  ggplot2::geom_segment(
    mapping = ggplot2::aes(
      x = x_start,
      xend = x_end,
      yend = id,
      colour = region
    ),
    linewidth = 0.4
  ) +
  ggplot2::geom_point(
    data = data_time_points,
    mapping = ggplot2::aes(
      x = time,
      colour = region
    ),
    size = 0.6
  )


#----------------------------------------------------------#
# 3. Pollen information -----
#----------------------------------------------------------#

#--------------------------------------------------#
## 3.1. Prepare data -----
#--------------------------------------------------#

data_waffle_pollen <-
  prepare_waffle_data(
    data_source = data_case_studies,
    vec_columns = c(
      choosing_method = stringr::str_c(
        "method_of_choosing_pollen_data_",
        "did_the_authors_have_any_specific_method_for_selecting_records"
      ),
      choosing_method_length = stringr::str_c(
        "method_of_choosing_pollen_data_",
        "did_the_authors_select_records_based_on_their_length"
      ),
      choosing_method_info = stringr::str_c(
        "method_of_choosing_pollen_data_",
        "did_the_authors_select_records_based_on_the_amount_of_information"
      ),
      choosing_method_taxa = stringr::str_c(
        "method_of_choosing_pollen_data_",
        "did_the_authors_select_records_based_on_the_presence_of_",
        "specific_taxa_life_forms"
      ),
      database = "source_of_pollen_data_did_the_authors_source_from_database",
      published_study = stringr::str_c(
        "source_of_pollen_data_",
        "did_the_authors_source_from_published_study"
      ),
      experimental_study = "source_of_pollen_data_experimental_study",
      unpublished_study = stringr::str_c(
        "source_of_pollen_data_",
        "did_the_authors_source_from_unpublished_private_data"
      ),
      age_depth_recalculation = "age_depth_recalculation",
      counts_correction = "pollen_counts_corrections",
      harmonisation = "pollen_nomenclature_harmonisation"
    ),
    # Keeps the coding of 05_Pollen_information_analysis.R, where one
    #    ambiguous "TRUE\r\n" entry of counts correction is not reported.
    vec_values_not_reported = "TRUE\r\n"
  )

#--------------------------------------------------#
## 3.2. Plot -----
#--------------------------------------------------#

# Dots almost touch: the dot pitch is about 2.5 mm at this layout.
point_size_waffle <- 2
label_width_waffle <- 16L

plot_pollen_method <-
  plot_waffle(
    data_waffle = data_waffle_pollen,
    vec_facet_labels = c(
      choosing_method = "Any method of choosing records?",
      choosing_method_length = "Based on record length?",
      choosing_method_info = "Based on amount of information?",
      choosing_method_taxa = "Based on available plant taxa?"
    ),
    palette = palette_true_false,
    vec_fill_order = vec_waffle_fill_order,
    n_col = 2,
    point_size = point_size_waffle,
    label_width = label_width_waffle,
    title = "Choosing pollen data"
  ) +
  ggplot2::labs(
    tag = "(b)"
  )

plot_pollen_source <-
  plot_waffle(
    data_waffle = data_waffle_pollen,
    vec_facet_labels = c(
      database = "From a database?",
      published_study = "From a published study?",
      experimental_study = "From an experimental study?",
      unpublished_study = "From an unpublished study?"
    ),
    palette = palette_true_false,
    vec_fill_order = vec_waffle_fill_order,
    n_col = 2,
    point_size = point_size_waffle,
    label_width = label_width_waffle,
    title = "Source of pollen data"
  )

plot_pollen_processing <-
  plot_waffle(
    data_waffle = data_waffle_pollen,
    vec_facet_labels = c(
      age_depth_recalculation = "Age-depth recalculation applied?",
      counts_correction = "Counts correction applied?",
      harmonisation = "Taxonomic harmonisation applied?"
    ),
    palette = palette_true_false,
    vec_fill_order = vec_waffle_fill_order,
    n_col = 2,
    point_size = point_size_waffle,
    label_width = label_width_waffle,
    title = "Data processing"
  )


#----------------------------------------------------------#
# 4. Compose and save -----
#----------------------------------------------------------#

# Share of the panel width taken by the 130-120 kyr window (each window
#    has 0.5 kyr of padding on both sides, see scale_x_reverse()).
vec_window_widths <-
  data_time_windows |>
  dplyr::mutate(
    width = window_upr - window_lwr + 1
  ) |>
  dplyr::pull(width)

window_old_share <-
  vec_window_widths[[1]] / base::sum(vec_window_widths)

# The region legend sits in the 130-120 kyr window, just under the
#    "Pleistocene" label and centred between 130 and 120 kyr.
plot_time_coverage_legend <-
  plot_time_coverage +
  patchwork::inset_element(
    plot_colour_legend(
      palette = palette_regions[
        base::levels(data_time_coverage_sorted[["region"]])
      ],
      title = "Regions:",
      direction = "vertical",
      font_size = font_size_small,
      flag_inset_box = TRUE,
      hjust = 0.5
    ),
    left = 0,
    bottom = 0.47,
    right = window_old_share,
    top = 0.93
  )

plot_pollen_information <-
  (plot_pollen_method | plot_pollen_source | plot_pollen_processing) /
  plot_colour_legend(
    palette = palette_true_false,
    flag_inset_box = TRUE
  ) +
  patchwork::plot_layout(
    heights = grid::unit(c(1, 6), c("null", "mm"))
  )

# free() stops patchwork from aligning panel (b) with the axes of
#    panel (a), which left empty space at its sides.
plot_figure_02 <-
  plot_time_coverage_legend /
  patchwork::free(plot_pollen_information) +
  patchwork::plot_layout(
    heights = grid::unit(c(1, 70), c("null", "mm"))
  )

save_figure(
  plot = plot_figure_02,
  file_name = "Figure_02",
  height = vec_figure_heights[["figure_02"]]
)
