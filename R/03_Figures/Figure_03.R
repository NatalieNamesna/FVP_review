#----------------------------------------------------------#
#
#
#                 The FVP review project
#
#                        Figure 3
#                  - functional information
#
#           N. Namesna, S. Flantua, O. Mottl
#
#                         2026
#
#----------------------------------------------------------#

#  (a) Number of case studies per year by proxy of functional diversity,
#  (b) source of trait data, (c) plant traits used in each case study.


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

label_width_waffle <- 18L


#----------------------------------------------------------#
# 2. Proxy of functional diversity per year -----
#----------------------------------------------------------#

data_year_pft <-
  data_case_studies |>
  dplyr::mutate(
    proxy = dplyr::if_else(
      source_of_fuctional_information_plant_functional_types,
      "PFTs",
      "Plant traits"
    )
  ) |>
  dplyr::count(year, proxy)

plot_year_pft <-
  plot_case_studies_per_year(
    data_counts = data_year_pft,
    col_fill = "proxy",
    palette = palette_pft
  ) +
  ggplot2::labs(
    tag = "(a)"
  ) +
  ggplot2::theme(
    legend.position = "none"
  )

data_waffle_pft <-
  prepare_waffle_data(
    data_source = data_case_studies,
    vec_columns = c(
      plant_functional_types =
        "source_of_fuctional_information_plant_functional_types"
    )
  )

plot_waffle_pft <-
  plot_waffle(
    data_waffle = data_waffle_pft,
    vec_facet_labels = c(
      plant_functional_types = "Studies using PFTs"
    ),
    palette = palette_pft_waffle,
    vec_fill_order = vec_waffle_fill_order,
    n_col = 1,
    point_size = 2,
    label_width = label_width_waffle
  ) +
  # The inset box is drawn by build_anchored_inset(); the margin keeps
  #    the waffle off its outline.
  ggplot2::theme(
    plot.margin = ggplot2::margin(1.5, 1.5, 1.5, 1.5, "mm")
  )

# Upper end of the x-axis (2 % expansion in plot_case_studies_per_year())
#    and the largest yearly count, to whose grid line the waffle is
#    aligned.
n_studies_year_max <-
  data_year_pft |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    n_total = base::sum(n),
    .groups = "drop"
  ) |>
  dplyr::pull(n_total) |>
  base::max()

x_axis_upper <- n_studies_year_max * 1.02

# The PFT waffle sits in the empty space to the right of the bars, with
#    its right edge on the last grid line. The colour key sits directly
#    on top of it, centred.
plot_year_pft_inset <-
  plot_year_pft +
  patchwork::inset_element(
    build_anchored_inset(
      plot = plot_waffle_pft,
      aspect_ratio = 0.9,
      just = c("right", "bottom"),
      grob_header = build_colour_legend_grob(
        palette = palette_pft,
        flag_inset_box = TRUE,
        hjust = 0.5,
        vjust = 0
      )
    ),
    left = 0.6,
    bottom = 0.17,
    right = n_studies_year_max / x_axis_upper,
    top = 0.95
  )


#----------------------------------------------------------#
# 3. Source of trait data -----
#----------------------------------------------------------#

data_waffle_trait_source <-
  prepare_waffle_data(
    data_source = data_case_studies,
    vec_columns = c(
      database =
        "source_of_traits_data_did_the_authors_source_from_database",
      published_study = stringr::str_c(
        "source_of_traits_data_",
        "did_the_authors_source_from_published_study"
      ),
      other_source = "source_of_fuctional_information_other",
      unpublished_study = stringr::str_c(
        "source_of_traits_data_",
        "did_the_authors_source_from_unpublished_private_data"
      )
    )
  )

plot_trait_source <-
  plot_waffle(
    data_waffle = data_waffle_trait_source,
    vec_facet_labels = c(
      database = "From a database?",
      published_study = "From a published study?",
      other_source = "From another source?",
      unpublished_study = "From an unpublished study?"
    ),
    palette = palette_true_false,
    vec_fill_order = vec_waffle_fill_order,
    n_col = 2,
    point_size = 2.6,
    label_width = label_width_waffle,
    title = "Source of trait data"
  ) +
  ggplot2::labs(
    tag = "(b)"
  )


#----------------------------------------------------------#
# 4. Plant traits used -----
#----------------------------------------------------------#

data_waffle_traits <-
  prepare_waffle_data(
    data_source = data_case_studies,
    vec_columns = c(
      la = "traits_did_the_authors_use_la",
      ldmc = "traits_did_the_authors_use_ldmc",
      leaf_c = "traits_did_the_authors_use_leaf_c",
      leaf_morphology = "traits_did_the_authors_use_leaf_morphology",
      leaf_n = "traits_did_the_authors_use_leaf_n",
      leaf_p = "traits_did_the_authors_use_leaf_p",
      sla = "traits_did_the_authors_use_sla",
      plant_height = "traits_did_the_authors_use_plant_height",
      seed_traits = "traits_did_the_authors_use_seed_traits",
      wood_traits = "traits_did_the_authors_use_wood_traits",
      others = "traits_others"
    )
  )

# Traits are split into leaf and non-leaf traits, the grouping used in
#    06_Plant_traits_information_analysis.R. Both keep the same waffle
#    size (6 columns in total).
plot_traits_leaf <-
  plot_waffle(
    data_waffle = data_waffle_traits,
    vec_facet_labels = c(
      la = "Leaf area",
      ldmc = "Leaf dry matter content",
      leaf_c = "Leaf carbon",
      leaf_morphology = "Leaf morphology",
      leaf_n = "Leaf nitrogen",
      leaf_p = "Leaf phosphorus",
      sla = "Specific leaf area"
    ),
    palette = palette_true_false,
    vec_fill_order = vec_waffle_fill_order,
    n_col = 4,
    point_size = 2,
    label_width = label_width_waffle,
    title = "Leaf traits used"
  ) +
  ggplot2::labs(
    tag = "(c)"
  )

plot_traits_other <-
  plot_waffle(
    data_waffle = data_waffle_traits,
    vec_facet_labels = c(
      plant_height = "Height",
      seed_traits = "Seed traits",
      wood_traits = "Wood traits",
      others = "Others"
    ),
    palette = palette_true_false,
    vec_fill_order = vec_waffle_fill_order,
    n_col = 2,
    point_size = 2,
    label_width = label_width_waffle,
    title = "Non-leaf traits used"
  )

plot_traits <-
  plot_traits_leaf + plot_traits_other +
  patchwork::plot_layout(
    widths = c(4, 2)
  )


#----------------------------------------------------------#
# 5. Compose and save -----
#----------------------------------------------------------#

plot_figure_03 <-
  (
    plot_year_pft_inset + plot_trait_source +
      patchwork::plot_layout(widths = c(1.1, 1))
  ) /
  patchwork::free(plot_traits) /
  plot_colour_legend(
    palette = palette_true_false,
    flag_inset_box = TRUE
  ) +
  patchwork::plot_layout(
    heights = grid::unit(c(1.2, 1, 6), c("null", "null", "mm"))
  )

save_figure(
  plot = plot_figure_03,
  file_name = "Figure_03",
  height = vec_figure_heights[["figure_03"]]
)
