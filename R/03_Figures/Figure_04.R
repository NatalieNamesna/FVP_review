#----------------------------------------------------------#
#
#
#                 The FVP review project
#
#                        Figure 4
#                  - methods of the case studies
#
#           N. Namesna, S. Flantua, O. Mottl
#
#                         2026
#
#----------------------------------------------------------#

#  Waffle plots of the methods used for linking functional information
#    with fossil pollen and for processing the combined data.


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
# 2. Prepare data -----
#----------------------------------------------------------#

data_waffle_methods <-
  prepare_waffle_data(
    data_source = data_case_studies,
    vec_columns = c(
      bayesian_modelling =
        "bayesian_modelling_for_aligning_pollen_types_with_plant_species",
      linking_table = stringr::str_c(
        "linking_fossil_pollen_to_plant_taxa_",
        "did_the_authors_use_specialized_linking_table"
      ),
      probabilistic_modelling = stringr::str_c(
        "linking_fossil_pollen_to_plant_taxa_",
        "did_the_authors_use_probabilistic_modelling"
      ),
      traits_aggregation = stringr::str_c(
        "linking_fossil_pollen_to_plant_taxa_",
        "did_the_authors_aggregate_the_traits"
      ),
      cwm = "cwm",
      effect_of_climate = stringr::str_c(
        "effect_of_variables_on_trait_composition_",
        "did_the_authors_incorporate_effect_of_climate"
      ),
      effect_of_humans = stringr::str_c(
        "effect_of_variables_on_trait_composition_",
        "did_the_authors_incorporate_effect_of_humans"
      ),
      effect_of_variables = stringr::str_c(
        "effect_of_variables_on_trait_composition_",
        "did_the_authors_test_it"
      )
    )
  )


#----------------------------------------------------------#
# 3. Build panels -----
#----------------------------------------------------------#

# Dots almost touch: the dot pitch is about 3.5 mm at this layout.
point_size_waffle <- 3
label_width_waffle <- 28L

plot_linking_method <-
  plot_waffle(
    data_waffle = data_waffle_methods,
    vec_facet_labels = c(
      bayesian_modelling = stringr::str_c(
        "Was Bayesian modelling used to link functional traits and pollen?"
      ),
      linking_table = stringr::str_c(
        "Was a specialized linking table used to link functional traits ",
        "and pollen?"
      ),
      probabilistic_modelling = stringr::str_c(
        "Was probabilistic modelling used to link functional traits ",
        "and pollen?"
      ),
      traits_aggregation = stringr::str_c(
        "Were the traits aggregated at a higher taxonomic level to link ",
        "functional traits and pollen?"
      )
    ),
    palette = palette_true_false,
    vec_fill_order = vec_waffle_fill_order,
    n_col = 2,
    point_size = point_size_waffle,
    label_width = label_width_waffle,
    title = stringr::str_wrap(
      "Methods of linking functional information with fossil pollen",
      width = 45
    )
  )

plot_variables <-
  plot_waffle(
    data_waffle = data_waffle_methods,
    vec_facet_labels = c(
      cwm = "Were Community Weighted Means used?",
      effect_of_climate = "Was the impact of climate considered?",
      effect_of_humans = "Was the impact of humans considered?",
      effect_of_variables = "Was the impact of any variables considered?"
    ),
    palette = palette_true_false,
    vec_fill_order = vec_waffle_fill_order,
    n_col = 2,
    point_size = point_size_waffle,
    label_width = label_width_waffle,
    title = stringr::str_c(
      "Processing of the combined trait-pollen data and ",
      "incorporation of variables"
    ) |>
      stringr::str_wrap(width = 45)
  )


#----------------------------------------------------------#
# 4. Compose and save -----
#----------------------------------------------------------#

plot_figure_04 <-
  (plot_linking_method | plot_variables) /
  plot_colour_legend(
    palette = palette_true_false,
    flag_inset_box = TRUE
  ) +
  patchwork::plot_layout(
    heights = grid::unit(c(1, 6), c("null", "mm"))
  )

save_figure(
  plot = plot_figure_04,
  file_name = "Figure_04",
  height = vec_figure_heights[["figure_04"]]
)
