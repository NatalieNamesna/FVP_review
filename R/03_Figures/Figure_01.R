#----------------------------------------------------------#
#
#
#                 The FVP review project
#
#                        Figure 1
#                  - publication metadata
#
#           N. Namesna, S. Flantua, O. Mottl
#
#                         2026
#
#----------------------------------------------------------#

#  (a) Number of case studies per year by region, (b) journals,
#  (c) geographic distribution, (d) pollen and trait databases.


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

data_regions <-
  prepare_region_names(
    data_source = data_case_studies,
    method = "split"
  )


#----------------------------------------------------------#
# 2. Case studies per year by region -----
#----------------------------------------------------------#

data_year_region <-
  data_case_studies |>
  dplyr::select(id, year) |>
  dplyr::left_join(
    data_regions,
    by = dplyr::join_by(id),
    relationship = "one-to-many"
  ) |>
  dplyr::count(year, region)

plot_year_region <-
  plot_case_studies_per_year(
    data_counts = data_year_region,
    col_fill = "region",
    palette = palette_regions[
      base::names(palette_regions) != "Multiple regions"
    ]
  ) +
  ggplot2::labs(
    tag = "(a)"
  ) +
  ggplot2::theme(
    legend.position = "none"
  )

# The region key sits in the empty space to the right of the bars.
plot_year_region_legend <-
  plot_year_region +
  patchwork::inset_element(
    plot_colour_legend(
      palette = palette_regions[
        base::names(palette_regions) != "Multiple regions"
      ],
      title = "Regions:",
      direction = "vertical",
      font_size = font_size_small,
      flag_inset_box = TRUE
    ),
    left = 0.6,
    bottom = 0.45,
    right = 1,
    top = 1
  )


#----------------------------------------------------------#
# 3. Journals -----
#----------------------------------------------------------#

data_journals <-
  data_case_studies |>
  dplyr::mutate(
    journal = stringr::str_squish(journal) |>
      stringr::str_replace_all(
        pattern = "&",
        replacement = "and"
      ) |>
      stringr::str_to_title() |>
      # Lower-case short words, except at the start of a title.
      stringr::str_replace_all(
        pattern = "(?<=\\s)(Of|And|In|The)(?=\\s)",
        replacement = stringr::str_to_lower
      ),
    journal = dplyr::case_when(
      journal == "Journal of Quartenary Science" ~
        "Journal of Quaternary Science",
      journal == "Plos One" ~ "PLOS One",
      journal %in% c(
        "Veget Hist Archaeobot",
        "Vegetation History and Archeobotany"
      ) ~ "Vegetation History and Archaeobotany",
      .default = journal
    )
  ) |>
  dplyr::count(journal, sort = TRUE) |>
  dplyr::mutate(
    label = stringr::str_wrap(journal, width = 25)
  )

plot_journals <-
  ggplot2::ggplot(
    data = data_journals,
    mapping = ggplot2::aes(
      label = label,
      size = n
    )
  ) +
  # Word-cloud sizes are in pt (1 study = 9 pt). ggwordcloud packs the
  #    words tightly, so the sizes set how much of the panel is filled.
  ggplot2::scale_size(
    range = c(9, 27)
  ) +
  ggplot2::labs(
    tag = "(c)"
  ) +
  ggplot2::theme_void() +
  ggplot2::theme(
    plot.tag = ggplot2::element_text(
      size = font_size_title,
      face = "bold"
    ),
    plot.tag.position = "topleft",
    plot.tag.location = "margin"
  ) +
  ggwordcloud::geom_text_wordcloud_area(
    colour = colour_journals,
    lineheight = 0.85,
    eccentricity = 0.75,
    seed = 900723
  )


#----------------------------------------------------------#
# 4. Geographic distribution -----
#----------------------------------------------------------#

data_region_counts <-
  data_regions |>
  dplyr::count(region, name = "n_studies") |>
  dplyr::mutate(
    region = forcats::fct_reorder(region, n_studies)
  )

n_studies_max <-
  data_region_counts |>
  dplyr::pull(n_studies) |>
  base::max()

# Upper end of the x-axis (15 % space for the bar labels) and the last
#    grid line, to which the map inset is aligned.
x_axis_upper <- n_studies_max * 1.15
x_gridline_last <- base::floor(x_axis_upper / 5) * 5

# Bars and map use the region colours of panel (a); the bar length
#    shows the number of case studies.
scale_fill_regions <-
  ggplot2::scale_fill_manual(
    values = palette_regions,
    na.value = "grey90",
    guide = "none"
  )

plot_region_counts <-
  ggplot2::ggplot(
    data = data_region_counts,
    mapping = ggplot2::aes(
      x = n_studies,
      y = region,
      fill = region
    )
  ) +
  ggplot2::scale_x_continuous(
    breaks = base::seq(0, x_gridline_last, by = 5),
    expand = ggplot2::expansion(mult = c(0, 0.15))
  ) +
  scale_fill_regions +
  ggplot2::labs(
    tag = "(b)",
    x = "Number of case studies",
    y = NULL
  ) +
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_blank()
  ) +
  ggplot2::geom_col(
    width = 0.8
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(label = n_studies),
    hjust = -0.3,
    size = font_size_geom_small
  )

# Russia is split into its European and Asian federal districts, so
#    that each part is coloured by its own continent.
vec_russia_districts_europe <-
  c("Central", "Southern", "Northwestern", "North Caucasian", "Volga")

data_russia <-
  rnaturalearth::ne_states(
    country = "Russia",
    returnclass = "sf"
  ) |>
  dplyr::mutate(
    continent = dplyr::if_else(
      region %in% vec_russia_districts_europe,
      "Europe",
      "Asia"
    )
  ) |>
  dplyr::select(continent)

data_world <-
  rnaturalearth::ne_countries(
    returnclass = "sf"
  ) |>
  dplyr::filter(
    !continent %in% c("Antarctica", "Seven seas (open ocean)"),
    admin != "Russia"
  ) |>
  dplyr::select(continent) |>
  dplyr::bind_rows(data_russia)

plot_map <-
  ggplot2::ggplot(
    data = data_world
  ) +
  scale_fill_regions +
  ggplot2::theme_void() +
  # The inset box is drawn by build_anchored_inset(); the margin keeps
  #    the map off its outline.
  ggplot2::theme(
    plot.margin = ggplot2::margin(1.5, 1.5, 1.5, 1.5, "mm")
  ) +
  ggplot2::geom_sf(
    mapping = ggplot2::aes(fill = continent),
    colour = "white",
    linewidth = 0.05
  ) +
  # Added after geom_sf(), which would otherwise replace the projection.
  ggplot2::coord_sf(
    crs = "+proj=robin",
    expand = FALSE
  )

# Height / width of the projected map, so the inset keeps its shape.
vec_map_bbox <-
  data_world |>
  sf::st_transform(crs = "+proj=robin") |>
  sf::st_bbox()

map_aspect_ratio <-
  (vec_map_bbox[["ymax"]] - vec_map_bbox[["ymin"]]) /
  (vec_map_bbox[["xmax"]] - vec_map_bbox[["xmin"]])

# The map sits in the empty lower-right corner of the bar chart: its lower
#    edge on the x-axis and its right edge on the last grid line.
plot_geography <-
  plot_region_counts +
  patchwork::inset_element(
    build_anchored_inset(
      plot = plot_map,
      aspect_ratio = map_aspect_ratio,
      just = c("right", "bottom")
    ),
    left = 0.3,
    bottom = 0,
    right = x_gridline_last / x_axis_upper,
    top = 0.55
  )


#----------------------------------------------------------#
# 5. Pollen and trait databases -----
#----------------------------------------------------------#

#--------------------------------------------------#
## 5.1. Prepare data -----
#--------------------------------------------------#

# Neotoma and the pollen databases listed as its constituent databases at
#    https://www.neotomadb.org/data/constituent-databases (checked
#    2026-10-15). "Japanese Pollen Database" is listed there as "Japan
#    Pollen Database". The European Modern Pollen Database is not a
#    constituent database.
vec_neotoma_databases <-
  c(
    "Neotoma unspecified",
    "African Pollen Database",
    "Chinese Pollen Database",
    "European Pollen Database",
    "Indo-Pacific Pollen Database",
    "Japanese Pollen Database",
    "Latin American Pollen Database",
    "North American Pollen Database"
  )

data_pollen_databases <-
  data_case_studies |>
  dplyr::select(id, database = source_of_pollen_data_which_database) |>
  tidyr::separate_longer_delim(
    cols = database,
    delim = ","
  ) |>
  dplyr::mutate(
    database = stringr::str_squish(database) |>
      stringr::str_to_lower() |>
      stringr::str_to_title(),
    database = dplyr::case_when(
      base::is.na(database) ~ "Not Available",
      # Studies naming Neotoma itself, not one of its constituent
      #    databases.
      database == "Neotoma" ~ "Neotoma unspecified",
      database == "Pangea" ~ "Pangaea",
      database == "Latin America Pollen Database" ~
        "Latin American Pollen Database",
      database == "Se Australian Pollen Database" ~
        "SE Australian Pollen Database",
      .default = database
    ),
    group = dplyr::case_when(
      database == "Not Available" ~ "Not Available",
      database %in% vec_neotoma_databases ~
        "Neotoma or its constituent databases",
      .default = "Other"
    )
  ) |>
  dplyr::count(database, group)

data_trait_databases <-
  data_case_studies |>
  dplyr::select(id, database = source_of_trait_data_which_database) |>
  tidyr::separate_longer_delim(
    cols = database,
    delim = ","
  ) |>
  dplyr::mutate(
    database = stringr::str_squish(database),
    database = dplyr::case_when(
      base::is.na(database) ~ "Not Available",
      # A pollen database entered in the trait column (study 42, its only
      #    entry). To be corrected in the source table.
      database == "China Quaternary Pollen Data Base" ~ "Not Available",
      database == "Flora Europea" ~ "Flora Europaea",
      database == "LEDA trait database" ~ "LEDA",
      database %in% c("WD database", "Wood density database") ~
        "Global Wood Density Database",
      .default = database
    ),
    group = dplyr::if_else(
      database == "Not Available",
      "Not Available",
      "Trait database"
    )
  ) |>
  dplyr::count(database, group)

#--------------------------------------------------#
## 5.2. Plot -----
#--------------------------------------------------#

# Only databases used by 3+ studies are labelled here, as some boxes of
#    databases used by 2 studies are too small for a readable label.
plot_pollen_databases <-
  plot_treemap(
    data_databases = data_pollen_databases,
    palette = palette_tree_pollen,
    title = "Pollen databases",
    n_label_min = 3L
  ) +
  ggplot2::labs(
    tag = "(d)"
  )

plot_trait_databases <-
  plot_treemap(
    data_databases = data_trait_databases,
    palette = palette_tree_traits,
    title = "Trait databases"
  )

plot_databases <-
  (
    plot_pollen_databases | plot_trait_databases
  ) /
  (
    plot_colour_legend(
      palette = palette_tree_pollen,
      font_size = font_size_small
    ) |
      plot_colour_legend(
        palette = palette_tree_traits,
        font_size = font_size_small
      )
  ) +
  patchwork::plot_layout(
    heights = grid::unit(c(1, 5), c("null", "mm"))
  )


#----------------------------------------------------------#
# 6. Compose and save -----
#----------------------------------------------------------#

# free() stops patchwork from aligning the word cloud and panel (d) with
#    the axis of panel (a), which left empty space on their left.
plot_figure_01 <-
  (
    plot_year_region_legend + plot_geography +
      patchwork::plot_layout(widths = c(1, 1.2))
  ) /
  patchwork::free(plot_journals) /
  patchwork::free(plot_databases) +
  patchwork::plot_layout(
    heights = c(1.2, 0.7, 1)
  )

save_figure(
  plot = plot_figure_01,
  file_name = "Figure_01",
  height = vec_figure_heights[["figure_01"]]
)
