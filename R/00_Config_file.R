#----------------------------------------------------------#
#
#
#                 The FVP review project
#
#                       Config file
#
#           N. Namesna, S. Flantua, O. Mottl
#
#                         2026
#
#----------------------------------------------------------#

#  Shared paths, figure dimensions, font sizes, palettes and theme.
#    Sourced at the start of every figure script.
#
#  Required packages (install once, not from scripts):
#    install.packages(c(
#      "tidyverse", "here", "assertthat", "cli", "patchwork", "ggview",
#      "ggtext", "ggwordcloud", "treemapify", "sf", "rnaturalearth"
#    ))
#    install.packages(
#      "rnaturalearthhires",
#      repos = "https://ropensci.r-universe.dev"
#    )


#----------------------------------------------------------#
# 1. Packages -----
#----------------------------------------------------------#

vec_packages_required <-
  c(
    "assertthat",
    "cli",
    "dplyr",
    "forcats",
    "ggplot2",
    "ggtext",
    "ggview",
    "ggwordcloud",
    "gridtext",
    "here",
    "patchwork",
    "purrr",
    "readr",
    "rlang",
    "rnaturalearth",
    "rnaturalearthhires",
    "sf",
    "stringr",
    "tibble",
    "tidyr",
    "treemapify"
  )

vec_packages_missing <-
  vec_packages_required |>
  purrr::discard(
    .p = ~ base::requireNamespace(.x, quietly = TRUE)
  )

if (
  base::length(vec_packages_missing) > 0
) {
  cli::cli_abort(
    "Install missing packages first: {.pkg {vec_packages_missing}}"
  )
}

# Attached so that the patchwork operators (`|`, `/`, `+`) are available.
library(patchwork)


#----------------------------------------------------------#
# 2. Paths -----
#----------------------------------------------------------#

path_data_clean <-
  here::here("Data/Processed/case_studies_clean.csv")

dir_figures <-
  here::here("Outputs/Figures")

dir_functions <-
  here::here("R/Functions")


#----------------------------------------------------------#
# 3. Figure dimensions -----
#----------------------------------------------------------#

# Full page width is 166 mm, which at 300 dpi gives the
#    maximum of 1961 px.
figure_width <- 166
figure_units <- "mm"
figure_dpi <- 300

vec_figure_heights <-
  c(
    figure_01 = 215,
    figure_02 = 165,
    figure_03 = 165,
    figure_04 = 105
  )


#----------------------------------------------------------#
# 4. Font sizes (pt) -----
#----------------------------------------------------------#

font_size_base <- 8
font_size_small <- 7
font_size_title <- 9

# Size of text drawn by geoms is in mm, not pt.
font_size_geom_small <- font_size_small / ggplot2::.pt


#----------------------------------------------------------#
# 5. Palettes -----
#----------------------------------------------------------#

# Qualitative (regions have no order), based on Paul Tol's "muted" set.
#    Avoids the hues of TRUE/FALSE and of the PFT palette, stays distinct
#    under colour-blind simulation, and is readable as text on white.
palette_regions <-
  c(
    "Africa" = "#B8A044",
    "Asia" = "#882255",
    "Europe" = "#332288",
    "North America" = "#6699CC",
    "Oceania" = "#AA4499",
    "South America" = "#8C5A2B",
    "Multiple regions" = "grey40",
    "Not Available" = "grey70"
  )

palette_true_false <-
  c(
    "TRUE" = "#52BCA3",
    "FALSE" = "#E58606",
    "Not reported" = "grey70"
  )

# Waffle dots are filled from the bottom: FALSE first, TRUE on top.
vec_waffle_fill_order <- c("FALSE", "Not reported", "TRUE")

palette_pft <-
  c(
    "Plant traits" = "#56B4E9",
    "PFTs" = "#CC79A7"
  )

# The PFT waffle reuses the bar colours so that both read as one key.
palette_pft_waffle <-
  c(
    "TRUE" = "#CC79A7",
    "FALSE" = "#56B4E9",
    "Not reported" = "grey70"
  )

# Treemaps use hues not used for anything else: slate for pollen and
#    sage for traits. Grey still means "Not Available".
palette_tree_pollen <-
  c(
    "Neotoma or its constituent databases" = "#3F5368",
    "Other" = "#8193A6",
    "Not Available" = "grey70"
  )

palette_tree_traits <-
  c(
    "Trait database" = "#8E9F7F",
    "Not Available" = "grey70"
  )

# All insets (legends, map, PFT waffle) have a white background and a
#    dashed outline in this colour.
colour_inset_outline <- "grey70"

# Journal names carry no categories, so they are all one colour.
colour_journals <- "grey15"


#----------------------------------------------------------#
# 6. Theme -----
#----------------------------------------------------------#

theme_fvp <-
  ggplot2::theme_minimal(
    base_size = font_size_base
  ) +
  ggplot2::theme(
    text = ggplot2::element_text(colour = "black"),
    axis.text = ggplot2::element_text(
      size = font_size_small,
      colour = "black"
    ),
    strip.text = ggplot2::element_text(
      size = font_size_small,
      lineheight = 0.9
    ),
    legend.text = ggplot2::element_text(size = font_size_small),
    legend.title = ggplot2::element_blank(),
    legend.key.size = ggplot2::unit(3, "mm"),
    plot.title = ggplot2::element_text(
      size = font_size_base,
      face = "bold",
      hjust = 0
    ),
    plot.title.position = "plot",
    plot.tag = ggplot2::element_text(
      size = font_size_title,
      face = "bold"
    ),
    # Tags sit in their own margin so they never overlap section titles.
    plot.tag.position = "topleft",
    plot.tag.location = "margin",
    plot.margin = ggplot2::margin(2, 2, 2, 2, "mm"),
    panel.grid.minor = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
    plot.background = ggplot2::element_rect(fill = "transparent", colour = NA)
  )

ggplot2::theme_set(theme_fvp)


#----------------------------------------------------------#
# 7. Functions -----
#----------------------------------------------------------#

base::list.files(
  path = dir_functions,
  pattern = "\\.R$",
  full.names = TRUE
) |>
  purrr::walk(
    .f = ~ base::source(.x)
  )
