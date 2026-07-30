#----------------------------------------------------------#
#
#
#                 The FVP review project
#
#           N. Namesna, S. Flantua, O. Mottl
#
#                         2026
#
#----------------------------------------------------------#

#  Methodology of case studies analysis

#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#

# packages 
install.packages("rlang")
install.packages("devtools")
devtools::install_github("liamgilbey/ggwaffle")
install.packages("waffle", repos = "https://cinc.rud.is")
install.packages("ggtext")
install.packages("showtext")
install.packages("sysfonts")
install.packages("rcartocolor")
install.packages("GGally")
install.packages("glue")
install.packages("marquee")
install.packages("readr")
install.packages("stringr")


library(rcartocolor)
library(sysfonts)
library(showtext)
library(ggtext)
library(waffle)
library(tidyverse)
library(ggplot2)
library(ggtext)
library(sf) 
library(here)
library(dplyr)
library(GGally)
library(glue)
library(marquee)
library(readr)
library(stringr)
library(patchwork)

# Load the table with case studies

case_studies <- readr::read_csv("Data/Processed/case_studies_clean.csv")


#----------------------------------------------------------#
# 2.  Data wrangling -----
#----------------------------------------------------------#

# table for all pollen information ----
case_studies_methods <- case_studies |> 
  select(id, c(54:63))

unique(case_studies$linking_fossil_pollen_to_plant_taxa_did_the_authors_aggregate_the_traits)


# table for waffle chart ----

case_studies_method_waffle <- case_studies_methods |> 
  select(bayesian_modelling_for_aligning_pollen_types_with_plant_species,                                                
         cwm,                                                                                                            
         linking_fossil_pollen_to_plant_taxa_did_the_authors_aggregate_the_traits,                                       
         linking_fossil_pollen_to_plant_taxa_did_the_authors_use_specialized_linking_table,                              
         linking_fossil_pollen_to_plant_taxa_did_the_authors_use_probabilistic_modelling,                                
         effect_of_variables_on_trait_composition_did_the_authors_test_it,                                               
         effect_of_variables_on_trait_composition_did_the_authors_incorporate_effect_of_climate,                         
         effect_of_variables_on_trait_composition_did_the_authors_incorporate_effect_of_humans)  |> 
  mutate(
    bayesian_modelling = as.character(bayesian_modelling_for_aligning_pollen_types_with_plant_species),                                                
    cwm = as.character(cwm),                                                                                                            
    traits_aggregation = as.character(linking_fossil_pollen_to_plant_taxa_did_the_authors_aggregate_the_traits),                                       
    linking_table = as.character(linking_fossil_pollen_to_plant_taxa_did_the_authors_use_specialized_linking_table),                              
    probabilistic_modelling = as.character(linking_fossil_pollen_to_plant_taxa_did_the_authors_use_probabilistic_modelling),                                
    effect_of_variables = as.character(effect_of_variables_on_trait_composition_did_the_authors_test_it),                                               
    effect_of_climate = as.character(effect_of_variables_on_trait_composition_did_the_authors_incorporate_effect_of_climate),                         
    effect_of_humans = as.character(effect_of_variables_on_trait_composition_did_the_authors_incorporate_effect_of_humans)) |> 
  select(c(2, 9:15)) |> 
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "value"
  ) |> 
  mutate(
    value = case_when(
      is.na(value) ~ "Not reported",
      value == "NA" ~ "Not reported",
      value == "\nTRUE" ~ "TRUE",
      value == "TRUE\r\n" ~ "TRUE",
      value == "FALSE \r\n- trait scores?" ~ "FALSE",
      value == TRUE ~ "TRUE",
      value == FALSE ~ "FALSE")) |> 
  count(variable, value, name = "count")


case_studies_method_waffle <- case_studies_method_waffle |>
  complete(
    variable,
    value = c("TRUE", "FALSE", "Not reported"),
    fill = list(count = 0)
  ) 



#-----------------------------------------------------------------------------#

# 4. waffle plot for linking method ----

#-----------------------------------------------------------------------------#

# data ----
case_studies_link_method_waffle <- case_studies_method_waffle |> 
  slice(c(1:3, 16:24))

facet_names_linking_method <- c(
  bayesian_modelling =
    str_wrap(
      "Was Bayesian modelling used to link functional traits and pollen?",
      width = 30
    ),
  linking_table =
    str_wrap(
      "Was a specialized linking table used to link functional traits and pollen?",
      width = 30
    ),
  probabilistic_modelling =
    str_wrap(
      "Was probabilistic modelling used to link functional traits and pollen?",
      width = 30
    ),
  traits_aggregation =
    str_wrap(
      "Were the traits aggregated at a higher taxonomic level to link functional traits and pollen?",
      width = 30
    )
)

# font ----
font_add(
  family = "Font Awesome 7",
  regular = "Data/Input/fonts/Font Awesome 7 Free-Solid-900.otf"
)
showtext_auto()
showtext_opts(dpi = 300)


# basic plot ----
waffle_chart_link_method <- ggplot(data = case_studies_link_method_waffle) +
  geom_pictogram(
    mapping = aes(
     label = value,
      color = value,
      values = count
    ),
    flip = TRUE,
    n_rows = 10,
    size = 4,
    family = "Font Awesome 7"
  ) +
  facet_wrap(~variable,
             nrow = 1,
             strip.position = "bottom",
             labeller = labeller(variable = facet_names_linking_method)
  )

waffle_chart_link_method


# add icons ----
icons_plot_link_method <- waffle_chart_link_method +
  scale_label_pictogram(
    values = "circle",
    guide = "none"
  )

icons_plot_link_method

# advanced styling ----
bg_col <- "#FAFAFA"
text_col <- "black"

# colors ----
col_palette_link_method <-  c("#E84746",  "#BFC2C1", "#509B51")

# vector of T and F
true_false_NA_link_method <- unique(case_studies_link_method_waffle$value)

# T and F now have their own colors
names(col_palette_link_method) <- true_false_NA_link_method

# plot with new colors and icons
col_plot_link_method <- icons_plot_link_method +
  scale_color_manual(
    values = col_palette_link_method,
    guide = "none"
  )

# Adding style text ----

## title and caption ----
title_link_method <- str_wrap("Methods of linking plant traits and fossil pollen",
                              width = 30)

# text plot ----
text_plot_link_method <- col_plot_link_method +
  labs(
    title = title_link_method
  )
text_plot_link_method


# scale plot ----
scale_plot_link_method <- text_plot_link_method +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = c(1, 2, 3, 4, 5, 6, 7),              # Every 2 rows
   # labels = c("10","20","30", "40", "50", "60", "70"), # Converts rows back to "counts" if desired
    limits = c(0, 10)                        # Caps it perfectly at your n_rows height
  ) +
  coord_fixed() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )


# final touches ----
waffle_plot_link_method <- scale_plot_link_method +
  theme_minimal(
    base_size = 9
  )  +
  theme(
    # spacing around text and plot
    plot.title.position = "panel",
    #  plot.caption.position = "plot",
    plot.margin = margin(0, 0, 0, 0),
    # background and grid lines
    plot.background = element_rect(
      fill = bg_col, color = bg_col
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      linewidth = 0.3
    ),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    # axis.text.y = element_blank(),
    # format text with marquee
    plot.title = element_text(
      size = 15,
      face = "bold",
      lineheight = 1.0
    ),
    
    strip.text = element_text(
      size = 8,
      face = "bold",
      lineheight = 0.9,
      margin = margin(t = 2, b = 2)
    ),
    panel.spacing.x = unit(0.1, "lines"),
      axis.text.y = element_blank()
    
  )  + facet_wrap(~variable, ncol = 1, strip.position = "bottom", 
                  labeller = labeller(variable = facet_names_linking_method))


waffle_plot_link_method

ggplot2::ggsave(
  plot = waffle_plot_link_method,
  filename = here::here("Outputs/Figures/waffle_plot_link_method.png")) 



#-----------------------------------------------------------------------------#

# 5. waffle plot for variable testing ----

#-----------------------------------------------------------------------------#

# data ----
case_studies_variable_method_waffle <- case_studies_method_waffle |> 
  slice(4:15)

# font ----
font_add(
  family = "Font Awesome 7",
  regular = "Data/Input/fonts/Font Awesome 7 Free-Solid-900.otf"
)
showtext_auto()
showtext_opts(dpi = 300)

facet_names_variable <- c(
  cwm =
    str_wrap(
      "Were Community Weighted Means used?",
      width = 30
    ),
  effect_of_humans =
    str_wrap(
      "Was the impact of humans considered?",
      width = 30
    ),
  effect_of_climate =
    str_wrap(
      "Was the impact of climate considered?",
      width = 30
    ),
  effect_of_variables =
    str_wrap(
      "Was the impact of any variables considered",
      width = 30
    )
)

# basic plot ----
waffle_chart_variable_method <- ggplot(data = case_studies_variable_method_waffle) +
  geom_pictogram(
    mapping = aes(
      label = value,
      color = value,
      values = count
    ),
    flip = TRUE,
    n_rows = 10,
    size = 4,
    family = "Font Awesome 7"
  ) +
  facet_wrap(~variable,
             nrow = 1,
             strip.position = "bottom",
             labeller = labeller(variable = facet_names_variable)
  )

waffle_chart_variable_method


# add icons ----
icons_plot_variable_method <- waffle_chart_variable_method +
  scale_label_pictogram(
    values = "circle",
    guide = "none"
  )

icons_plot_variable_method

# advanced styling ----
bg_col <- "#FAFAFA"
text_col <- "black"

# colors ----
col_palette_variable_method <-  c("#E84746",  "#BFC2C1", "#509B51")

# vector of T and F
true_false_NA_variable_method <- unique(case_studies_variable_method_waffle$value)

# T and F now have their own colors
names(col_palette_variable_method) <- true_false_NA_variable_method

# plot with new colors and icons
col_plot_variable_method <- icons_plot_variable_method +
  scale_color_manual(
    values = col_palette_variable_method,
    guide = "none"
  )

# Adding style text ----

## title and caption ----
title_variable_method <- "Effects of variables"

# text plot ----
text_plot_variable_method <- col_plot_variable_method +
  labs(
    title = title_variable_method
  )
text_plot_variable_method


# scale plot ----
scale_plot_variable_method <- text_plot_variable_method +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = c(1, 2, 3, 4, 5, 6, 7),              # Every 2 rows
 #   labels = c("10","20","30", "40", "50", "60", "70"), # Converts rows back to "counts" if desired
    limits = c(0, 10)                        # Caps it perfectly at your n_rows height
  ) +
  coord_fixed() + 
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )


# final touches ----
waffle_plot_variable_method <- scale_plot_variable_method +
  theme_minimal(
    base_size = 9
  )  +
  theme(
    # spacing around text and plot
    plot.title.position = "panel",
    #  plot.caption.position = "plot",
    plot.margin = margin(0, 0, 0, 0),
    # background and grid lines
    plot.background = element_rect(
      fill = bg_col, color = bg_col
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      linewidth = 0.3
    ),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    # axis.text.y = element_blank(),
    # format text with marquee
    plot.title = element_text(
      size = 15,
      face = "bold",
      lineheight = 1.0
    ),
    
    strip.text = element_text(
      size = 8,
      face = "bold",
      lineheight = 0.9,
      margin = margin(t = 2, b = 2)
    ),
    panel.spacing.x = unit(0.1, "lines"),
      axis.text.y = element_blank()
    
  )  + facet_wrap(~variable, ncol = 1, strip.position = "bottom",
                  labeller = labeller(variable = facet_names_variable))


waffle_plot_variable_method


ggplot2::ggsave(
  plot = waffle_plot_variable_method,
  filename = here::here("Outputs/Figures/waffle_plot_variable_method.png")) 




#-----------------------------------------------------------------------------#

# 7. combined waffle plot for methods ----

#-----------------------------------------------------------------------------#


# we will combined these three plots -----
waffle_plot_variable_method
waffle_plot_link_method

# color palette ----
col_palette_link_method
col_palette_variable_method

# title and caption ----
title_combined_plot_method <- "Information about methods used for linking plant traits and fossil pollen"


# subtitle ----
st_combined_plot_method <- marquee_glue(
  "The analysis of 62 case studies.\n
{.{col_palette_link_method[[3]]} {names(col_palette_link_method)[[3]]}}, {.{col_palette_link_method[[1]]} {names(col_palette_link_method)[[1]]}}, {.{col_palette_link_method[[2]]} {names(col_palette_link_method)[[2]]}} "
)


# combined plot ----
combined_plot_method <-
  waffle_plot_variable_method +
waffle_plot_link_method +
  plot_layout(widths = c(4, 4)) + # <--- Force widths based on number of categories!
  plot_annotation(
    title = title_combined_plot_method,
    subtitle = st_combined_plot_method,
    theme = theme(
      plot.title = element_text(
        size = 20,
       # width = 1,
        hjust = 0
      ),
      plot.subtitle = element_marquee(
        size = 15,
        hjust = 0,
        width = 1
      )
    )
  ) 

combined_plot_method


ggplot2::ggsave(
  plot = combined_plot_method,
  filename = here::here("Outputs/Figures/combined_plot_method.png")) 
































