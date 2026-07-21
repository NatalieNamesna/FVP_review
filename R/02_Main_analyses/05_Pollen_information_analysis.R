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

#  Pollen information analysis

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

# Load the table with case studies

case_studies <- readr::read_csv("Data/Processed/case_studies_clean.csv")


#----------------------------------------------------------#
# 2.   Waffle chart -----
#----------------------------------------------------------#

# table for all pollen information ----
case_studies_pollen <- case_studies |> 
  select(id, c(20:33))

# table for waffle chart ----

case_studies_pollen_waffle <- case_studies_pollen |> 
  select(method_of_choosing_pollen_data_did_the_authors_have_any_specific_method_for_selecting_records, 
         method_of_choosing_pollen_data_did_the_authors_select_records_based_on_the_amount_of_information,
         method_of_choosing_pollen_data_did_the_authors_select_records_based_on_their_length,
         method_of_choosing_pollen_data_did_the_authors_select_records_based_on_the_presence_of_specific_taxa_life_forms,
         source_of_pollen_data_experimental_study,
         source_of_pollen_data_did_the_authors_source_from_database,
         source_of_pollen_data_did_the_authors_source_from_published_study,
         source_of_pollen_data_did_the_authors_source_from_unpublished_private_data,
         pollen_nomenclature_harmonisation,
         pollen_counts_corrections,
         age_depth_recalculation) |> 
  mutate(
    choosing_method = as.character(method_of_choosing_pollen_data_did_the_authors_have_any_specific_method_for_selecting_records),
    choosing_method_info = as.character(method_of_choosing_pollen_data_did_the_authors_select_records_based_on_the_amount_of_information),
    choosing_method_length = as.character(method_of_choosing_pollen_data_did_the_authors_select_records_based_on_their_length),
    choosing_method_taxa = as.character(method_of_choosing_pollen_data_did_the_authors_select_records_based_on_the_presence_of_specific_taxa_life_forms),
    experimental_study = as.character(source_of_pollen_data_experimental_study),
    database = as.character(source_of_pollen_data_did_the_authors_source_from_database),
    published_study = as.character(source_of_pollen_data_did_the_authors_source_from_published_study),
    unpublished_study = as.character(source_of_pollen_data_did_the_authors_source_from_unpublished_private_data),
    harmonisation = as.character(pollen_nomenclature_harmonisation),
    counts_correction = as.character(pollen_counts_corrections),
    age_depth_recalculation = as.character(age_depth_recalculation)
  ) |> 
  select(c(12:21)) |> 
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "value"
  ) |> 
  mutate(
    value = case_when(
      is.na(value) ~ "Not reported",
      value == "NA" ~ "Not reported",
      value == "TRUE\r\n" ~ "Not reported",
      value == TRUE ~ "TRUE",
      value == FALSE ~ "FALSE")) |> 
  count(variable, value, name = "count")


case_studies_pollen_waffle <- case_studies_pollen_waffle |>
  complete(
    variable,
    value = c("TRUE", "FALSE", "Not reported"),
    fill = list(count = 0)
  )


# waffle plot ----

## making a figure ----
waffle_chart_pollen = ggplot(case_studies_pollen_waffle, aes(fill = value, values = count)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
  facet_wrap(~variable, ncol = 5, strip.position = "bottom") + # varibles names
  scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0))+
  coord_equal()+
  labs(title = "Neco jako: Pollen information extracted from 62 case studies.",
       subtitle = "Jeste nevim...")+
  theme_minimal()+
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text( size=8),
    axis.text.y = element_text( size=8),
    strip.text = element_text(size = 8),
    
    # Legend
    legend.title = element_blank(),
    legend.spacing = unit(1, 'cm'),
    legend.key.height= unit(0.6, 'cm'),
    legend.key.width= unit(0.6, 'cm'),
    legend.text = element_text(colour = "black", size = 8),
    
    
    # Title
    plot.title.position = "plot",
    plot.title = element_textbox(margin = margin(30, 0, 10, 0),
                                 size = 20,
                                 face = "bold",
                                 width = unit(55, "lines")),
    
    # Sub-title
    plot.subtitle = element_text(margin = margin(10, 0, 20, 0),
                                 size = 10,
                                 color = "grey15"),

    plot.background = element_rect(color="white", fill="white"),
    plot.margin = margin(40, 60, 40, 60)
  ) 



waffle_chart_pollen


## save it ----
ggplot2::ggsave(
  plot = waffle_chart_pollen,
  filename = here::here("Outputs/Figures/waffle_chart_pollen.png")) 





#-----------------------------------------------------------------------------#

# 3. better waffle chart for pollen information ----

#-----------------------------------------------------------------------------#

# data ----
case_studies_pollen_waffle

# font ----
font_add(
  family = "Font Awesome 7",
  regular = "Data/Input/fonts/Font Awesome 7 Free-Solid-900.otf"
)
showtext_auto()
showtext_opts(dpi = 300)


# basic plot leaf traits ----
waffle_chart_pollen <- ggplot(data = case_studies_pollen_waffle) +
  geom_pictogram(
    mapping = aes(
      label = value,
      color = value,
      values = count
    ),
    flip = TRUE,
    n_rows = 10,
    size = 1,
    family = "Font Awesome 7"
  ) +
  facet_wrap(~variable,
             nrow = 1,
             strip.position = "bottom"
  )

waffle_chart_pollen


# add icons to leaf traits waffle ----
icons_plot_pollen <- waffle_chart_pollen +
  scale_label_pictogram(
    values = "circle",
    guide = "none"
  )

icons_plot_pollen


# advanced styling ----
bg_col <- "#FAFAFA"
text_col <- "black"

display_carto_all(
  n = 2, type = "qualitative"
)

# color ----
col_palette_pollen <- carto_pal(
  n = length(unique(case_studies_pollen_waffle$value)) + 1,
  name = "Vivid"
)[1:length(unique(case_studies_pollen_waffle$value))]

# vector of T and F
true_false_NA <- unique(case_studies_pollen_waffle$value)

# T and F now have their own colors
names(col_palette_pollen) <- true_false_NA

# plot with new colors and icons
col_plot_pollen <- icons_plot_pollen +
  scale_color_manual(
    values = col_palette_pollen,
    guide = "none"
  )

# Adding style text ----

## title and caption ----
title_4 <- "**Which information about pollen were used in the case studies?**"
cap_4 <- "**Data**: Analyses of 62 case studies | **Graphic**: N. Namesna, inspired by N. Rennie "


source_caption <- function(source, graphic, sep = " | ") {
  caption <- glue::glue(
    "**Data**: {source}{sep}**Graphic**: {graphic}"
  )
  return(caption)
}

cap_4 <- source_caption(
  source = "62 case studies",
  graphic = "N. Namesna, inspired by N. Rennie"
)
cap_4

## subtitle ----
st_4 <- marquee_glue(
  "The analysis of 62 case studies. It shows which information about pollen were used in the case studies:  
  {.{col_palette_3[[3]]} {names(col_palette_3)[[3]]}}, {.{col_palette_3[[2]]} {names(col_palette_3)[[2]]}}, {.{col_palette_3[[1]]} {names(col_palette_3)[[1]]}}. "
)


# text plot ----
text_plot_pollen <- col_plot_pollen +
  labs(
    title = title_4,
    subtitle = st_4,
    caption = cap_4
  )
text_plot_pollen


# scale plot ----
scale_plot_pollen <- text_plot_pollen +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = c(1, 2, 3, 4, 5, 6, 7),              # Every 2 rows
    labels = c("10","20","30", "40", "50", "60", "70"), # Converts rows back to "counts" if desired
    limits = c(0, 10)                        # Caps it perfectly at your n_rows height
  ) +
  coord_fixed()


# final touches ----
scale_plot_pollen +
  theme_minimal(
    base_size = 9
  )  +
  theme(
    # spacing around text and plot
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(10, 20, 10, 20),
    # background and grid lines
    plot.background = element_rect(
      fill = bg_col, color = bg_col
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      linewidth = 0.4
    ),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    # format text with marquee
    plot.title = element_marquee(
      color = text_col,
      width = 1,
      size = 30
    ),
    plot.subtitle = element_marquee(
      color = text_col,
      width = 1,
      size = 20
    ),
    plot.caption = element_marquee(
      hjust = 0,
      lineheight = 0.5,
      size = 15,
      margin = margin(t = 5)
    ),
    strip.text = element_marquee(
      size = 13,
      # face = "bold"
    ),
    panel.spacing.x = unit(0.15, "lines"),
    axis.text.y = element_marquee(size = 4)
    
  ) + facet_wrap(~variable, ncol = 5, strip.position = "bottom") 


# save it ----
# nefunguje to
ggplot2::ggsave(
  plot = scale_plot_leaf_traits,
  filename = here::here("Outputs/Figures/scale_plot_leaf_traits.png"),
  width = 10,       # Explicitly set width in inches
  height = 8,       # Explicitly set height in inches
  dpi = 300         # Matches your showtext resolution!
)



#-----------------------------------------------------------------------------#

# 4. waffle plot for choosing method ----

#-----------------------------------------------------------------------------#

# data ----
case_studies_pollen_method_waffle <- case_studies_pollen_waffle |> 
  slice(1:12)





















