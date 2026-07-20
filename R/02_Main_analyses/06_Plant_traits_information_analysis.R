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

#  plant traits information analysis

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
case_studies_traits <- case_studies |> 
  select(id, c(34:53))

# table for waffle chart ----

case_studies_traits_waffle <- case_studies_traits |> 
  select(source_of_fuctional_information_plant_functional_types,
         source_of_functional_information_plant_traits,
         source_of_fuctional_information_other,
         source_of_traits_data_did_the_authors_source_from_database,
         source_of_traits_data_did_the_authors_source_from_published_study,
         source_of_traits_data_did_the_authors_source_from_unpublished_private_data,
         gap_filling_did_the_authors_somehow_gap_fill_missing_trait_data,
  ) |> 
  mutate(
   plant_functional_types = as.character(source_of_fuctional_information_plant_functional_types),
   plant_traits = as.character(source_of_functional_information_plant_traits),
   other_source_of_functional_info = as.character(source_of_fuctional_information_other),
   database = as.character(source_of_traits_data_did_the_authors_source_from_database),
   published_study = as.character(source_of_traits_data_did_the_authors_source_from_published_study),
   unpublished_study = as.character(source_of_traits_data_did_the_authors_source_from_unpublished_private_data),
   gap_filling = as.character(gap_filling_did_the_authors_somehow_gap_fill_missing_trait_data)
   
  ) |> 
  select(8:14)|> 
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
      value == "FASLE" ~ "FALSE",
      value == TRUE ~ "TRUE",
      value == FALSE ~ "FALSE")) |> 
  count(variable, value, name = "count")


case_studies_traits_waffle <- case_studies_traits_waffle |>
  complete(
    variable,
    value = c("TRUE", "FALSE", "Not reported"),
    fill = list(count = 0)
  )


# waffle plot ----

## making a figure ----
waffle_chart_traits = ggplot(case_studies_traits_waffle, aes(fill = value, values = count)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
  facet_wrap(~variable, ncol = 3, strip.position = "bottom") + # varibles names
  scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0))+
  coord_equal()+
  labs(title = "Information about traits extracted from 62 case studies.",
       subtitle = "Neco tu bude...")+
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



waffle_chart_traits


## save it ----
ggplot2::ggsave(
  plot = waffle_chart_traits,
  filename = here::here("Outputs/Figures/waffle_chart_traits.png")) 


#----------------------------------------------------------#
# 3.   Waffle chart for traits -----
#----------------------------------------------------------#

# table for waffle chart ----

case_studies_traits_waffle_2 <- case_studies_traits |> 
  select(id,
         traits_did_the_authors_use_plant_height,
         traits_did_the_authors_use_leaf_morphology,
         traits_did_the_authors_use_sla,
         traits_did_the_authors_use_la,
         traits_did_the_authors_use_leaf_p,
         traits_did_the_authors_use_leaf_n,
         traits_did_the_authors_use_leaf_c,
         traits_did_the_authors_use_ldmc,
         traits_did_the_authors_use_seed_traits,
         traits_did_the_authors_use_wood_traits,
         traits_others
  ) |> 
  mutate(
    ID = as.character(id),
    plant_height = as.character(traits_did_the_authors_use_plant_height),
    leaf_morphology = as.character(traits_did_the_authors_use_leaf_morphology),
    SLA = as.character(traits_did_the_authors_use_sla),
    LA = as.character(traits_did_the_authors_use_la),
    leaf_P = as.character(traits_did_the_authors_use_leaf_p),
    leaf_N = as.character(traits_did_the_authors_use_leaf_n),
    leaf_C = as.character(traits_did_the_authors_use_leaf_c),
    LDMC = as.character(traits_did_the_authors_use_ldmc),
    seed_traits = as.character(traits_did_the_authors_use_seed_traits),
    wood_traits = as.character(traits_did_the_authors_use_wood_traits),
   others = as.character(traits_others)
    
  ) |> 
  select(14:24)|> 
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
      value == "FASLE" ~ "FALSE",
      value == TRUE ~ "TRUE",
      value == FALSE ~ "FALSE"))

# counts
case_studies_traits_waffle_2_n <- case_studies_traits_waffle_2 |> 
  count(variable, value, name = "count") 

# fill
case_studies_traits_waffle_2 <- case_studies_traits_waffle_2 |>
  complete(
    variable,
    value = c("TRUE", "FALSE"),
    fill = list(count = 0)
  )

#-----------------------------------------------------------------------------#

# 4. waffle chart for only leaf traits ----

#-----------------------------------------------------------------------------#


waffle_leaf_traits <- case_studies_traits_waffle_2_n |> 
  slice(1:14)

# font ----
font_add(
  family = "Font Awesome 7",
  regular = "Data/Input/fonts/Font Awesome 7 Free-Solid-900.otf"
)
showtext_auto()
showtext_opts(dpi = 300)


# basic plot leaf traits ----
waffle_chart_leaf_traits <- ggplot(data = waffle_leaf_traits) +
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

waffle_chart_leaf_traits


# add icons to leaf traits waffle ----
icons_plot_leaf_traits <- waffle_chart_leaf_traits +
  scale_label_pictogram(
    values = "leaf",
    guide = "none"
  )

icons_plot_leaf_traits


# advanced styling ----
bg_col <- "#FAFAFA"
text_col <- "black"

display_carto_all(
  n = 2, type = "qualitative"
)


# colors ----
col_palette <- carto_pal(
  n = length(unique(waffle_leaf_traits$value)) + 1,
  name = "Vivid"
)[1:length(unique(waffle_leaf_traits$value))]

# vector of T and F
true_false <- unique(waffle_leaf_traits$value)

# T and F now have their own colors
names(col_palette) <- true_false

# plot with new colors and icons
col_plot_leaf_traits <- icons_plot_leaf_traits +
  scale_color_manual(
    values = col_palette,
    guide = "none"
  )

# Adding style text ----

## title and caption ----
title <- "**Which leaf traits were used in the case studies?**"
cap <- "**Data**: Analyses of 62 case studies | **Graphic**: N. Namesna, inspired by N. Rennie "


source_caption <- function(source, graphic, sep = " | ") {
  caption <- glue::glue(
    "**Data**: {source}{sep}**Graphic**: {graphic}"
  )
  return(caption)
}

cap <- source_caption(
  source = "62 case studies",
  graphic = "N. Namesna, inspired by N. Rennie"
)
cap

## subtitle ----
st <- marquee_glue(
  "The analysis of 62 case studies. It shows how many case studies used a particular leaf trait - 
   {.{col_palette[[2]]} {names(col_palette)[[2]]}}, or not - 
  {.{col_palette[[1]]} {names(col_palette)[[1]]}} in their analyses."
)

# text plot ----
text_plot_leaf_traits <- col_plot_leaf_traits +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  )
text_plot_leaf_traits


# scale plot ----
scale_plot_leaf_traits <- text_plot_leaf_traits +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = c(1, 2, 3, 4, 5, 6, 7),              # Every 2 rows
    labels = c("10","20","30", "40", "50", "60", "70"), # Converts rows back to "counts" if desired
    limits = c(0, 10)                        # Caps it perfectly at your n_rows height
  ) +
  coord_fixed()


# final touches ----
scale_plot_leaf_traits +
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
        size = 15,
       # face = "bold"
      ),
    panel.spacing.x = unit(0.15, "lines"),
    axis.text.y = element_marquee(size = 4)
   
    )


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

# 5. waffle chart for other traits ----

#-----------------------------------------------------------------------------#


waffle_traits_other <- case_studies_traits_waffle_2_n |> 
  slice(15:22)

# font ----
font_add(
  family = "Font Awesome 7",
  regular = "Data/Input/fonts/Font Awesome 7 Free-Solid-900.otf"
)
showtext_auto()
showtext_opts(dpi = 300)


# basic plot leaf traits ----
waffle_chart_traits_other <- ggplot(data = waffle_traits_other) +
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

waffle_chart_traits_other


# add icons to leaf traits waffle ----
icons_plot_traits_other <- waffle_chart_traits_other +
  scale_label_pictogram(
    values = "seedling",
    guide = "none"
  )

icons_plot_traits_other


# advanced styling ----
bg_col <- "#FAFAFA"
text_col <- "black"

display_carto_all(
  n = 2, type = "qualitative"
)


# colors ----
col_palette_2 <- carto_pal(
  n = length(unique(waffle_traits_other$value)) + 1,
  name = "Vivid"
)[1:length(unique(waffle_traits_other$value))]

# vector of T and F
true_false_2 <- unique(waffle_traits_other$value)

# T and F now have their own colors
names(col_palette_2) <- true_false_2

# plot with new colors and icons
col_plot_traits_other <- icons_plot_traits_other +
  scale_color_manual(
    values = col_palette_2,
    guide = "none"
  )

# Adding style text ----

## title and caption ----
title_2 <- "**Which functional traits were used in the case studies?**"
cap_2 <- "**Data**: Analyses of 62 case studies | **Graphic**: N. Namesna, inspired by N. Rennie "


source_caption <- function(source, graphic, sep = " | ") {
  caption <- glue::glue(
    "**Data**: {source}{sep}**Graphic**: {graphic}"
  )
  return(caption)
}

cap_2 <- source_caption(
  source = "62 case studies",
  graphic = "N. Namesna, inspired by N. Rennie"
)
cap_2

## subtitle ----
st_2 <- marquee_glue(
  "The analysis of 62 case studies. It shows how many case studies used a particular plant funtional trait - 
   {.{col_palette[[2]]} {names(col_palette)[[2]]}}, or not - 
  {.{col_palette[[1]]} {names(col_palette)[[1]]}} in their analyses."
)

# text plot ----
text_plot_traits_other <- col_plot_traits_other +
  labs(
    title = title_2,
    subtitle = st_2,
    caption = cap_2
  )
text_plot_traits_other


# scale plot ----
scale_plot_traits_other <- text_plot_traits_other +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = c(1, 2, 3, 4, 5, 6, 7),              # Every 2 rows
    labels = c("10","20","30", "40", "50", "60", "70"), # Converts rows back to "counts" if desired
    limits = c(0, 10)                        # Caps it perfectly at your n_rows height
  ) +
  coord_fixed()


# final touches ----
scale_plot_traits_other +
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
      size = 15,
      # face = "bold"
    ),
    panel.spacing.x = unit(0.15, "lines"),
    axis.text.y = element_marquee(size = 4)
    
  )


# save it ----
# nefunguje to
ggplot2::ggsave(
  plot = scale_plot_traits_other,
  filename = here::here("Outputs/Figures/scale_plot_traits_other.png"),
  width = 10,       # Explicitly set width in inches
  height = 8,       # Explicitly set height in inches
  dpi = 300         # Matches your showtext resolution!
)


#-----------------------------------------------------------------------------#

# 6. waffle chart for traits information ----

#-----------------------------------------------------------------------------#

# data ----
case_studies_traits_waffle

# font ----
font_add(
  family = "Font Awesome 7",
  regular = "Data/Input/fonts/Font Awesome 7 Free-Solid-900.otf"
)
showtext_auto()
showtext_opts(dpi = 300)


# basic plot leaf traits ----
waffle_chart_traits_general <- ggplot(data = case_studies_traits_waffle) +
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

waffle_chart_traits_general


# add icons to leaf traits waffle ----
icons_plot_traits_general <- waffle_chart_traits_general +
  scale_label_pictogram(
    values = "circle",
    guide = "none"
  )

icons_plot_traits_general

# advanced styling ----
bg_col <- "#FAFAFA"
text_col <- "black"

display_carto_all(
  n = 2, type = "qualitative"
)


# colors ----
col_palette_3 <- carto_pal(
  n = length(unique(case_studies_traits_waffle$value)) + 1,
  name = "Vivid"
)[1:length(unique(case_studies_traits_waffle$value))]

# vector of T and F
true_false_NA <- unique(case_studies_traits_waffle$value)

# T and F now have their own colors
names(col_palette_3) <- true_false_NA

# plot with new colors and icons
col_plot_traits_general <- icons_plot_traits_general +
  scale_color_manual(
    values = col_palette_3,
    guide = "none"
  )

# Adding style text ----

## title and caption ----
title_3 <- "**Which information about functional traits were used in the case studies?**"
cap_3 <- "**Data**: Analyses of 62 case studies | **Graphic**: N. Namesna, inspired by N. Rennie "


source_caption <- function(source, graphic, sep = " | ") {
  caption <- glue::glue(
    "**Data**: {source}{sep}**Graphic**: {graphic}"
  )
  return(caption)
}

cap_3 <- source_caption(
  source = "62 case studies",
  graphic = "N. Namesna, inspired by N. Rennie"
)
cap_3

## subtitle ----
st_3 <- marquee_glue(
  "The analysis of 62 case studies. It shows which information about plant funtional traits were used in the case studies:  
  {.{col_palette_3[[3]]} {names(col_palette_3)[[3]]}}, {.{col_palette_3[[2]]} {names(col_palette_3)[[2]]}}, {.{col_palette_3[[1]]} {names(col_palette_3)[[1]]}}. "
)

# text plot ----
text_plot_traits_general <- col_plot_traits_general +
  labs(
    title = title_3,
    subtitle = st_3,
    caption = cap_3
  )
text_plot_traits_general


# scale plot ----
scale_plot_traits_general <- text_plot_traits_general +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = c(1, 2, 3, 4, 5, 6, 7),              # Every 2 rows
    labels = c("10","20","30", "40", "50", "60", "70"), # Converts rows back to "counts" if desired
    limits = c(0, 10)                        # Caps it perfectly at your n_rows height
  ) +
  coord_fixed()


# final touches ----
scale_plot_traits_general +
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
    strip.text = element_textbox(
      size = 3,
      face = "bold"
    ),
    panel.spacing.x = unit(0.15, "lines"),
    axis.text.y = element_marquee(size = 4)
    
  )


# save it ----
# nefunguje to
ggplot2::ggsave(
  plot = scale_plot_traits_general,
  filename = here::here("Outputs/Figures/scale_plot_traits_general.png"),
  width = 10,       # Explicitly set width in inches
  height = 8,       # Explicitly set height in inches
  dpi = 300  )       # Matches your showtext resolution!










