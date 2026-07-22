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

#  Geographic distribution of case studies - map analysis

#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#

# packages
install.packages("ggiraph")
install.packages("patchwork")
install.packages("sf")
install.packages("viridis")
library(viridis)
library(tidyverse)
library(here)
library(ggiraph)
library(ggplot2)
library(dplyr)
library(patchwork)
library(sf)

# Load the table with case studies
case_studies <- readr::read_csv("Data/Processed/case_studies_clean.csv")

#----------------------------------------------------------#
# 2. Map  -----
#----------------------------------------------------------#

# table with countries and IDs of case studies ----

case_studies_map <- case_studies |> 
  select(id, country) |> 
  mutate(
    country = str_replace_all(country, "\\n", " "),
    country = str_squish(country),
    country = str_to_title(country),
    country = str_replace(country, "Usa", "United States of America"),
    country = if_else(country == "Mediterranean Region", NA, country)
  ) |>
  separate_longer_delim(country, delim = ",") |>
  mutate(
    country = str_trim(country) 
  ) 

# table with number of studies in each country ----
  case_studies_map_n <- case_studies_map |>
  group_by(country) |>
  summarise(
    number_of_studies = n(),
    ids = list(id),
    .groups = "drop"
  )


# Read the full world map ----
world_sf <- read_sf("https://raw.githubusercontent.com/holtzy/R-graph-gallery/master/DATA/world.geojson")
world_sf <- world_sf |> 
  filter(!name %in% c("Antarctica", "Greenland"))


# Join my data with the full world map ----
world_sf <- world_sf |> 
  left_join(case_studies_map_n, by = c("name" = "country"))



# create a chart ----

## palette ----
viridis_scale <- scale_fill_viridis_c(
  option = "C",
  direction = -1,
  name = "Number of studies",
  breaks = 0:13,
  limits = c(0, 13)
)

## first chart ----
p1 <- ggplot(world_sf, aes(
  x = reorder(name, number_of_studies),
  y = number_of_studies,
  tooltip = name,
  data_id = name,
  fill = number_of_studies
)) +
  geom_col_interactive(data = filter(world_sf, !is.na(number_of_studies))) +
  coord_flip() +
  scale_y_continuous(
    breaks = 0:13,
    limits = c(0, 13),
    expand = expansion(mult = c(0, 0.02))
  ) +
  theme_minimal() +
  theme(
   # axis.title.x = element_blank(),
   # axis.title.y = element_blank(),
   # legend.position = "none"
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.title = element_blank(),
    legend.position = "none"
  ) 


## Create the third chart (choropleth) ----
p2 <- ggplot() +
  geom_sf(data = world_sf, fill = "lightgrey", color = "lightgrey") +
  geom_sf_interactive(
    data = filter(world_sf, !is.na(number_of_studies)),
    aes(fill = number_of_studies, tooltip = name, data_id = name)
  ) +
  coord_sf(crs = st_crs(3857)) +
  theme_void() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  ) 

## Combine the plots ----
p1 <- p1 + viridis_scale
p2 <- p2 + viridis_scale

combined_plot <-
  p1 / p2 +
  plot_layout(
    heights = c(1.8, 1.5),
    guides = "collect"
  ) &
  theme(
    legend.position = "right",
    legend.title = element_text(colour = "black", size = 15, face = "bold"),
    legend.text = element_text(colour = "black", size = 10),
  )


combined_plot

## 










