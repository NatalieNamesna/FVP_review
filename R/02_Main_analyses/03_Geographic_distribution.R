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
install.packages("maps")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("ggview")
install.packages("rnaturalearthhires")
library(ggplot2)
library(sf)
library(rnaturalearth)
library(ggview)
library(readxl)
library(viridis)
library(tidyverse)
library(here)
library(ggiraph)
library(ggplot2)
library(dplyr)
library(patchwork)
library(sf)
library(maps)

# Load the table with case studies
case_studies <- readr::read_csv("Data/Processed/case_studies_clean.csv")

#----------------------------------------------------------#
# 2. Map countries  -----
#----------------------------------------------------------#

# table with countries and IDs of case studies ----

case_studies_map_country <- case_studies |> 
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
  case_studies_map_country_n <- case_studies_map_country |>
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
world_sf_country <- world_sf |> 
  left_join(case_studies_map_country_n, by = c("name" = "country"))



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
p1 <- ggplot(world_sf_country, aes(
  x = reorder(name, number_of_studies),
  y = number_of_studies,
  tooltip = name,
  data_id = name,
  fill = number_of_studies
)) +
  geom_col_interactive(data = filter(world_sf_country, !is.na(number_of_studies))) +
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
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title = element_blank(),
    legend.position = "none"
  ) 


## Create the third chart (choropleth) ----
p2 <- ggplot() +
  geom_sf(data = world_sf_country, fill = "lightgrey", color = "lightgrey") +
  geom_sf_interactive(
    data = filter(world_sf_country, !is.na(number_of_studies)),
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

combined_plot_map_countries <-
  p1 / p2 +
  plot_layout(
    heights = c(1.8, 1.5),
    guides = "collect"
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(colour = "black", size = 15),
    legend.text = element_text(colour = "black", size = 10),
  ) +
  plot_annotation(
    title = "Geographic distribution of case studies",
    theme = theme(plot.title = element_text(size = 20, face = "bold"))
  )


combined_plot_map_countries

## save it ---
ggplot2::ggsave(
  plot = combined_plot_map_countries,
  filename = here::here("Outputs/Figures/Combined_plot_map_countries.png"),
  width = 20,
  height = 30) 


#----------------------------------------------------------#
# 2. Map regions  -----
# one big issue: Russia
#----------------------------------------------------------#

# table with regions and IDs of case studies ----

case_studies_map_region <- case_studies |> 
  select(id, region) |> 
  mutate(
    region = str_replace_all(region, "\\n", " "),
    region = str_squish(region),
    region = str_to_title(region)
  ) |>
  separate_longer_delim(region, delim = ",") |>
  mutate(
    region = str_trim(region) 
  ) 


# table with number of studies in each country ----
case_studies_map_region_n <- case_studies_map_region |>
  mutate(
    region = str_replace(region, "Middle East", "Asia"),
    region = str_replace(region, "Australia And Oceania", "Oceania"),
    region = str_replace(region, "Latin America", "South America")
  ) |> 
  group_by(region) |>
  summarise(
    number_of_studies = n(),
    ids = list(id),
    .groups = "drop"
  ) 

# getting map of continents ----

world_sf_continent <- ne_countries(returnclass = "sf") # countries + info about continents
world_sf_continent_2 <- ne_states(returnclass = "sf") # states
world_sf_continent <- world_sf_continent |> 
  filter(!continent %in% c("Antarctica", "Seven seas (open ocean)"))
world_sf_continent_2 <- world_sf_continent_2 |> 
  filter(!continent %in% c("Antarctica", "Seven seas (open ocean)"))


# problem with Russia - splitting into European and Asian part ----
## Download Russia's states/regions (level 1)
russia_states <- ne_states(country = "Russia", returnclass = "sf")

## Define European vs Asian Federal Districts ----
# European: Central, Southern, Northwestern, North Caucasian, Volga
# Asian: Ural, Siberian, Far Eastern

russia_states <- russia_states |> 
  mutate(
    continent = ifelse(
      russia_states$region %in% c("Central", "Southern", "Northwestern", "North Caucasian", "Volga"),
      "Europe", "Asia"
    )
  )

## World without Russia ----
world_without_russia <- world_sf_continent |> 
  filter(admin != "Russia")


# New world with splitted Russia ----
world_sf_continent_3 <- bind_rows(
  world_without_russia,
  russia_states
)

# Join my data with the full world map ----
world_sf_region <- world_sf_continent_3 |> 
  left_join(case_studies_map_region_n, by = c("continent" = "region"))



# create a chart ----

## first chart ----

### drop geometry -> the number of studies will not be number of rows anymore ----
continent_counts <-
  world_sf_region |>
  st_drop_geometry() |>
  distinct(continent, number_of_studies)

### plot ----
p1_region <- ggplot(continent_counts, aes(
  x =  reorder (continent, number_of_studies),
  y = number_of_studies,
  tooltip = continent,
  data_id = continent,
  fill = number_of_studies
)) +
  geom_col_interactive(data = filter(continent_counts, !is.na(number_of_studies))) +
  coord_flip() +
  scale_y_continuous(
    breaks = 0:29,
    limits = c(0, 29),
    expand = expansion(mult = c(0, 0.02))
  ) +
  theme_minimal() +
  theme(
    # axis.title.x = element_blank(),
    # axis.title.y = element_blank(),
    # legend.position = "none"
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title = element_blank(),
    legend.position = "none"
  ) 


## Create the third chart (choropleth) ----
p2_region <- ggplot() +
  geom_sf(data = world_sf_region, fill = "lightgrey", color = "lightgrey") +
  geom_sf_interactive(
    data = filter(world_sf_region, !is.na(number_of_studies)),
    aes(fill = number_of_studies, tooltip = continent, data_id = continent)
  ) +
  coord_sf(crs = st_crs(3857)) +
  theme_void() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "roght"
  ) 

## palette ----
viridis_scale_2 <- scale_fill_viridis_c(
  option = "C",
  direction = -1,
  name = "Number of studies",
  breaks = 0:30,
  limits = c(0, 30)
)

## Combine the plots ----
p1_region <- p1_region + viridis_scale_2
p2_region <- p2_region + viridis_scale_2

combined_plot_map_continents <-
  p1_region / p2_region +
  plot_layout(
    heights = c(1, 2),
    guides = "collect"
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(colour = "black", size = 15),
    legend.text = element_text(colour = "black", size = 5),
  ) +
  plot_annotation(
    title = "Geographic distribution of case studies - continents",
    theme = theme(plot.title = element_text(size = 20, face = "bold"))
  )


combined_plot_map_continents

## save it ---
ggplot2::ggsave(
  plot = combined_plot_map_continents,
  filename = here::here("Outputs/Figures/Combined_plot_map_continents.png"),
  width = 20,
  height = 30) 


#----------------------------------------------------------#
# 3. Map regions, histogram as legend  -----
#----------------------------------------------------------#

# journal data ----
case_studies_map_region # id of study + continent
case_studies_map_region_n # continent with number of studies
world_sf_region # world map connected with case_studies_map_region_n

# getting map of continents ----
world_sf_continent <- ne_countries(returnclass = "sf")
world_sf_continent <- world_sf_continent |> 
  filter(!continent %in% c("Antarctica", "Seven seas (open ocean)"))

# plot map ----
ggplot() +
  geom_sf(data = world_sf_continent, fill = "#DEB887", color = "white", linewidth = 0.25) 
# Albers projection
#coord_sf(crs = st_crs("ESRI:102003"))


# Get x-axis limits of the bounding box for the state data ----
bbox <- st_bbox(world_sf_continent)

# expand the map to fit the legend ----
xlim_expanded <- c(
  bbox["xmin"],
  bbox["xmax"] + 0.5 * (bbox["xmax"] - bbox["xmin"])
)

# y axis ----
y_expand <-  0.3 * (bbox["ymax"] - bbox["ymin"])

ylim_expanded <- c(
  bbox["ymin"] - y_expand,
  bbox["ymax"] + y_expand
)

# show the expanded map ----
ggplot() +
  geom_sf(data = world_sf_continent, fill = "#DEB887", color = "white", linewidth = 0.25) +
  coord_sf( xlim = xlim_expanded, ylim = ylim_expanded)




# the map with nice legend ----

## legend ----
legend_plot <- ggplot(
  case_studies_map_region_n,
  aes(
    y = reorder(region, number_of_studies),
    x = number_of_studies,
    fill = number_of_studies
  )
) +
  geom_col(width = 0.8, colour = "white") +
  
  scale_fill_distiller(
    palette = "YlGnBu",
    direction = 1,
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = c(1, 5, 10, 15, 20, 25, 29),
    limits = c(0, 29),
    expand = c(0, 0)
  ) +
  

  labs(
    x = "Number of studies",
    y = NULL
  ) +
  
  theme_minimal(base_size = 8) +
  theme(
    axis.text.y = element_text(size = 8, face = "bold"),
    axis.text.x = element_text(, size = 7),
    axis.ticks.x = element_line(),
    axis.line.x = element_line(colour = "black"),
    axis.title.x = element_text(face = "bold", size = 10),
    panel.grid = element_blank(),
    plot.margin = margin(0,0,0,0),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background  = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA)
    )
  

legend_plot

## plot map ----
map_continents <- ggplot() +
  # Add counties filled with unemployment levels
  geom_sf(
    data = world_sf_region, aes(fill = number_of_studies), color = NA, linewidth = 0
  ) +
  # don't actually show the legend
  scale_fill_stepsn(
    colours = scales::brewer_pal(palette = "YlGnBu")(9),
    breaks = c(1, 5, 10, 15, 20, 25, 29),
    labels = c("1", "5", "10", "15", "20", "25", "29"),
    limits = c(1, 29), 
    guide = "none"
  ) +
  # Yay labels
  labs(
    title = "Geographic distribution of case studies",
    subtitle = "...",
    caption = "Inspired by: Andrew Heiss, TidyTuesday"
  ) +
  # Use new x-axis limits
  coord_sf( xlim = xlim_expanded, ylim = ylim_expanded) 

## add the legend to the map ----

combined_plot_map_regions_2 <- map_continents + 
  inset_element(legend_plot, left = 0.68,
                bottom = 0.05,
                right = 0.98,
                top = 0.8)

combined_plot_map_regions_2 


## save it ----

ggplot2::ggsave(
  plot = combined_plot_map_regions_2,
  filename = here::here("Outputs/Figures/Combined_plot_map_continents_2.png")) 


#----------------------------------------------------------#
# 4. Map countries, histogram as legend  -----
#----------------------------------------------------------#

# journal data ----
case_studies_map_country
case_studies_map_country_n
world_sf_region

# getting map of countries ----

world_sf_country


# plot map ----
ggplot() +
  geom_sf(data = world_sf_country, fill = "#DEB887", color = "white", linewidth = 0.25) 
# Albers projection
#coord_sf(crs = st_crs("ESRI:102003"))


# Get x-axis limits of the bounding box for the state data ----
bbox_2 <- st_bbox(world_sf_country)

# expand the map to fit the legend ----
xlim_expanded_2 <- c(
  bbox_2["xmin"],
  bbox_2["xmax"] + 0.7 * (bbox_2["xmax"] - bbox_2["xmin"])
)

# y-axis ----
y_expand_2 <-  0.5 * (bbox_2["ymax"] - bbox_2["ymin"])

ylim_expanded_2 <- c(
  bbox_2["ymin"] - y_expand_2,
  bbox_2["ymax"] + y_expand_2
)

# show the expanded map ----
ggplot() +
  geom_sf(data = world_sf_country, fill = "#DEB887", color = "white", linewidth = 0.25) +
  coord_sf( xlim = xlim_expanded_2)

# the map with nice legend ----

case_studies_map_country_n_2 <- case_studies_map_country_n |> 
  slice(-1)

## legend ----
legend_plot_2 <- ggplot(
  case_studies_map_country_n_2,
  aes(
    y = reorder(country, number_of_studies),
    x = number_of_studies,
    fill = number_of_studies
  )
) +
  geom_col(width = 0.8, colour = "white") +
  
  scale_fill_distiller(
    palette = "YlGnBu",
    direction = 1,
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = c(1:13),
    limits = c(0, 13),
    expand = c(0, 0)
  ) +
  
  
  labs(
    x = "Number of studies",
    y = NULL
  ) +
  
  theme_minimal(base_size = 8) +
  theme(
    axis.text.y = element_text(size = 7, face = "bold"),
    axis.text.x = element_text(size = 7),
    axis.ticks.x = element_line(),
    axis.line.x = element_line(colour = "black"),
    axis.title.x = element_text(face = "bold", size = 8),
    panel.grid = element_blank(),
    plot.margin = margin(0,0,0,0),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background  = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA)
  )


legend_plot_2

## plot map ----
map_countries <- ggplot() +
  # Add counties filled with unemployment levels
  geom_sf(
    data = world_sf_country, aes(fill = number_of_studies), color = NA, linewidth = 0
  ) +
  # don't actually show the legend
  scale_fill_stepsn(
    colours = scales::brewer_pal(palette = "YlGnBu")(9),
    breaks = c(1:13),
    # labels = c("1", "5", "10", "15", "20", "25", "29"),
    limits = c(1, 13), 
    guide = "none"
  ) +
  # Yay labels
  labs(
    title = "Geographic distribution of case studies - countries",
    subtitle = "...",
    caption = "Inspired by: Andrew Heiss, TidyTuesday"
  ) +
  # Use new x-axis limits
  coord_sf( xlim = xlim_expanded_2, ylim = ylim_expanded_2) 

## add the legend to the map ----

combined_plot_map_countries_2 <- map_countries + 
  inset_element(legend_plot_2, left = 0.6,
                bottom = 0.01,
                right = 0.99,
                top = 0.99)

combined_plot_map_countries_2 


## save it ----

ggplot2::ggsave(
  plot = combined_plot_map_countries_2,
  filename = here::here("Outputs/Figures/Combined_plot_map_countries_2.png")) 

















