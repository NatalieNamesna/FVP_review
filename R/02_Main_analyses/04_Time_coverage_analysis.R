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

#  Time coverage of case studies

#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#

install.packages("remotes")
remotes::install_github("hrbrmstr/ggalt")
library(ggalt)
library(ggplot2)
library(tidyverse)
library(here)
library(extrafont)

# Load the table with case studies

case_studies <- readr::read_csv("Data/Processed/case_studies_clean.csv")


# Data ----

case_studies_time_coverage <- case_studies |> 
  select(id, first_author, time_period_period_young_kyr, time_period_period_old_kyr, region, source_of_pollen_data_which_database, source_of_fuctional_information_plant_functional_types ) |> 
  mutate(
    case_study = id,
    time_young_kyr = time_period_period_young_kyr,
    time_old_kyr = time_period_period_old_kyr,
    region = region, 
    pollen_database = source_of_pollen_data_which_database,
    pft = source_of_fuctional_information_plant_functional_types
    
  ) |> 
  select(case_study, time_young_kyr, time_old_kyr, first_author, region, pollen_database, pft )  |> 
  mutate(
    region = str_replace_all(region, "\\n", " "),
    region = str_squish(region),
    region = str_to_title(region)
  ) |>
  mutate(pollen_database = pollen_database, 
         pollen_database = str_replace_all(pollen_database, "\\n", ""),
         pollen_database = str_trim(pollen_database),           
         pollen_database = str_squish(pollen_database),         
         pollen_database = str_to_lower(pollen_database),
         pollen_database = str_to_title(pollen_database), 
         pollen_database = str_replace(pollen_database, "Pangea", "Pangaea"),
         pollen_database = str_replace(pollen_database, "Se Australian Pollen Database", "SE Australian Pollen Database"))

#----------------------------------------------------------#
# 2. Dumbbell plot for time coverage + geographic location  -----
#----------------------------------------------------------#

# making one category from those studies that have more than one continent -> coloring ----
case_studies_time_coverage_region <-
  case_studies_time_coverage |>
  mutate(
    region_plot = case_when(
      str_detect(region, ",") ~ "Multiple regions",
      TRUE ~ region
    )
  )

# sort case studies to have them in groups based on their geographic region ----
case_studies_time_coverage_region <-
  case_studies_time_coverage_region |>
  arrange(region_plot, desc(time_old_kyr)) |>
  mutate(
    case_study = factor(case_study, levels = rev(case_study))
  )

# Dumbbell plot with ggalt and geom_dumbbell ----

plot_case_studies_time_coverage_region <- ggplot(case_studies_time_coverage_region, aes(y = case_study)) +
 # Pleistocene
  geom_rect(
    aes(xmin = 11.7, xmax = 130,
        ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "#8A9497",
    alpha = 0.15
  ) +
  annotate("text",
           x = 123,
           y = 64,
           label = "Pleistocene",
           vjust = 1,
           fontface = "bold",
           size = 4
           # ,angle = 90
           ) +
   
  ## LGM
  geom_rect(
    aes(xmin = 19, xmax = 26.5,
        ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "#BFEFFF",
    alpha = 0.20
  ) +
  annotate("text",
           x = 23,
           y = 64,
           label = "LGM",
           vjust = 1,
           fontface = "bold",
           size = 4
          # , angle = 90
           ) +
  ## Holocene
  geom_rect(
    aes(xmin = 0, xmax = 11.7,
        ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "#D5D5D5",
    alpha = 0.20
  ) +
  annotate("text",
           x = 6,
           y = 64,
           label = "Holocene",
           vjust = 1,
           fontface = "bold",
           size = 4
          # ,angle = 90
           ) +
  # dumbbell
  geom_segment(
    aes(
      x = time_young_kyr,
      xend = time_old_kyr,
      yend = case_study,
      colour = region_plot
    ),
    linewidth = 1.5
  ) +
  
  geom_point(
    aes(
      x = time_young_kyr,
      colour = region_plot
    ),
    size = 3
  ) +
  
  geom_point(
    aes(
      x = time_old_kyr,
      colour = region_plot
    ),
    size = 3
  ) +
  
  scale_colour_manual(
    name = "Region",
    values = c(
      "Africa" = "#E69F00",
      "Asia" = "#0072B2",
      "Europe" = "#009E73",
      "North America" = "#CC79A7",
      "South America" = "#D55E00",
      "Latin America" = "#56B4E9",
      "Australia And Oceania" = "#F0E442",
      "Multiple regions" = "grey40"
    )
  ) +
  labs(
    title = "Time coverage of each case study + their geographic location",
    x = "Time (kyr)",
    y = "Individual case studies"
  ) +
  scale_x_continuous(
    trans = "reverse",
    limits = c(0, 130),
    breaks = seq(0, 130, by = 10)
  ) +
  scale_y_discrete(position = "right") +
  coord_cartesian(xlim = c(0, 130), expand = TRUE) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text( size = 15),
    legend.position = "right",
    legend.title = element_text("Region", size = 16, face = "bold"),
    legend.text = element_text(size = 15),
    plot.title = element_text(
        face = "bold", size = 20, vjust = 1, margin=margin(0,0,10,0)
    ),
    plot.title.position = "plot",
    plot.margin = margin(2,2,2,1, "cm")
  )


plot_case_studies_time_coverage_region




# save it ----
ggplot2::ggsave(
  plot = plot_case_studies_time_coverage_region,
  filename = here::here("OUtputs/Figures/plot_case_studies_time_coverage_region.png")) 

       



#----------------------------------------------------------#
# 3. Dumbbell plot for time coverage + database -----
#----------------------------------------------------------#

# separate multiple databases in to more rows ----
case_studies_time_coverage_database <- case_studies_time_coverage |> 
  separate_longer_delim(pollen_database, delim = ",") |> 
  mutate(pollen_database = str_trim(pollen_database))

# new dataset for points as name of database ---
database_points <- case_studies_time_coverage_database |> 
  select(case_study, pollen_database) |> 
  mutate(Database = pollen_database)
  
# position on x axis
database_points <-
  database_points |>
  group_by(case_study) |>
  mutate(
    xpos = -2 - (row_number()-1)*2
  )


# Dumbbell plot with ggalt and geom_dumbbell ----

plot_case_studies_time_coverage_database <- ggplot(case_studies_time_coverage_database, aes(y = case_study)) +
  # Pleistocene
  geom_rect(
    aes(xmin = 11.7, xmax = 130,
        ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "#8A9497",
    alpha = 0.15
  ) +
  annotate("text",
           x = 123,
           y = 64,
           label = "Pleistocene",
           vjust = 1,
           fontface = "bold",
           size = 4
           # ,angle = 90
  ) +
  
  ## LGM
  geom_rect(
    aes(xmin = 19, xmax = 26.5,
        ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "#BFEFFF",
    alpha = 0.20
  ) +
  annotate("text",
           x = 23,
           y = 64,
           label = "LGM",
           vjust = 1,
           fontface = "bold",
           size = 4
           # , angle = 90
  ) +
  ## Holocene
  geom_rect(
    aes(xmin = 0, xmax = 11.7,
        ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "#D5D5D5",
    alpha = 0.20
  ) +
  annotate("text",
           x = 6,
           y = 64,
           label = "Holocene",
           vjust = 1,
           fontface = "bold",
           size = 4
           # ,angle = 90
  ) +
  # dumbbell
  geom_segment(
    aes(
      x = time_young_kyr,
      xend = time_old_kyr,
      yend = case_study,
    #  colour = region_plot
    ),
    linewidth = 1.5
  ) +
  
  geom_point(
    aes(
      x = time_young_kyr,
     # colour = region_plot
    ),
    size = 3
  ) +
  
  geom_point(
    aes(
      x = time_old_kyr,
     # colour = region_plot
    ),
    size = 3
  ) +
  geom_point(
    data = database_points,
    aes(
      x = xpos,
      y = case_study,
      colour = Database
    ),
    size = 3
  ) +
  labs(
    title = "Time coverage of each case study + pollen database",
    x = "Time (kyr)",
    y = "Individual case studies"
  ) +
  scale_x_continuous(
    trans = "reverse",
   # limits = c(0, 130),
    breaks = seq(0, 130, by = 10)
  ) +
  scale_y_discrete(position = "right") +
  coord_cartesian(xlim = c(-8, 130), clip = "off") +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text( size = 15),
    legend.position = "right",
    legend.title = element_text("Database", size = 16, face = "bold"),
    legend.text = element_text(size = 15),
    plot.title = element_text(
      face = "bold", size = 20, vjust = 1, margin=margin(0,0,10,0)
    ),
    plot.title.position = "plot",
    plot.margin = margin(1,1,1,1, "cm")
  )


plot_case_studies_time_coverage_database




# save it ----
ggplot2::ggsave(
  plot = plot_case_studies_time_coverage_database,
  filename = here::here("OUtputs/Figures/plot_case_studies_time_coverage_database.png")) 







#----------------------------------------------------------#
# 4. Dumbbell plot for time coverage + pft -----
#----------------------------------------------------------#









































