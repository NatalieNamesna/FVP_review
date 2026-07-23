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


#----------------------------------------------------------#
# 2. Dumbbell plot for time coverage  -----
#----------------------------------------------------------#

case_studies_time_coverage <- case_studies |> 
  select(id, first_author, time_period_period_young_kyr, time_period_period_old_kyr, region) |> 
  mutate(
    case_study = id,
    time_young_kyr = time_period_period_young_kyr,
    time_old_kyr = time_period_period_old_kyr,
    region = region
  ) |> 
  select(case_study, time_young_kyr, time_old_kyr, first_author, region)  |> 
  mutate(
    region = str_replace_all(region, "\\n", " "),
    region = str_squish(region),
    region = str_to_title(region)
  ) 



case_studies_time_coverage_2 <-
  case_studies_time_coverage |>
  mutate(
    region_plot = case_when(
      str_detect(region, ",") ~ "Multiple regions",
      TRUE ~ region
    )
  )


case_studies_time_coverage_2 <-
  case_studies_time_coverage_2 |>
  arrange(region_plot, desc(time_old_kyr)) |>
  mutate(
    case_study = factor(case_study, levels = rev(case_study))
  )


# adding labels to dumbell # Dumbbell plot with geom_segment and geom_point ----
ggplot(case_studies_time_coverage) +
  geom_segment(aes(x = time_young_kyr, xend = time_old_kyr,
                   y = first_author, yend = first_author)) +
  geom_point(aes(x = time_young_kyr, y = first_author), size = 3) +
  geom_point(aes(x = time_old_kyr, y = first_author), size = 3)


# Dumbbell plot with ggalt and geom_dumbbell ----

plot_case_studies_time_coverage <- ggplot(case_studies_time_coverage_2, aes(y = case_study)) +
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
           size = 1.5
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
           size = 1.5
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
           size = 1.5
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
    axis.text.x = element_text(size = 3),
    axis.title.x = element_text(size = 5),
    axis.title.y = element_text( size = 5),
    legend.position = "right",
    legend.title = element_text("Region", size = 6, face = "bold"),
    legend.text = element_text(size = 5),
    plot.title = element_text(
        face = "bold", size = 8, vjust = 1, margin=margin(0,0,10,0)
    ),
    plot.title.position = "plot",
    plot.margin = margin(2,2,2,1, "cm")
  )


plot_case_studies_time_coverage




# save it ----
ggplot2::ggsave(
  plot = plot_case_studies_time_coverage,
  filename = here::here("OUtputs/Figures/plot_case_studies_time_coverage.png")) 

       



#----------------------------------------------------------#
# 3. Dumbbell plot for time coverage + region -----
#----------------------------------------------------------#

segments <- segments |>
  group_by(case_study) |>
  mutate(
    n_regions = n(),
    part = row_number()
  ) |>
  ungroup()















































