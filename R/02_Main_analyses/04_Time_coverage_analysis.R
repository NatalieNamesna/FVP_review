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

# Load the table with case studies

case_studies <- readr::read_csv("Data/Processed/case_studies_clean.csv")


#----------------------------------------------------------#
# 2. Dumbbell plot for time coverage  -----
#----------------------------------------------------------#

case_studies_time_coverage <- case_studies |> 
  select(id, first_author, time_period_period_young_kyr, time_period_period_old_kyr) |> 
  mutate(
    case_study = id,
    time_young_kyr = time_period_period_young_kyr,
    time_old_kyr = time_period_period_old_kyr
  ) |> 
  select(case_study, time_young_kyr, time_old_kyr, first_author)


# Dumbbell plot with geom_segment and geom_point ----
ggplot(case_studies_time_coverage) +
  geom_segment(aes(x = time_young_kyr, xend = time_old_kyr,
                   y = first_author, yend = first_author)) +
  geom_point(aes(x = time_young_kyr, y = first_author), size = 3) +
  geom_point(aes(x = time_old_kyr, y = first_author), size = 3)


# Dumbbell plot with ggalt and geom_dumbbell ----

plot_case_studies_time_coverage <- ggplot(case_studies_time_coverage, aes(y = reorder(case_study, time_old_kyr))) +
 # Pleistocene
  geom_rect(
    aes(xmin = 11.7, xmax = 130,
        ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "#8FBC8F",
    alpha = 0.15
  ) +
  annotate("text",
           x = 75,
           y = 2,
           label = "Pleistocene",
           vjust = 1,
           fontface = "bold",
           size = 4) +
   
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
           y = 2,
           label = "LGM",
           vjust = 1,
           fontface = "bold",
           size = 4) +
  ## Holocene
  geom_rect(
    aes(xmin = 0, xmax = 11.7,
        ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "#C1FFC1",
    alpha = 0.20
  ) +
  annotate("text",
           x = 5.8,
           y = 2,
           label = "Holocene",
           vjust = 1,
           fontface = "bold",
           size = 4) +
  # dumbbell
  geom_dumbbell(aes(x = time_young_kyr, xend = time_old_kyr),
                color = "black",
                size = 1, dot_guide = FALSE, 
                size_x = 3,  size_xend = 3,
                colour_x = "black", colour_xend = "black") +
  labs(
    title = "Time coverage of each case study",
    x = "Time_kyr",
    y = "Case_study_id",
  )+
  scale_x_continuous(limits = c(0, 130), breaks = seq(0, 130, by = 5)) +
  scale_y_discrete(expand = expansion(add = 0.5)) +
  coord_cartesian(xlim = c(0, 130), expand = TRUE) +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.y = element_text(margin = margin(r= -10)),
    legend.position = "none",
    legend.title = element_blank(),
    plot.title = element_text(
      face = "bold",
      margin = margin(b = 10)
    ),
    plot.title.position = "plot",
    plot.margin = margin(20, 15, 20, 15)
  )

plot_case_studies_time_coverage

# save it ----
ggplot2::ggsave(
  plot = plot_case_studies_time_coverage,
  filename = here::here("OUtputs/Figures/plot_case_studies_time_coverage.png")) 

       
               

















