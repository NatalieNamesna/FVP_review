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
library(showtext)
library(ggtext)
library(waffle)
library(ggwaffle)
library(tidyverse)
library(ggplot2)
library(ggtext)
library(sf) 
library(here)

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








