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
      value == TRUE ~ "TRUE",
      value == FALSE ~ "FALSE")) |> 
  count(variable, value, name = "count")


case_studies_pollen_waffle <- case_studies_pollen_waffle |>
  complete(
    variable,
    value = c("TRUE", "FALSE", "Not reported"),
    fill = list(count = 0)
  )

# to vim, ze by se delat nemelo, ale nemuzu prijit, proc tam tenhle radek byl
case_studies_pollen_waffle <- case_studies_pollen_waffle |> 
  slice(-31)


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
    axis.text.x = element_text( size=3),
    axis.text.y = element_text( size=3),
    strip.text = element_text(size = 3),
    
    # Legend
    legend.title = element_blank(),
    legend.spacing = unit(1, 'cm'),
    legend.key.height= unit(0.6, 'cm'),
    legend.key.width= unit(0.6, 'cm'),
    legend.text = element_text(colour = "black", size = 3),
    
    
    # Title
    plot.title.position = "plot",
    plot.title = element_textbox(margin = margin(30, 0, 10, 0),
                                 size = 6,
                                 face = "bold",
                                 width = unit(55, "lines")),
    
    # Sub-title
    plot.subtitle = element_text(margin = margin(10, 0, 20, 0),
                                 size = 4,
                                 color = "grey15"),

    plot.background = element_rect(color="white", fill="white"),
    plot.margin = margin(40, 60, 40, 60)
  ) 



waffle_chart_pollen





























