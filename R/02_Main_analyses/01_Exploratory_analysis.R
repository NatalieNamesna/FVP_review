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

#  Exploratory analysis

#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#

# packages

library(tidyverse)
library(here)
library(dplyr)
library(ggplot2)

# Load the table with case studies
case_studies <- readr::read_csv("Data/Processed/case_studies_clean.csv")


#----------------------------------------------------------#
# 2. Summary   -----
#----------------------------------------------------------#

summary(case_studies)

#----------------------------------------------------------#
# 3. Journal   -----
#----------------------------------------------------------#

# table with number of observations ----
case_studies_journal <- case_studies |> 
  select(id, journal) |> 
  mutate(journal = str_trim(journal),           
         journal = str_squish(journal),         
         journal = str_to_title(journal),
         journal = str_replace(journal, "&", "And")) |> 
  count(journal, name= "n", sort = TRUE)


# plot the number of case studies published in a particular journals ----
plot_case_studies_journal <- ggplot(
  data = case_studies_journal,
  mapping = aes(
    y=  reorder(journal, n),
    x= n,
  )
) +
  xlim(0,14) +
  geom_col() +
  labs(
    title = "The number of case studies published in a particular journals",
    x = "n",
    y = "Journal",
  )+
  scale_x_continuous(breaks = seq(0, 14, by = 2)) +
  coord_cartesian(expand = FALSE) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    plot.title = element_text(
    face = "bold",
    margin = margin(b = 10)
    ),
   plot.title.position = "plot",
   plot.margin = margin(15, 10, 10, 15)
  )

ggplot2::ggsave(
  plot = plot_case_studies_journal,
  filename = here::here("OUtputs/Figures/plot_case_studies_journal.png")) 


#----------------------------------------------------------#
# 4. Year   -----
#----------------------------------------------------------#

# table with number of observations ----
case_studies_year <- case_studies |> 
  select(id, year) |> 
  count(year, name= "n", sort = TRUE)

# plot the number of case studies published in a particular years ----
plot_case_studies_year <- ggplot(
  data = case_studies_year,
  mapping = aes(
    y=  reorder(year, n),
    x= n,
  )
) +
  xlim(0,7) +
  geom_col() +
  labs(
    title = "The number of case studies published in a particular years",
    x = "n",
    y = "Year",
  )+
  scale_x_continuous(breaks = seq(0, 7, by = 1)) +
  coord_cartesian(expand = FALSE) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    plot.title = element_text(
      face = "bold",
      margin = margin(b = 10)
    ),
    plot.title.position = "plot",
    plot.margin = margin(15, 10, 10, 15)
  )

ggplot2::ggsave(
  plot = plot_case_studies_year,
  filename = here::here("OUtputs/Figures/plot_case_studies_year.png")) 

#----------------------------------------------------------#
# 5. Region   -----
#----------------------------------------------------------#

# table with number of observations ----
case_studies_region <- case_studies |>
  select(id, region) |>
  mutate(
    region = str_replace_all(region, "\\n", " "),
    region = str_squish(region),
    region = str_to_title(region)
  ) |>
  separate_longer_delim(region, delim = ",") |>
  mutate(
    region = str_trim(region)   # <-- remove spaces after splitting
  ) |>
  count(region, name = "n", sort = TRUE) |> 
    filter(region %in% c("Africa", "Asia","Australia And Oceania", "Europe","Latin America", "Middle East", "North America", "South America"))

sort(unique(case_studies_region$region))

# plot the number of case studies focused on a particular region----
plot_case_studies_region <- ggplot(
  data = case_studies_region,
  mapping = aes(
    y=  reorder(region, n),
    x= n,
  )
) +
  xlim(0,28) +
  geom_col() +
  labs(
    title = "The number of case studies focused on a particular region",
    x = "n",
    y = "Year",
  )+
  scale_x_continuous(breaks = seq(0, 28, by = 2)) +
  coord_cartesian(expand = FALSE) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    plot.title = element_text(
      face = "bold",
      margin = margin(b = 10)
    ),
    plot.title.position = "plot",
    plot.margin = margin(15, 10, 10, 15)
  )

ggplot2::ggsave(
  plot = plot_case_studies_region,
  filename = here::here("OUtputs/Figures/plot_case_studies_region.png")) 


#----------------------------------------------------------#
# 6. Number of pollen data   -----
#----------------------------------------------------------#

case_studies_n_pollen <- case_studies |> 
  select(id, number_of_modern_pollen_records, number_of_fossil_pollen_records) 

#----------------------------------------------------------#
# 7. Pollen databese   -----
#----------------------------------------------------------#

# table with number of observations ----
case_studies_pollen_database <- case_studies |> 
  select(id, source_of_pollen_data_which_database) |> 
  separate_longer_delim(source_of_pollen_data_which_database, delim = ",") |>
  mutate(pollen_database = source_of_pollen_data_which_database, 
         pollen_database = str_replace_all(pollen_database, "\\n", ""),
         pollen_database = str_trim(pollen_database),           
         pollen_database = str_squish(pollen_database),         
         pollen_database = str_to_lower(pollen_database),
         pollen_database = str_to_title(pollen_database), 
         pollen_database = str_replace(pollen_database, "Pangea", "Pangaea")) |> 
  count(pollen_database, name= "n", sort = TRUE)
  

sort(unique(case_studies_pollen_database$pollen_database))

# Plot the number of case studies that used a particular pollen database ----
plot_case_studies_pollen_database <- ggplot(
  data = case_studies_pollen_database,
  mapping = aes(
    y=  reorder(pollen_database, n),
    x= n,
  )
) +
  xlim(0,24) +
  geom_col() +
  labs(
    title = "The number of case studies that used a particular pollen database",
    x = "n",
    y = "Pollen Database",
  )+
  scale_x_continuous(breaks = seq(0, 24, by = 2)) +
  coord_cartesian(expand = FALSE) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    plot.title = element_text(
      face = "bold",
      margin = margin(b = 10)
    ),
    plot.title.position = "plot",
    plot.margin = margin(15, 10, 10, 15)
  )

ggplot2::ggsave(
  plot = plot_case_studies_pollen_database,
  filename = here::here("OUtputs/Figures/plot_case_studies_pollen_database.png")) 


#----------------------------------------------------------#
# 8. Trait database   -----
#----------------------------------------------------------#

# table with number of observations ----
case_studies_trait_database <- case_studies |> 
  select(id, source_of_trait_data_which_database) |> 
  separate_longer_delim(source_of_trait_data_which_database, delim = ",") |>
  mutate(trait_database = source_of_trait_data_which_database, 
         trait_database = str_replace_all(trait_database, "\\n", ""),
         trait_database = str_trim(trait_database),           
         trait_database = str_squish(trait_database),         
       #  trait_database = str_to_lower(trait_database),
       #  trait_database = str_to_title(trait_database),
         trait_database = str_replace(trait_database, "Flora Europea", "Flora Europaea"),
         trait_database = str_replace(trait_database, "LEDA trait database", "LEDA")) |> 
  count(trait_database, name= "n", sort = TRUE)


sort(unique(case_studies_trait_database$trait_database))

# Plot the number of case studies that used a particular trait database ----
plot_case_studies_trait_database <- ggplot(
  data = case_studies_trait_database,
  mapping = aes(
    y=  reorder(trait_database, n),
    x= n,
  )
) +
  xlim(0,44) +
  geom_col() +
  labs(
    title = "The number of case studies that used a particular trait database",
    x = "n",
    y = "Trait Database",
  )+
  scale_x_continuous(breaks = seq(0, 44, by = 2)) +
  coord_cartesian(expand = FALSE) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    plot.title = element_text(
      face = "bold",
      margin = margin(b = 10)
    ),
    plot.title.position = "plot",
    plot.margin = margin(15, 10, 10, 15)
  )

ggplot2::ggsave(
  plot = plot_case_studies_trait_database,
  filename = here::here("OUtputs/Figures/plot_case_studies_trait_database.png")) 







