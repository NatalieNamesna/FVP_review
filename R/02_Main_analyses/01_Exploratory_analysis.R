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


# plot the numbers of individual journals of our case studies ----
ggplot(
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


#----------------------------------------------------------#
# 4. Year   -----
#----------------------------------------------------------#

# table with number of observations ----
case_studies_year <- case_studies |> 
  select(id, year) |> 
  count(year, name= "n", sort = TRUE)

# plot how many studies were published in each year ----
ggplot(
  data = case_studies_year,
  mapping = aes(
    y=  reorder(year, n),
    x= n,
  )
) +
  xlim(0,7) +
  geom_col() +
  labs(
    title = "The umber of case studies published in a particular years",
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

# plot how many studies we focused in which region ----
ggplot(
  data = case_studies_region,
  mapping = aes(
    y=  reorder(region, n),
    x= n,
  )
) +
  xlim(0,28) +
  geom_col() +
  labs(
    title = "The number of case studies focused on a particular region.",
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


#----------------------------------------------------------#
# 6. Number of pollen data   -----
#----------------------------------------------------------#


#----------------------------------------------------------#
# 7. Pollen databese   -----
#----------------------------------------------------------#


#----------------------------------------------------------#
# 2. Trait database   -----
#----------------------------------------------------------#









