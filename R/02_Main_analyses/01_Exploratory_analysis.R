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
install.packages("wordcloud2")
install.packages("webshot")
install.packages("htmlwidgets")
library("htmlwidgets")
install.packages(c("tidyverse", "treemap", "ggfittext", "scales", "ggtext"))
library(tidyverse)
library(treemap)
library(ggfittext)
library(scales)
library(ggtext)
library(webshot)
library(tidyverse)
library(here)
library(dplyr)
library(ggplot2)
library(wordcloud2)

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
         journal = str_replace(journal, "&", "And"),
         journal = str_replace(journal, "Journal Of Quartenary Science", "Journal Of Quaternary Science"),
         journal = str_replace(journal, "Plos One", "PLOS One"),
         journal = str_replace(journal, "Of", "of"),
         journal = str_replace(journal, "The", "the"),
         journal = str_replace(journal, "In", "in"),
         journal = str_replace(journal, "And", "and"),
         journal = str_replace(journal, "Veget Hist Archaeobot", "Vegetation History and Archeobotany")) |> 
  count(journal, name= "n", sort = TRUE)

reorder(case_studies_journal$journal, case_studies_journal$n)

# basic plot: the number of case studies published in a particular journals ----
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
  filename = here::here("Outputs/Figures/plot_case_studies_journal.png")) 

# wordcloud: the number of case studies published in a particular journals ----

## save wordcloud ----
# install webshot
webshot::install_phantomjs()

word_cloud_journal <- wordcloud2(case_studies_journal, size=0.4, color=rep_len( c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000"), 
                                                                                 nrow(case_studies_journal) ), minRotation = -pi/16, maxRotation = -pi/16, rotateRatio = 3)
saveWidget(word_cloud_journal,"tmp.html",selfcontained = F)

# and in png or pdf
webshot("tmp.html",file = here::here("Outputs/Figures/wourd_cloud_journal.png"), vwidth = 1000,   # Set large width
        vheight = 1000,  # Set large height
        delay = 10)      # Gives the JavaScript animation time to complete )

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
         pollen_database = str_replace(pollen_database, "Pangea", "Pangaea"),
         pollen_database = str_replace(pollen_database, "Latin America Pollen Database", "Latin American Pollen Database"),
         pollen_database = str_replace(pollen_database, "Se Australian Pollen Database", "SE Australian Pollen Database"),
         pollen_database = if_else(is.na(pollen_database), "Not Available", pollen_database)) |> 
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

# Plot the number of case studies that used a particular pollen database - treemap----

# new column for Neotoma ---
case_studies_pollen_database_categorised <- case_studies_pollen_database |> 
  mutate(
    is_it_Neotoma = if_else(
      str_detect(
        pollen_database,
        "Neotoma|European Modern Pollen Database V.2|European Modern Pollen Database|Latin American Pollen Database|European Pollen Database|North American Pollen Database"
      ),
      "Neotoma",
      "Other"
    ),
    is_it_Neotoma = if_else(
      pollen_database == "Not Available",
      "Not Available", is_it_Neotoma
    ),
    label = paste0(pollen_database, "\n(n = ", n, ")"),
    id_tree = row_number()
  )


# palette
tree_colours_pollen <- c(
  "Neotoma" =  "#56B4E9",
  "Not Available" = "gray",
  "Other"   = "#E69F00"
  
)

pollen_database__tree <- treemap(
  case_studies_pollen_database_categorised,
  index = "label",
  vSize = "n",
  type = "categorical",
  vColor = "is_it_Neotoma",
  algorithm = "pivotSize",
  sortID = "id_tree",
  mirror.y = TRUE,
  mirror.x = TRUE,
  border.lwds = 0.7,
  aspRatio = 5/3,
  
  # colours
  palette = tree_colours_pollen,
  
  # labels
  fontsize.labels = 12,
  fontcolor.labels = "white",
  fontface.labels = 1,
  
  # remove legend title
  title.legend = ""
)




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
         trait_database = str_replace(trait_database, "LEDA trait database", "LEDA"),
       trait_database = if_else(is.na(trait_database), "Not Available", trait_database)) |> 
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

# Plot the number of case studies that used a particular trait database - treemap----
case_studies_trait_database_tree <- case_studies_trait_database |> 
  mutate(
    label = paste0(trait_database, "\n(n = ", n, ")"),
    id_tree = row_number()
  )

case_studies_trait_database_tree <- case_studies_trait_database |> 
  mutate(
    database_type = if_else(
      trait_database == "Not Available",
      "Not Available",
      "Trait database"
    ),
    label = paste0(trait_database, "\n(n = ", n, ")"),
    id_tree = row_number()
  )

# palette
tree_colours_traits <- c(
  "Not Available" = "gray",
  "Trait database" =  "#009E73")


trait_database_tree <- treemap(
  case_studies_trait_database_tree,
  index = "label",
  vSize = "n",
  type = "categorical",
  vColor = "database_type",
  algorithm = "pivotSize",
  sortID = "id_tree",
  mirror.y = TRUE,
  mirror.x = TRUE,
  border.lwds = 0.7,
  aspRatio = 5/3,
  
  # colours
  palette = tree_colours_traits,
  
  # labels
  fontsize.labels = 12,
  fontcolor.labels = "white",
  fontface.labels = 1,
  
  # remove legend title
  title.legend = ""
)


