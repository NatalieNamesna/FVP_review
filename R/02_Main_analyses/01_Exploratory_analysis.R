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
# 6. Journal, Year, Region   -----
#----------------------------------------------------------#

# tables with IDs of case studies ----

## Journal ----
case_studies_journal_id <- case_studies |> 
  select(id, journal) |> 
  mutate(journal = str_trim(journal),           
         journal = str_squish(journal),         
         journal = str_to_title(journal),
         journal = str_replace(journal, "&", "And"))

## Region ----
case_studies_region_id <- case_studies |>
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


## Year ----
case_studies_year_id <- case_studies |> 
  select(id, year)

# Sankey diagram ----

## one table with all variables ----
sankey_data <- 
  left_join(case_studies_journal_id, case_studies_year_id, by = "id")

sankey_data_all <- 
  left_join(sankey_data, case_studies_region_id, by= "id")

## grouping to time slices ----
sankey_data_all_year_groups <- sankey_data_all |> 
  mutate(
    year_group = case_when(
      year >= 1995 & year <= 2000 ~ "1995–2000",
      year >= 2001 & year <= 2005 ~ "2001–2005",
      year >= 2006 & year <= 2010 ~ "2006–2010",
      year >= 2011 & year <= 2015 ~ "2011–2015",
      year >= 2016 & year <= 2020 ~ "2016–2020",
      year >= 2021 & year <= 2025 ~ "2021–2025",
      TRUE ~ "Other"
    )
  )


## making sankey diagram ----

install.packages("remotes")
install.packages("ggalluvial")
remotes::install_github("davidsjoberg/ggsankey")
install.packages("networkD3")
library(networkD3)
library(ggsankey)
library(ggplot2)
library(dplyr)
library(ggalluvial)

### sankey diagram with ggalluvial ----
ggplot(
  sankey_data_all,
  aes(axis1 = journal,
      axis2 = region,
      axis3 = factor(year))
) +
  geom_alluvium(aes(fill = region), width = 1/12) +
  geom_stratum(width = 1/8) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Journal", "Region", "Year")) +
  theme_minimal()

### sankey diagram with networkD3 ----

#### it require node and links ----

links1 <- sankey_data_all_year_groups |>
  count(journal, region, name = "value")

links2 <- sankey_data_all_year_groups |>
  count(region, year_group, name = "value")

nodes <- data.frame(
  name = unique(c(
    links1$journal,
    links1$region,
    links2$year_group
  ))
)

#### create source and target IDs ----
links1 <- links1 |>
  mutate(
    source = match(journal, nodes$name) - 1,
    target = match(region, nodes$name) - 1,
    group = region
  )

links2 <- links2 |>
  mutate(
    source = match(region, nodes$name) - 1,
    target = match(year_group, nodes$name) - 1,
    group = region
  )

links <- bind_rows(links1, links2)

#### assign node groups -> different colouring of journals, year and regions ----
nodes <- nodes |>
  mutate(
    group = case_when(
      name %in% unique(sankey_data_all_year_groups$journal) ~ "Journal",
      name %in% unique(sankey_data_all_year_groups$region) ~ "Region",
      name %in% unique(sankey_data_all_year_groups$year_group) ~ "Year"
    )
  )

#### colour palette ----
colourScale <- '
d3.scaleOrdinal()
.domain([
"Journal",
"Region",
"Year",
"Africa",
"Asia",
"Europe",
"Latin America",
"North America",
"South America",
"Middle East",
"Australia And Oceania"
])
.range([
"#4E79A7",
"#F28E2B",
"#59A14F",
"#E15759",
"#76B7B2",
"#EDC948",
"#B07AA1",
"#FF9DA7",
"#9C755F",
"#BAB0AC",
"#86BCB6"
])
'

#### draw the sankey ----

figure_sankey <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  
  NodeGroup = "group",
  LinkGroup = "group",
  
  colourScale = colourScale,
  
  fontSize = 20,
  nodeWidth = 50,
  nodePadding = 12,
  
  width = 1200,
  height = 700
)


#### try to making nicer ----
install.packages("htmlwidgets")
library(htmlwidgets)

figure_sankey_2 <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  NodeGroup = "group",
  LinkGroup = "group",
  colourScale = colourScale,
  fontSize = 20,
  nodeWidth = 35,
  nodePadding = 12
)

figure_sankey_2 <- onRender(
  figure_sankey_2,
  '
function(el, x) {
  d3.select(el)
    .selectAll(".node text")
    .attr("x", -8)
    .attr("text-anchor", "end");
}
'
)

figure_sankey_2



#----------------------------------------------------------#
# 6. Number of pollen data   -----
#----------------------------------------------------------#


#----------------------------------------------------------#
# 7. Pollen databese   -----
#----------------------------------------------------------#


#----------------------------------------------------------#
# 2. Trait database   -----
#----------------------------------------------------------#









