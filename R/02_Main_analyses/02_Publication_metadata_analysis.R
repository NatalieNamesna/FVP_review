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

#  Publication metadata analysis

#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#

# packages 

install.packages("remotes")
install.packages("ggalluvial")
remotes::install_github("davidsjoberg/ggsankey")
install.packages("networkD3")
install.packages("htmlwidgets")
install.packages("webshot2")
library(webshot2)
library(htmlwidgets)
library(networkD3)
library(ggsankey)
library(ggplot2)
library(dplyr)
library(ggalluvial)
library(tidyverse)
library(here)

# Load the table with case studies 

case_studies <- readr::read_csv("Data/Processed/case_studies_clean.csv")

#----------------------------------------------------------#
# 2. Journal, Year, Region   -----
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
      year >= 1995 & year <= 2000 ~ "1995-2000",
      year >= 2001 & year <= 2005 ~ "2001-2005",
      year >= 2006 & year <= 2010 ~ "2006-2010",
      year >= 2011 & year <= 2015 ~ "2011-2015",
      year >= 2016 & year <= 2020 ~ "2016-2020",
      year >= 2021 & year <= 2025 ~ "2021-2025",
      TRUE ~ "Other"
    )
  )

## define the order of years ----
year_order <- c(
  "1995-2000",
  "2001-2005",
  "2006-2010",
  "2011-2015",
  "2016-2020",
  "2021-2025"
)

## define the order of journal and year ----
journal_order <-
  sankey_data_all_year_groups |>
  count(journal, sort = TRUE) |>
  pull(journal)

region_order <-
  sankey_data_all_year_groups |>
  count(region, sort = TRUE) |>
  pull(region)




## making sankey diagram ----

### sankey diagram with ggalluvial ----
ggplot(
  sankey_data_all_year_groups,
  aes(axis1 = journal,
      axis2 = region,
      axis3 = factor(year_group))
) +
  geom_alluvium(aes(fill = region, width = 1/12), curve_type = "sine") +
  geom_stratum( width = 1/8) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Journal", "Region", "Year")) +
  theme_minimal() +
  ggtitle("Sankey diagram for journal, year and region of the case studies")




### sankey diagram with networkD3 ----

#### it requires node and links ----

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

nodes <- data.frame(
  name = c(
    journal_order,
    region_order,
    year_order
  )
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
  width = 1800,
  height = 1000,
  fontSize = 16,
  nodeWidth = 40,
  nodePadding = 16
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

#### save it ----
# nevim jak to ulozit, aby se mi neusekly ty nazvy journals...
saveWidget(
  figure_sankey_2,
  "figure_sankey.html",
  file = here("Outputs/Figures/Figure_sankey_2.html"),
  selfcontained = TRUE
)

webshot(
  url = here("Outputs/Figures/Figure_sankey_2.html"),
  file = here("Outputs/Figures/Figure_sankey_2.png"),
  vwidth = 1800,
  vheight = 1000,
  zoom = 3
)







