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

#  Prepare the table with case studies for further analysis

#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#

library(tidyverse)
library(here)


# Load table with case studies
here::here("Data/Input/case_studies_table.csv")
case_studies <- readr::read_csv("Data/Input/case_studies_table.csv")


# cleaning colnames
case_studies_clean <- case_studies |> 
  janitor::clean_names()


# save
write_csv(case_studies_clean, "Data/Processed/case_studies_clean.csv")


