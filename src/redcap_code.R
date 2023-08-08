# Example scripts to work with REDCap - based on "Clinical Trials" sample database
library(tidyverse)
library(readr)
library(REDCapTidieR)


data <- read_csv("ClinicalTrial-Baseline_DATA_2023-08-07_1351.csv", 
                 na = "NA",
                 col_types = cols(.default = "c"))

df_separated <- data %>%
  separate_rows(dm_race, sep = ",")


library(REDCapTidieR)
superheroes <- read_redcap(redcap_uri, token)

superheroes |>
  rmarkdown::paged_table()