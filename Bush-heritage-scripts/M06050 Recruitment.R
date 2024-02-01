# Title: Recruitment
# Author details: Gareth Davies
# Script and data info: This script loads, pre-processes, and visualises data for recruitment method:
# - M06.050 Recruitment
# Data obtained from Sharepoint exports out of Echo

# Output tables produced by this script:
# - Echo_M06050_project contains raw M06.050 data for the selected project and date range
# - recr_sites
# - recr_targets
# - recr_site_species
# - recr_target_species

# Libraries
install.packages("here")
install.packages("janitor")
install.packages("tidyverse")
install.packages("lubridate")

library(here)
library(janitor)
library(tidyverse) 
library(lubridate)


#####
# Define variables
#####

### Specify details of Reserve/Project and date range
project_code_for_data_analysis <- "SCOT"
project_name <- "Scottsdale Reserve"
date_filter_start <- as_date("2006-01-01")
date_filter_end <- as_date("2020-01-01")


#####
# Data import
#####
# Import the Echo data file
# Manually specify column types to avoid CSV miscasts
Echo_M06050 <- read_csv("Data/M06_050_Recruitment_Analysis_Join_Parent_Child.csv",
                        col_types = "dccccccccdcccdddddccccldccdccccccc") %>%
  clean_names() %>%
  mutate(survey_year = year(dmy_hms(created_date_local_adjusted)),
         survey_date = date(dmy_hms(created_date_local_adjusted)))

# Create a subset of the data file for the selected project
Echo_M06050_project <- Echo_M06050 %>%
  filter(prj_cde == project_code_for_data_analysis) %>%
  filter(survey_date >= date_filter_start) %>%
  filter(survey_date < date_filter_end)

Echo_M06050_project %>% glimpse()

# Remove Echo_M04020 from memory
rm(Echo_M06050)


## Outputs:

# Number of sites with recruits
# Total number of recruits
# Mean number of recruits per 100m2 at sites with recruitment
# Total number of species recorded

recr_sites <- Echo_M06050_project %>%
  distinct(survey_year, site_target_name, site_id) %>%
  select(survey_year, site_target_name, site_id)

recr_targets <- recr_sites %>%
  group_by(survey_year, site_target_name) %>%
  summarise(n_target_sites = n()) %>%
  mutate(n_target_segments = n_target_sites * 5)

recr_site_species <- Echo_M06050_project %>%
  group_by(survey_year, site_target_name, site_id) %>%
  summarise(recruits_all = sum(replace_na(recruitment_count_of_species, 0))) %>%
  left_join(Echo_M06050_project %>%
              group_by(survey_year, site_target_name, site_id, scientific_name) %>%
              summarise(n_species_segments = n(), recruits_sp = sum(replace_na(recruitment_count_of_species, 0)))) %>%
  mutate(scientific_name = replace_na(scientific_name, "Unknown"), site_recruitment_pct = recruits_sp / recruits_all) %>%
  select(survey_year, site_target_name, site_id, scientific_name, recruits_sp, recruits_all, site_recruitment_pct, n_species_segments)

recr_target_species <- recr_targets %>%
  left_join(recr_site_species) %>%
  group_by(survey_year, site_target_name, scientific_name, n_target_sites, n_target_segments) %>%
  summarise(n_species_sites = n(), recruits_sp_total = sum(recruits_sp), recruits_sp_mean = mean(recruits_sp), n_species_segments = sum(n_species_segments)) %>%
  mutate(site_freq = n_species_sites / n_target_sites, segment_freq = n_species_segments / n_target_segments) %>%
  select(survey_year, site_target_name, scientific_name, recruits_sp_total, n_species_sites, recruits_sp_mean, n_target_sites, site_freq, n_species_segments, n_target_segments, segment_freq)

# recr_reserve_species <- 
