# Title: G06.000 vegetation survey field trip report
# Author details: Justin McCann
# Script and data info: This script loads, pre-processes, and visualises data for the vegetation survey group of methods
# Data obtained from Sharepoint exports out of Echo

#+ message = F, warnings = F, echo = F, include = F
# These lines of code let us turn on/off the warnings that show up when compiling this script into a word doc

#+ message = F, warnings = F, echo = F, include = F
# Load Libraries ####
library(flextable)
library(here)
library(janitor)
library(tidyverse) 
library(lubridate)
library(glue)

# Assign variables ####
#+ message = F, warnings = F, echo = F, include = F
# Specify details of Reserve/Project and Session to create Report for
project_code <- "SCOT"
project_name <- "Scottsdale Reserve"
recent_session <- "2022" #assuming one trapping session per year

#+ message = F, warnings = F, echo = F, include = F
#  create functions ####
# Define a function to print as a table into the word doc a bit neater
print_as_table_for_word <- function(input_table){
  input_table %>% 
    flextable::flextable() %>% 
    flextable::merge_v() %>% 
    flextable::hline() %>% 
    flextable::autofit() %>%
    flextable::padding(padding.top = 0, padding.bottom = 0)
}

# Read in data ####
input_files = list.files(path = here("Data/"), pattern = "*.csv") %>% str_c("Data/",.)  # List all files in the Data folder, then keep the folder name at the front

input_files %>% # 
  map(function(input_files){ # Iterate through each file name
    assign(x = input_files %>% str_remove_all(pattern = "Data/|.csv") %>% make_clean_names(), # make a sensible name for the dataset (in this function, the variable name is called "x")
           value = read_csv(here(input_files), guess_max = 1000000) %>% 
             clean_names() %>% 
             # Now we will generate a session code
             mutate(surveydatetime = date,
                    survey_year = year(surveydatetime),
                    survey_date = date(surveydatetime),
                    survey_month = month(surveydatetime)) %>%
             mutate(season = case_when(survey_month %in% 3:5 ~ 'Autumn', #assign months to the appropriate season
                                       survey_month %in% 6:8 ~ 'Winter',
                                       survey_month %in% 9:11 ~ 'Spring',
                                       TRUE ~ 'Summer')) %>%
             mutate(sessionID = paste(survey_year,season,sep="_")) %>%
             # Now filter the dataset to just the project code we are interested in
             filter(prj_cde == project_code),# read in the data, then rename columns to "clean" names by removing white space and capitalisation.
           envir = .GlobalEnv)
  })

# List all of the methods that have had data downloaded for them ####
# This can be used if all data is valid. If not, name manually below
# datasets_downloaded <- sapply(.GlobalEnv, is.data.frame) 
# mget(names(datasets_downloaded)[datasets_downloaded])

# Manually hash out datasets THat are not to be used for analysis
datasets_downloaded <- lst(
  m04_020_soil_surface_quadrat_analysis_join_parent_child,
  m06_040_photo_points_analysis_join_parent_child, # not to be analysed
  m06_050_recruitment_analysis_join_parent_child,
  # m06_070_legacy_tree_health_quadrat,
  m06_071_tree_health_quadrat_analysis_join_parent_child, # frustratingly, this is being exported with an inconsistent date format
  m06_080_vegetation_quadrat_species_analysis_join_parent_child,
  m06_090_vegetation_intercept_analysis_join_parent_child
  # m06_090_vegetation_intercept_analysis_join_parent_child_v2_aggregated # This is the same data as the previous table, in wide format
)

# Set up datasets to report for this session, or all data ####
# put all of this data into one big table
all.echo.data <- bind_rows(datasets_downloaded, .id = "method_code") %>% 
  mutate(method_code = str_remove(method_code, "_analysis_join_parent_child")) %>% 
  # Move across species names to the scientific name column, allowing grouping by species for tables. 
  mutate(scientific_name = case_when(!is.na(recruitment_species_not_on_list) ~ recruitment_species_not_on_list, 
                                  !is.na(tree_qudrat_species_not_on_list) ~ tree_qudrat_species_not_on_list,
                                  !is.na(intercept_species_not_on_list) ~ intercept_species_not_on_list,
                                  TRUE ~ scientific_name)) %>% 
  select(-recruitment_species_not_on_list, -tree_qudrat_species_not_on_list, intercept_species_not_on_list)

session.data <- all.echo.data %>% 
  filter(str_detect(sessionID, recent_session)) 

#+ message = F, warnings = F, echo = F, include = T
# Data quality check ####

#' list datasets
#' 
#' All data
all.echo.data %>% select(sessionID, method_code) %>% group_by(sessionID) %>% distinct(method_code) %>% arrange(sessionID) %>% print_as_table_for_word()

#' This session
session.data %>% select(sessionID, method_code) %>% group_by(sessionID) %>% distinct(method_code) %>% arrange(sessionID) %>% print_as_table_for_word()

#' check all transect lengths
session.data %>% 
  group_by(method_code) %>%
  distinct(length_m) %>% 
  print_as_table_for_word()

# Make a field trip summary ####
#' Summary variables
#+ message = F, warnings = F, echo = F, include = F
start_date <- min(session.data$survey_date)
end_date <- max(session.data$survey_date)
site_count <- length(unique(session.data$site_id))
methods_used <- lst(unique(session.data$method_code))

#+ message = F, warnings = F, echo = F, include = T
glue("The {list(unique(session.data$sessionID))} vegetation monitoring session at {project_name} ran from {start_date} to {end_date}")
glue("We undertook a total of {length(unique(session.data$method_code))} vegetation survey methods during this time, at {site_count} sites")
glue("The vegetation survey methods used were {methods_used}")

#' Soil surface summary #####
#' Count of quadrat function grouped by target
#+ fig.width = 6, fig.height = 4, dpi = 600
m04_020_soil_surface_quadrat_analysis_join_parent_child %>%  
  group_by(site_target_name) %>% 
  count(quadrat_function) %>% 
  ggplot(aes(x = quadrat_function, y = n)) +
  facet_wrap(~site_target_name) +
  geom_col()  +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1))
  
#' boxplots of soil data
#+ message = F, warnings = F, echo = F, include = F
soil_surface_data_for_plot <- m04_020_soil_surface_quadrat_analysis_join_parent_child %>%  
  group_by(site_target_name) %>% 
  select(bare_ground_percent:perennial_weed_percent) %>% 
  pivot_longer(cols = -site_target_name)

groups_2_3 <- "annual|perennial"
group_3 <- "weed"

veg_plot <- function(inherited_dataset){
  inherited_dataset %>% 
  ggplot(aes(x = site_target_name, y = value)) +
  facet_wrap(~ name) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1))
}
#' Standard soil metrics
#+ message = F, warnings = F, echo = F, include = T
#+ fig.width = 9, fig.height = 8, dpi = 600
soil_surface_data_for_plot %>% 
  filter(!str_detect(name, groups_2_3)) %>% 
  veg_plot()

#' Cover life history
#+ fig.width = 6, fig.height = 4, dpi = 600
soil_surface_data_for_plot %>% 
  filter(str_detect(name, groups_2_3),
         !str_detect(name, group_3)) %>% 
  veg_plot()

#' Cover weediness
#+ fig.width = 6, fig.height = 4, dpi = 600
soil_surface_data_for_plot %>% 
  filter(str_detect(name, groups_2_3),
         str_detect(name, group_3)) %>% 
  veg_plot()
  
#' Recruitment species list - Count of species grouped by site
#+ message = F, warnings = F, echo = F, include = F
recruitment_species_list <- session.data %>%
  filter(str_detect(method_code, "m06_050_recruitment")) %>% 
  group_by(site_target_name, site_id) %>% 
  filter(str_detect(sessionID, recent_session)) %>% # filter to the session (year) of interest
  filter(!is.na(scientific_name) | !is.na(english_name)) %>% # remove records without a species record
  arrange(scientific_name) %>% # sort by name, can add other sorting here if needed
  select(scientific_name, english_name) %>% 
  count(scientific_name, english_name)
#' Below is a table of recruitment observed at each site
#+ message = F, warnings = F, echo = F, include = T
recruitment_species_list %>% print_as_table_for_word()

#+ message = F, warnings = F, echo = F, include = F
#' The following sites have no species listed, and are assumed to have no recruitment.
session.data.na <- session.data %>%
  filter(str_detect(method_code, "m06_050_recruitment")) %>% 
  filter(is.na(scientific_name)) %>% # assess records without a species record
  group_by(site_target_name, site_id) %>%
  relocate(site_target_name) %>% 
  distinct(observation_status) 

#+ message = F, warnings = F, echo = F, include = T
session.data.na %>% 
  print_as_table_for_word()

#' The following is an analysis of vegetation transects:
#' Richness (total number of species across all sites)
#+ message = F, warnings = F, echo = F, include = T
session.data %>%
  filter(str_detect(method_code, "m06_090")) %>% 
  group_by(site_target_name) %>% 
  distinct(scientific_name) %>% 
  count() %>% 
  rename("Count of species scientific names (richness)" = n) %>% 
  print_as_table_for_word()

#' Cover per site (not accounting for overlap)
#+ message = F, warnings = F, echo = F, include = F
transect_length = unique(session.data$length_m)

cover_per_site <- session.data %>%
  filter(str_detect(method_code, "m06_090")) %>% 
  # remove_empty(which = "cols") %>% # Scottsdale data did not record intercept_strata
  group_by(site_target_name, intercept_strata, site_id) %>% 
  mutate(cover_calculation_finish_minus_start = finish - start) %>% 
  summarise(sum_of_cover = sum(cover_calculation_finish_minus_start),
            cover_percent = sum_of_cover/transect_length *100)
#+ message = F, warnings = F, echo = F, include = T
cover_per_site %>% print_as_table_for_word()

cover_per_site %>% ggplot(aes(x = intercept_strata, y = cover_percent)) +
  facet_wrap(. ~ site_target_name) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1))

#' Average cover per target
#+ message = F, warnings = F, echo = F, include = T
cover_per_site %>% 
  select(-site_id) %>%
  summarise(mean_percent = round(mean(cover_percent))) %>% 
  print_as_table_for_word()

#' mean height
session.data %>%
  filter(str_detect(method_code, "m06_090")) %>% 
  # remove_empty(which = "cols") %>% # Scottsdale data did not record intercept_strata
  group_by(site_target_name, intercept_strata) %>% 
  summarise(min_height = round(min(height)),
            mean_height = round(mean(height)),
            max_height = round(max(height))) %>% 
  print_as_table_for_word()

#' Species diversity table all veg methods #####
#' Number of unique species names grouped by method and target
#+ message = F, warnings = F, echo = F, include = F
species_count_veg_methods <- session.data %>%
  filter(!str_detect(method_code, "m04_020_soil")) %>% 
  remove_empty(which = "cols") %>% 
  group_by(method_code, site_target_name) %>% 
  distinct(scientific_name) %>% 
  count()
#+ message = F, warnings = F, echo = F, include = T
species_count_veg_methods %>% print_as_table_for_word()

#' Number of unique species names grouped by method and target for all years 
#+ message = F, warnings = F, echo = F, include = F
species_count_veg_methods_all_echo_data <- all.echo.data %>% 
  filter(!str_detect(method_code, "m04_020_soil")) %>% 
  remove_empty(which = "cols") %>% 
  group_by(survey_year, method_code, site_target_name) %>% 
  distinct(scientific_name) %>% 
  count() %>% 
  rename('unique_species' = 'n')
#+ message = F, warnings = F, echo = F, include = T
#+ fig.width = 6, fig.height = 5, dpi = 600
species_count_veg_methods_all_echo_data %>% 
  ggplot(aes(x = survey_year, y = unique_species, colour = method_code)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ site_target_name) +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(nrow = 3))

