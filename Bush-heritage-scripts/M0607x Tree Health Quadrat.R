# Title: Tree Health
# Author details: Gareth Davies
# Script and data info: This script loads, pre-processes, and visualises data for tree health methods:
# - M06.070 Tree Health Quadrat (Legacy)
# - M06.071 Tree Health Quadrat
# Data obtained from Sharepoint exports out of Echo

# The following output tables can be produced by this script:
# - Echo_M0607x_project contains merged raw M06.070 and M06.071 data for the selected project and date range
# - tree_all
# - tree_target
# - tree_site
# - tree_species

# This script is designed to be run interactively - select and run lines based on your requirements.
# The script is not designed to be run end-to-end.



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
# Import the Echo data files
# Manually specify column types to avoid CSV miscasts and mismatches

Echo_M06070 <- read_csv("Data/M06_070_Legacy_Tree_Health_Quadrat.csv",
                        col_types="icccccccccnccciicinnccccliiccccc") %>%
  clean_names() %>%
  mutate(survey_year = year(dmy_hms(created_date_local_adjusted)),
         survey_date = date(dmy_hms(created_date_local_adjusted)),
         method = "M06.070") %>%
  select(-created_date_local_adjusted)

Echo_M06071 <- read_csv("Data/M06_071_Tree_Health_Quadrat_Analysis_Join_Parent_Child.csv",
                        col_types="icccccccTncclnncninncccclicciccccccc") %>%
  clean_names() %>%
  mutate(survey_year = year(created_date_local_adjusted),
         survey_date = date(created_date_local_adjusted),
         score_text = score,
         method = "M06.071") %>%
  select(-created_date_local_adjusted, -score)

# Create filtered sets of observations for the identified project
Echo_M06070_project <- Echo_M06070 %>%
  filter(prj_cde == project_code_for_data_analysis) %>%
  filter(survey_date >= date_filter_start) %>% # Start date of interest
  filter(survey_date < date_filter_end) # End date of interest

Echo_M06071_project <- Echo_M06071 %>%
  filter(prj_cde == project_code_for_data_analysis) %>%
  filter(survey_date >= date_filter_start) %>% # Start date of interest
  filter(survey_date < date_filter_end) # End date of interest
# filter(!is.na(scientific_name)) %>% # Drop observations without species
# filter(scientific_name != "Unrecorded")

# Compare columns of the two project datasets - check for type mismatches
compare_df_cols(Echo_M06070_project, Echo_M06071_project)

# Merge datasets
Echo_M0607x_project <- full_join(Echo_M06070_project, Echo_M06071_project)

# Remove source datasets from memory
rm(Echo_M06070, Echo_M06071, Echo_M06070_project, Echo_M06071_project)

# Preview dataset
Echo_M0607x_project %>% glimpse()


#####
# Visualise raw data
#####

Echo_M0607x_project %>%
  group_by(scientific_name) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

Echo_M0607x_project %>%
  group_by(score) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

# View records with no score - exclude?
Echo_M0607x_project %>%
  filter(is.na(score)) %>%
  View()
# An NA score may indicate an absence of trees in the location.


# Note: Score descriptions range from 1 (best) to 5 (worst)


#####
# Create output datasets
#####

tree_all <- Echo_M0607x_project %>%
  select(site_id, site_target_name, survey_year, survey_date, peg, tree_number, scientific_name, score) %>%
  filter(!is.na(score))

tree_target <- tree_all %>%
  group_by(site_target_name, survey_year) %>%
  summarise(score_mean = mean(score, na.rm = TRUE),
            score_med = median(score, na.rm = TRUE),
            score_sd = sd(score, na.rm = TRUE),
            n = n())

tree_site <- tree_all %>%
  group_by(site_id, site_target_name, survey_year) %>%
  summarise(score_mean = mean(score, na.rm = TRUE),
            score_med = median(score, na.rm = TRUE),
            score_sd = sd(score, na.rm = TRUE),
            n = n())

tree_species <- tree_all %>%
  filter(!is.na(scientific_name)) %>%
  group_by(scientific_name, survey_year) %>%
  summarise(score_mean = mean(score, na.rm = TRUE),
            score_med = median(score, na.rm = TRUE),
            score_sd = sd(score, na.rm = TRUE),
            n = n())

tree_all %>%
  group_by(site_target_name) %>%
  summarise(score_mean = mean(score, na.rm = TRUE))

tree_all %>%
  group_by(site_target_name, survey_year) %>%
  summarise(score_mean = mean(score, na.rm = TRUE))


#####
# Plots
#####

### All targets
# Boxplot
tree_all %>%
  ggplot(aes(x = as.factor(survey_year), y = score)) +
  facet_wrap(~site_target_name) +
  geom_boxplot() +
  labs(x = "Year", y = "Tree health score")


### Target of interest
focal_target <- "Grassy Box-Gum Woodlands"

# Boxplot
tree_all %>%
  filter(site_target_name %in% focal_target) %>%
  ggplot(aes(x = as.factor(survey_year), y = score)) +
  geom_boxplot() +
  labs(x = "Year", y = "Tree health score")

# Dotplot with glm trendline
tree_all %>%
  filter(site_target_name %in% focal_target) %>%
  ggplot(aes(x = survey_date, y = score, xmin = date_filter_start, xmax = date_filter_end)) +
  scale_y_reverse() +
  # geom_jitter(width = 0.1, height = 0.1) +
  geom_jitter(width = 14, height = 0) +
  geom_smooth(method = "glm") +
  theme_classic() +
  labs(x = "Year", y = "Tree health score") +
  ggtitle(paste0("Tree health - ", project_name, " ", focal_target), subtitle = "Score range: 1 (best) to 5 (worst). Horizontal jitter added.")

