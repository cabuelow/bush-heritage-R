# Title: Vegetation Quadrat
# Author details: Gareth Davies
# Script and data info: This script loads, pre-processes, and visualises data for vegetation cover methods:
# - M06.080 Vegetation Quadrat
# Data obtained from Sharepoint exports out of Echo

# The following output tables can be produced by this script:
# - Echo_M06080_project contains raw M06.080 data for the selected project and date range
# - veg_qdt_site
# - veg_qdt_species
# - veg_qdt_species_names
# - veg_qdt_native

# This script is designed to be run interactively - select and run lines based on your requirements.
# The script is not designed to be run end-to-end.



# Libraries
install.packages("here")
install.packages("janitor")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("rgbif")
# install.packages("taxize")
# install.packages("galah")

library(here)
library(janitor)
library(tidyverse) 
library(lubridate)
# library(taxize)
# library(galah)
library(rgbif)



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
Echo_M06080 <- read_csv("Data/M06_080_Vegetation_Quadrat_Species_Analysis_Join_Parent_Child.csv",
                        col_types="iccccccccIccciiddccccccccccliccdc") %>%
  clean_names() %>%
  mutate(survey_year = year(dmy_hms(created_date_local_adjusted)),
         survey_date = date(dmy_hms(created_date_local_adjusted)))

## View all project codes
# Echo_M06080 %>%
#   group_by(prj_cde) %>%
#   summarise(count = n()) %>%
#   print(n=nrow(.))

## Bar chart showing number of records per project 
# ggplot(Echo_M04020) + geom_bar(mapping = aes(x = fct_rev(prj_cde))) + coord_flip()


# Create a filtered set of observations for the identified project
Echo_M06080_project <- Echo_M06080 %>%
  filter(prj_cde == project_code_for_data_analysis) %>%
  filter(survey_date >= date_filter_start) %>% # Start date of interest
  filter(survey_date < date_filter_end) # End date of interest
  # filter(!is.na(scientific_name)) %>% # Drop observations without species
  # filter(scientific_name != "Unrecorded")

Echo_M06080_project %>% glimpse()

# Remove Echo_M06080 from memory
rm(Echo_M06080)



#####
# Visualise raw data
#####

# Tables
# Show survey dates and number of observations
Echo_M06080_project %>%
  group_by(survey_date) %>%
  summarise(count = n()) %>%
  print(n=nrow(.))

# Show targets, survey dates and number of observations
Echo_M06080_project %>%
  group_by(site_target_name, survey_date) %>%
  summarise(count = n()) %>%
  print(n=nrow(.))

# Show targets, site IDs, survey dates and number of observations
Echo_M06080_project %>%
  group_by(site_id, site_target_name, survey_date, test = is.na(scientific_name)) %>%
  summarise(n_all_obs = n()) %>%
  print(n=nrow(.))

# Echo_M06080_project %>%
#   filter(site_id == "SCT001") %>%
#   View()

# Show species and number of observations
Echo_M06080_project %>%
  group_by(scientific_name) %>%
  summarise(count = n()) %>%
  print(n=nrow(.))


# Plots
# Plot species counts over time
Echo_M06080_project %>%
  ggplot(., aes(x = survey_date)) +
  geom_point(aes(y = quadrat_count_of_species)) +
  theme_classic() +
  theme(legend.position = "none")
  labs(x = "Time")

# Plot species counts per species
Echo_M06080_project %>%
  ggplot(., aes(x = fct_rev(scientific_name))) +
  geom_point(aes(y = quadrat_count_of_species), alpha = 1/10) +
  theme_classic() +
  theme(axis.text.y = element_text(size = 6)) +
  coord_flip() +
  labs(x = "Scientific name")



#####
# Calculate species, taxonomic family and quadrat metrics
#####

# Quadrats are spaced at intervals along a transect.
# Current M06.080 Echo data includes a record of the number of quadrats within which a species occurs for a given transect.
# This data is collected only for selected species.

# Species density, relative abundance, and other metrics cannot be reliably obtained from this data unless we assume
# that a species' frequency of occurrence across quadrats is equivalent to species density, i.e. the number of individuals
# occurring within a quadrat. This is unlikely.
#
# Only frequency and relative frequency can be reliably obtained.


### Species

## Frequency

# Create simplified species data table
veg_qdt_species <- Echo_M06080_project %>%
  select(site_target_name, site_id, survey_year, survey_date, scientific_name, freq = quadrat_count_of_species) %>%
  filter(!is.na(freq))


## Relative frequency (of species observed)
veg_qdt_species <- veg_qdt_species %>%
 group_by(site_target_name, site_id, survey_year, survey_date) %>%
   mutate(rel_freq = freq / sum(freq))

veg_qdt_species %>% glimpse()
# veg_qdt_species %>% write.table(., "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)


### Taxonomic family

# Echo data does not currently provide categories by which to group species - e.g. genus, NVIS growthform, native/exotic.
# The R package galah (an interface to Atlas of Living Australia data) can provide taxonomic family from a recorded
# scientific name, but seems to encounter problems with homonyms.
# The R package taxize can be used to find taxonomic family from a recorded scientific name via scraping of The Plant List
# data, but is slow.
# Using galah first, then taxize gives pretty good results:

# Using package galah, scrape the Atlas of Living Australia to find taxonomic family from recorded scientific name, if possible.
# veg_qdt_species <- veg_qdt_species %>%
#   rowwise() %>%
#   mutate(family = ifelse(is.null(select_taxa(scientific_name)$family), NA, select_taxa(scientific_name)$family)) %>%
#   ungroup()
# 
# # Using package taxize, fill any gaps in galah outputs
# # taxize leaves null string results if it can't find a match - replace these with NA
# veg_qdt_species <- veg_qdt_species %>%
#   rowwise() %>%
#   mutate(family = ifelse(is.na(family), na_if(plantminer(scientific_name, messages = FALSE)$family, ""), family)) %>%
#   ungroup()

# # If preferred, the order of application could be reversed:
# # Using package taxize, scrape web database to add taxonomic family from recorded scientific name, if possible.
# veg_qdt_species <- veg_qdt_species %>%
#   mutate(family = plantminer(scientific_name, messages = FALSE)$family)
# # galah could also be applied as a second step after a first pass of taxize:
# veg_qdt_species <- veg_qdt_species %>%
#   mutate(family = ifelse(family == "", ifelse(is.null(select_taxa(scientific_name)$family), NA, select_taxa(scientific_name)$family), family))


### Species origin - introduced/invasive species and inferred nativeness

# Using package rgbif, parse recorded scientific names in the Global Biodiversity Information Framework and find
# whether they can be found in the Global Register of Introduced and Invasive Species checklist for Australia.
# GRIIS Australia datasetKey = 15147db1-27c3-49b5-9c69-c78d55a4b8ff

# Extract observed scientific names
veg_qdt_species_names <- veg_qdt_species %>%
  filter(!is.na(scientific_name)) %>%
  distinct(scientific_name)

# Return taxonomic information from the GBIF backbone taxonomy
veg_qdt_species_names <- veg_qdt_species_names %>%
  rowwise() %>%
  mutate(gbif = list(name_backbone(scientific_name))) %>%
  unnest(cols = c(gbif)) %>%
  select(scientific_name, rank, status, kingdom, phylum, order, family, genus, species)

# Identify native and non-native species, with reference to the GRIIS Australia dataset
veg_qdt_species_names <- veg_qdt_species_names %>%
  rowwise() %>%
  mutate(native = ifelse(
    rank == "SPECIES", ifelse(
      name_lookup(
        species,
        datasetKey = "15147db1-27c3-49b5-9c69-c78d55a4b8ff"
      )$meta$count > 0,
      "Introduced or invasive",
      "Native"),
    "Unknown"))

# Remove rowwise grouping
veg_qdt_species_names <- ungroup(veg_qdt_species_names)

# Join with veg_qdt_species
veg_qdt_species <- veg_qdt_species %>%
  left_join(veg_qdt_species_names)



# Summarise observations per family (NAs may need further investigation)
veg_qdt_species %>%
  group_by(family) %>%
  summarise(count = n()) %>%
  print(n = nrow(.))

# Produce family table
veg_qdt_family <- veg_qdt_species %>%
  group_by(site_target_name, site_id, survey_year, survey_date, family) %>%
  summarise(freq = sum(freq)) %>%
  group_by(site_target_name, site_id, survey_year, survey_date) %>%
  mutate(rel_freq = freq / sum(freq)) %>%
  ungroup()


# Produce native table
veg_qdt_native <- veg_qdt_species %>%
  group_by(site_target_name, site_id, survey_year, survey_date, native) %>%
  summarise(freq = sum(freq)) %>%
  group_by(site_target_name, site_id, survey_year, survey_date) %>%
  mutate(rel_freq = freq / sum(freq)) %>%
  ungroup()
  


### Site - observed species richness

# Use with caution - only valid if there has been equal effort across quadrats

veg_qdt_site <- veg_qdt_species %>%
  filter(!is.na(scientific_name) & scientific_name != 'Unrecorded') %>%
  group_by(site_id, site_target_name, survey_year, survey_date) %>%
  summarise(obs_richness = n())



#####
# Plots for report
#####

# Optional: define a focal target of interest. Change manually as required.
# focal_target <- "Grassy Box-Gum Woodlands"


## Subset of species - one or several

# Define species (single or multiple) of interest
focal_species <- c("Themeda triandra", "Rytidosperma sp.")
# focal_target <- "Grassy Box-Gum Woodlands"

# Species frequency
veg_qdt_species %>%
  filter(scientific_name %in% focal_species) %>%
  group_by(survey_date, scientific_name) %>%
  ggplot(., aes(x = survey_date, y = freq, alpha = 20)) +
  facet_wrap(~scientific_name) +
  geom_point() +
  geom_smooth(method = glm) +
  theme_classic() +
  labs(x="Survey date", y="Frequency") +
  ggtitle(label = "Species frequency", subtitle = paste0(focal_species,collapse=", "))

# Species frequency for a focal target of interest
veg_qdt_species %>%
  filter(scientific_name %in% focal_species) %>%
  filter(site_target_name %in% focal_target) %>%
  group_by(survey_date, scientific_name) %>%
  ggplot(., aes(x = survey_date, y = freq, alpha = 20)) +
  facet_wrap(~scientific_name) +
  geom_point() +
  geom_smooth(method = glm) +
  theme_classic() +
  labs(x="Survey date", y="Frequency") +
  ggtitle(label = paste0("Species frequency - ",focal_target), subtitle = paste0(focal_species,collapse=", "))


# Relative frequency
veg_qdt_species %>%
  filter(scientific_name %in% focal_species) %>%
  group_by(survey_date, scientific_name) %>%
  ggplot(., aes(x = survey_date, y = rel_freq)) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~scientific_name) +
  geom_point() +
  geom_smooth(method = glm) +
  theme_classic() +
  labs(x="Survey date", y="Relative frequency") +
  ggtitle(label = "Species relative frequency", subtitle = paste0(focal_species, collapse=", "))

# Relative frequency for a focal target of interest
veg_qdt_species %>%
  filter(scientific_name %in% focal_species) %>%
  filter(site_target_name %in% focal_target) %>%
  group_by(survey_date, scientific_name) %>%
  ggplot(., aes(x = survey_date, y = rel_freq)) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~scientific_name) +
  geom_point() +
  geom_smooth(method = glm) +
  theme_classic() +
  labs(x="Survey date", y="Relative frequency") +
  ggtitle(label = paste0("Species relative frequency - ",focal_target), subtitle = paste0(focal_species, collapse=", "))




## All species

# Plot species frequency over time
veg_qdt_species %>%
  filter(scientific_name != "Unrecorded") %>%
  group_by(survey_year, scientific_name) %>%
  ggplot(., aes(x = as.factor(survey_year), y = freq)) +
  facet_wrap(~scientific_name) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs (x = "Year", y = "Frequency") +
  ggtitle("Species frequency")

# Species density over time for a focal target of interest
# veg_qdt_species %>%
#   filter(site_target_name %in% focal_target) %>%
#   group_by(survey_year, scientific_name) %>%
#   ggplot(., aes(x = as.factor(survey_year), y = quadrat_count_of_species)) +
#   facet_wrap(~scientific_name) +
#   geom_boxplot() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   labs (x = "Year") +
#   ggtitle(paste("Species frequency -", focal_target))

# Plot species relative frequency over time
veg_qdt_species %>%
  filter(scientific_name != "Unrecorded") %>%
  # filter(site_target_name %in% focal_target) %>%
  group_by(survey_year, scientific_name) %>%
  ggplot(., aes(x = as.factor(survey_year), y = rel_freq)) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~scientific_name) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs (x = "Year", y = "Relative frequency") +
  ggtitle("Species relative frequency")

# Plot relative abundance over time for a focal target of interest
# veg_qdt_species %>%
#   filter(scientific_name != "Unrecorded") %>%
#   filter(site_target_name %in% focal_target) %>%
#   group_by(survey_year, scientific_name) %>%
#   ggplot(., aes(x = as.factor(survey_year), y = rel_abund)) +
#   facet_wrap(~scientific_name) +
#   geom_boxplot() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   labs (x = "Year", y = "Relative abundance") +
#   ggtitle(paste("Species relative abundance -", focal_target))



## Taxonomic family

# Family frequency boxplots
veg_qdt_species %>%
  filter(!is.na(family)) %>%
  group_by(survey_year, family) %>%
  ggplot(., aes(x = as.factor(survey_year), y = freq)) +
  facet_wrap(~family) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs (x = "Year", y = "Frequency") +
  ggtitle("Taxonomic family frequency")

# Family density boxplot for target of interest
# veg_qdt_species %>%
#   filter(!is.na(family)) %>%
#   filter(site_target_name == focal_target) %>%
#   group_by(survey_year, family) %>%
#   ggplot(., aes(x = as.factor(survey_year), y = quadrat_count_of_species)) +
#   facet_wrap(~family) +
#   geom_boxplot() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   labs (x = "Year") +
#   ggtitle(paste("Taxonomic family frequency -", focal_target))


# Family relative frequency boxplot
veg_qdt_species %>%
  filter(!is.na(family)) %>%
  group_by(survey_year, family) %>%
  ggplot(., aes(x = as.factor(survey_year), y = rel_freq)) +
  scale_y_continuous(labels = scales::percent, limits = c(0,.25)) +
  facet_wrap(~family) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs (x = "Year", y = "Frequency") +
  ggtitle("Taxonomic family relative frequency")

# Family relative abundance boxplot for target of interest
# veg_qdt_species %>%
#   filter(!is.na(family)) %>%
#   filter(site_target_name == focal_target) %>%
#   group_by(survey_year, family) %>%
#   ggplot(., aes(x = as.factor(survey_year), y = rel_freq)) +
#   facet_wrap(~family) +
#   geom_boxplot() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   labs (x = "Year") +
#   ggtitle(paste("Taxonomic family relative frequency -", focal_target))



### Native species

## Relative freq

# Boxplot
veg_qdt_native %>%
  filter(native == "Native") %>%
  ggplot(., aes(x = as.factor(survey_year), y = rel_freq)) +
  scale_y_continuous(labels = scales::percent) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs (x = "Year", y = "Relative Frequency") +
  ggtitle("Relative frequency of native species")


# Dotplot with trendline
veg_qdt_native %>%
  filter(native == "Native") %>%
  ggplot(aes(x = survey_date, y = rel_freq, xmin = date_filter_start, xmax = date_filter_end)) +
  scale_y_continuous(labels = scales::percent) +
  geom_smooth(method = "lm") +
  geom_point() +
  theme_classic() +
  labs (x = "Date", y = "Relative Frequency") +
  ggtitle("Relative frequency of native species")

# Dotplot with trendline - target of interest
veg_qdt_native %>%
  filter(native == "Native") %>%
  filter(site_target_name %in% focal_target) %>%
  ggplot(aes(x = survey_date, y = rel_freq, xmin = date_filter_start, xmax = date_filter_end)) +
  scale_y_continuous(labels = scales::percent) +
  geom_smooth(method = "lm") +
  geom_point() +
  theme_classic() +
  labs (x = "Date", y = "Relative Frequency") +
  ggtitle("Relative frequency of native species", subtitle = paste0(project_name, " - ", focal_target))




### Site

# Warning - change over time here may only indicate difference in survey effort

# Site observed species richness dotplot
veg_qdt_site %>%
  group_by(survey_year, site_id) %>%
  ggplot(., aes(x = survey_year, y = obs_richness)) +
  facet_wrap(~site_id) +
  geom_point() +
  geom_line() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs (x = "Year", y = "Species richness") +
  ggtitle("Site observed species richness")

# Target observed species richness boxplot
veg_qdt_site %>%
  group_by(survey_year, site_target_name) %>%
  ggplot(., aes(x = as.factor(survey_year), y = obs_richness)) +
  facet_wrap(~site_target_name) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs (x = "Year", y = "Species richness") +
  ggtitle("Site observed species richness")

