# Title: Vegetation Transect
# Author details: Justin McCann, Gareth Davies
# Script and data info: This script loads, pre-processes, and visualises data for vegetation cover method:
# - M06.090 Vegetation Transect
# Data obtained from Sharepoint exports out of Echo

# The following output tables can be produced by this script:
# - Echo_M06090_project contains raw M06.090 data for the selected project and date range
# - veg_tsc_all contains processed raw data, including intercept start and finish distances per observation
# - veg_tsc_all_names contains taxonomic information and native/introduced status (from GBIF and GRIIS Australia) 
#    for distinct names recorded in veg_tsc_all
# - veg_tsc_species contains total observed cover per site, survey, stratum and species
# - veg_tsc_family contains total observed cover (excluding overlap) per site, survey, stratum and taxonomic family
# - veg_tsc_strata contains total observed cover per site, survey and stratum
# - veg_tsc_transect contains total observed cover (excluding overlap) per site and survey

# This script is designed to be run interactively - select and run lines based on your requirements.
# The script is not designed to be run end-to-end.



#####
# Libraries
#####

# Install if required

install.packages("rgbif")
install.packages("lubridate")


# Libraries
library(here)
library(janitor)
library(tidyverse) 
library(readxl)
library(lubridate)

# library(galah)
# library(taxize)
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
# Import the data file (left join to site information could be added here)
# Manually specify column types to avoid CSV miscasts
Echo_M06090 <- read_csv("Data/M06_090_Vegetation_Intercept_Analysis_Join_Parent_Child.csv",
                        col_types="icccccccccdddicccccddccccccicccccccccc") %>%
  clean_names() %>%
  mutate(survey_year = year(dmy_hms(created_date_local_adjusted)),
         survey_date = date(dmy_hms(created_date_local_adjusted)))

# Create a filtered set of observations for the identified project
Echo_M06090_project <- Echo_M06090 %>%
  filter(prj_cde == project_code_for_data_analysis) %>%
  filter(survey_date >= date_filter_start) %>% # Start date of interest
  filter(survey_date < date_filter_end) # End date of interest

Echo_M06090_project %>% glimpse()

# Remove Echo_M06090 from memory
rm(Echo_M06090)


#####
# Preprocessing
#####

# Plot frequency of species observations - check for high numbers of 'NA' or 'Unrecorded'
ggplot(data = Echo_M06090_project) + geom_bar(mapping = aes(x = fct_rev(scientific_name))) + coord_flip()

# Plot frequency of 'NA' and 'Unrecorded' species observations over time
Echo_M06090_project %>%
  filter(scientific_name == "Unrecorded" | is.na(scientific_name)) %>%
  group_by(survey_date) %>%
  summarise(count = n()) %>%
  ggplot(., aes(x = survey_date, y=count)) +
  geom_point()

# Plot frequency of valid species observations over time
Echo_M06090_project %>%
  filter(scientific_name != "Unrecorded" & !is.na(scientific_name)) %>%
  group_by(survey_date) %>%
  summarise(count = n()) %>%
  ggplot(., aes(x = survey_date, y=count)) +
  geom_point()

Echo_M06090_project %>%
  filter(scientific_name != "Unrecorded" & !is.na(scientific_name)) %>%
  group_by(survey_year) %>%
  summarise(count = n()) %>%
  ggplot(., aes(x = survey_year, y=count)) +
  geom_point()



## If invalid count seems high 
Echo_M06090_project %>%
  # filter(scientific_name == "Unrecorded" | is.na(scientific_name)) %>%
  filter(scientific_name == "Unrecorded") %>%
  ggplot() + geom_bar(mapping = aes(x = fct_rev(created_user))) + coord_flip()

Echo_M06090_project %>%
  filter(scientific_name == "Unrecorded" | is.na(scientific_name)) %>%
  count(created_user, scientific_name, sort = TRUE)

# Check sample count per stratum
Echo_M06090_project %>%
  group_by(intercept_strata) %>%
  summarise(n = n())

# Check sample count per species
Echo_M06090_project %>%
  count(scientific_name, sort = TRUE)


## Placeholder: Growthform pre-processing could occur here if/when supported in Echo data

## Placeholder: do we need to deal with varying stratum height per survey?




#####
# Calculate metrics
#####

### Create data table of all observations

Echo_M06090_project <- Echo_M06090_project %>%
  mutate(cover = (finish - start) / length_m)

veg_tsc_all <- Echo_M06090_project %>%
  filter(!is.na(intercept_strata)) %>%
  select(site_target_name, site_id, survey_year, survey_date, stratum = intercept_strata, scientific_name, english_name, start, finish, length_m, cover)

# Remove Echo_M06090_project from memory
# rm(Echo_M06090_project)


### Find taxonomic family

# The R package galah (an interface to Atlas of Living Australia data) can provide taxonomic family from a recorded
# scientific name, but seems to encounter problems with homonyms.
# The R package taxize can be used to find taxonomic family from a recorded scientific name via scraping of The Plant List
# data, but is slow.
# It may be useful to view density and relative abundance over time for all species within a site target of interest,
# and for individual species.

# Using package galah, scrape the Atlas of Living Australia to find taxonomic family from recorded scientific name, if possible.
# veg_tsc_all <- veg_tsc_all %>%
#   rowwise() %>%
#   mutate(family = ifelse((!is.na(scientific_name) & scientific_name != "Unrecorded" & !str_detect(scientific_name, regex("unidentified", ignore_case = TRUE))),
#                          ifelse(is.null(galah_family <- search_taxa(scientific_name)$family), NA, galah_family), NA))
# 
# # Using package taxize, fill any gaps in galah outputs
# # taxize leaves null string results if it can't find a match - need to replace these with NA
# veg_tsc_all <- veg_tsc_all %>%
#   rowwise() %>%
#   mutate(family = ifelse((!is.na(scientific_name) & scientific_name != "Unrecorded" & !str_detect(scientific_name,regex("unidentified", ignore_case = TRUE))),
#                          ifelse(is.na(family), na_if(plantminer(scientific_name, messages = FALSE)$family, ""), family), NA))
# 
# # Replace all NAs in family with "Unknown"
# veg_tsc_all <- veg_tsc_all %>%
#   mutate(family = replace_na(family, "Unknown"))
# 
# # View all values of family
# veg_tsc_all %>%
#   group_by(family) %>%
#   tally()


# Using package rgbif, parse recorded scientific names in the Global Biodiversity Information Framework and find
# whether they can be found in the Global Register of Introduced and Invasive Species checklist for Australia.
# GRIIS Australia datasetKey = 15147db1-27c3-49b5-9c69-c78d55a4b8ff

# Extract observed scientific names
veg_tsc_all_names <- veg_tsc_all %>%
  filter(!is.na(scientific_name)) %>%
  distinct(scientific_name)

# Return taxonomic information from the GBIF backbone taxonomy
veg_tsc_all_names <- veg_tsc_all_names %>%
  rowwise() %>%
  mutate(gbif = list(name_backbone(scientific_name))) %>%
  unnest(cols = c(gbif)) %>%
  select(scientific_name, rank, status, kingdom, phylum, order, family, genus, species)

# Identify native and non-native species, with reference to the GRIIS Australia dataset
veg_tsc_all_names <- veg_tsc_all_names %>%
  rowwise() %>%
  mutate(native = ifelse(rank == "SPECIES", ifelse(name_lookup(species, datasetKey = "15147db1-27c3-49b5-9c69-c78d55a4b8ff")$meta$count > 0, FALSE, TRUE), NA))

# Join with veg_tsc_all
veg_tsc_all <- veg_tsc_all %>%
  left_join(veg_tsc_all_names)


### Create species table

veg_tsc_species <- veg_tsc_all %>%
  group_by(site_target_name, site_id, survey_year, survey_date, stratum, family, genus, species, native) %>%
  summarise(cover = sum(cover), n = n())


### Create native species cover table
# Needs to exclude potential overlaps within a family, but otherwise preserve separate observations prior to summarisation

# Remove grouping applied by rowwise()
veg_tsc_all <- ungroup(veg_tsc_all)

veg_tsc_all <- veg_tsc_all %>%
  arrange(site_target_name, site_id, survey_year, survey_date, stratum, start, finish)

veg_tsc_native <- veg_tsc_all %>%
  filter(native == TRUE) %>%
  arrange(site_target_name, site_id, survey_year, survey_date, stratum, native, start) %>%
  mutate(overlap_with_prev = if_else(start < dplyr::lag(finish, default = FALSE) &
                                       site_id == lag(site_id) &
                                       survey_date == lag(survey_date) &
                                       stratum == lag(stratum) &
                                       native == lag(native),
                                     TRUE, FALSE)) %>%
  mutate(overlap_with_next = if_else(finish > dplyr::lead(start, default = FALSE) &
                                       site_id == lead(site_id) &
                                       survey_date == lead(survey_date) &
                                       stratum == lead(stratum) &
                                       native == lead(native),
                                     TRUE, FALSE)) %>%
  # If a row overlaps with both neighbours, exclude it
  filter(overlap_with_prev == FALSE | overlap_with_next == FALSE) %>%
  # If a row overlaps with the one after it, update its finish to that of the neighbour, then remove the neighbour;
  # otherwise leave as-is.
  # (works with spans of overlap across multiple observations because we've already excluded the middle ones)
  mutate(finish = ifelse(overlap_with_next, lead(finish), finish)) %>%
  filter(overlap_with_prev == FALSE) %>%
  # Select only necessary columns (i.e. drop scientific_name and temporary ones)
  select(site_target_name, site_id, survey_year, survey_date, stratum, native, start, finish, length_m) %>%
  # Recalculate cover for all rows
  mutate(cover = (finish - start) / length_m, veg_present = TRUE) %>%
  # Find total cover per survey, stratum and family
  group_by(site_target_name, site_id, survey_year, survey_date, stratum, native) %>%
  summarise(cover = sum(cover), n = n())

# Find rows which overlap with neighbours of the same survey, stratum and family
# veg_tsc_family <- veg_tsc_all %>%
#   arrange(site_target_name, site_id, survey_year, survey_date, stratum, family, start) %>%
#   mutate(overlap_with_prev = if_else(start < dplyr::lag(finish, default = FALSE) &
#                                        site_id == lag(site_id) &
#                                        survey_date == lag(survey_date) &
#                                        stratum == lag(stratum) &
#                                        family == lag(family),
#                                      TRUE, FALSE)) %>%
#   mutate(overlap_with_next = if_else(finish > dplyr::lead(start, default = FALSE) &
#                                        site_id == lead(site_id) &
#                                        survey_date == lead(survey_date) &
#                                        stratum == lead(stratum) &
#                                        family == lead(family),
#                                      TRUE, FALSE)) %>%
#   # If a row overlaps with both neighbours, exclude it
#   filter(overlap_with_prev == FALSE | overlap_with_next == FALSE) %>%
#   # If a row overlaps with the one after it, update its finish to that of the neighbour, then remove the neighbour;
#   # otherwise leave as-is.
#   # (works with spans of overlap across multiple observations because we've already excluded the middle ones)
#   mutate(finish = ifelse(overlap_with_next, lead(finish), finish)) %>%
#   filter(overlap_with_prev == FALSE) %>%
#   # Select only necessary columns (i.e. drop scientific_name and temporary ones)
#   select(site_target_name, site_id, survey_year, survey_date, stratum, family, start, finish, length_m) %>%
#   # Recalculate cover for all rows
#   mutate(cover = (finish - start) / length_m, veg_present = TRUE) %>%
#   # Find total cover per survey, stratum and family
#   group_by(site_target_name, site_id, survey_year, survey_date, stratum, family) %>%
#   summarise(cover = sum(cover), n = n())



### Create stratum table
# Since observations are per stratum, we can obtain totals by summing observed cover in each

veg_tsc_strata <- veg_tsc_species %>%
  group_by(site_target_name, site_id, survey_year, survey_date, stratum) %>%
  summarise(cover = sum(cover), n = n())



### Create total vegetative cover table for each transect (e.g. cover visible from above,
# for comparison with remote sensing data)
# Includes some wrangling to facilitate calculation of the average distance between vegetated sections 
# and average section length. Could convert to function?

veg_tsc_transect <- veg_tsc_all %>%
  
  # First, identify surveys which have a row where start == 0
  filter(start == 0) %>%
  distinct(site_target_name, site_id, survey_year, survey_date) %>%
  
  # Use an inverted join to find all unique surveys which DON'T meet this criterion
  anti_join(veg_tsc_all, .) %>%
  distinct(site_target_name, site_id, survey_year, survey_date) %>%
  
  # Add zero start and finish values and an identifying flag for these rows
  mutate(start = 0, finish = 0, veg_observed = FALSE) %>%
  
  # Merge with a tibble from which subsumed observations have been excluded
  full_join(
    veg_tsc_all %>%
      # Group to avoid rollover between transects
      group_by(site_target_name, site_id, survey_year, survey_date) %>%
      # Arrange for sort order
      arrange(site_target_name, site_id, survey_year, survey_date, start) %>%
      # Find cumulative minima and maxima
      mutate(min_start = order_by(desc(start), cummin(start))) %>%
      mutate(max_finish = cummax(finish)) %>% # View()
      # Condense to exclude subsumed observations
      group_by(site_target_name, site_id, survey_year, survey_date, length_m, max_finish) %>%
      summarise(start = min(min_start)) %>%
      # Reorder columns
      select(site_target_name, site_id, survey_year, survey_date, start, finish = max_finish, length_m)
  ) %>% 

  # Include the zero-start rows in our sort order
  arrange(site_target_name, site_id, survey_year, survey_date, start) %>%
  
  # Fill missing values of veg_observed
  mutate(veg_observed = replace_na(veg_observed, TRUE)) %>%

  # Look for veg observations which overlap with or adjoin neighbours and can be merged
  mutate(overlap_with_prev = if_else(
    replace_na(veg_observed == TRUE & start <= lag(finish) & site_id == lag(site_id) & survey_date == lag(survey_date), FALSE),
    TRUE,
    FALSE)) %>%
  mutate(overlap_with_next = if_else(
    replace_na(veg_observed == TRUE & finish >= lead(start) & site_id == lead(site_id) & survey_date == lead(survey_date), FALSE),
    TRUE,
    FALSE)) %>% # View()
  
  # If a row overlaps with both neighbours, exclude it
  filter(overlap_with_prev == FALSE | overlap_with_next == FALSE) %>%
  
  # If a row overlaps with the next, update the finish distance of the first, and exclude the second
  mutate(finish = ifelse(overlap_with_next, lead(finish), finish)) %>%
  filter(overlap_with_prev == FALSE) %>% 

  # Calculate distance between sections
  mutate(dist_to_next = ifelse(
    replace_na(site_id == lead(site_id) & survey_date == lead(survey_date), FALSE),
    lead(start) - finish,
    ifelse(finish == length_m, NA, length_m - finish))) %>% # View()
    
  # Recalculate cover for all rows except the zero-start ones we added
  mutate(cover = ifelse(veg_observed == TRUE, (finish - start) / length_m, NA)) %>%
    
  # Select only necessary columns
  select(site_target_name, site_id, survey_year, survey_date, start, finish, dist_to_next, length_m, cover, veg_observed) %>%
  
  # Find total cover per transect
  group_by(site_target_name, site_id, survey_year, survey_date) %>%
  summarise(total_cover = sum(cover, na.rm = TRUE), avg_cover = mean(cover, na.rm = TRUE), avg_gap_length = mean(dist_to_next, na.rm = TRUE), avg_gap_pct = mean(dist_to_next/length_m, na.rm = TRUE), n_sections = sum(veg_observed == TRUE)) %>%
  
  # Overwrite avg_gap_size values to zero where NaN (possible if observed vegetation forms a large, unbroken section)
  mutate(avg_gap_length = ifelse(is.nan(avg_gap_length), 0, avg_gap_length), avg_gap_pct = ifelse(is.nan(avg_gap_pct), 0, avg_gap_pct))






#####
# Create plots for report
#####
# functions can be found at https://rstudio.com/resources/cheatsheets/. For this process we want "Data Visualization"
# also https://www.statmethods.net/graphs/index.html


### Subset of species - one or several

## Define species (single or multiple) of interest
focal_species <- "Eucalyptus bridgesiana"
focal_species <- c("Eucalyptus bridgesiana", "Eucalyptus melliodora")

focal_stratum <- "Groundcover"

focal_target <- "Grassy Box-Gum Woodlands"


## Species cover across all strata

# Dotplot with generalised linear model trendline
veg_tsc_species %>%
  filter(species %in% focal_species) %>%
  ggplot(., aes(x = survey_date, y = cover)) +
  facet_wrap(~species) +
  geom_point() +
  geom_smooth(method = glm) +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Survey date", y = "Cover") +
  ggtitle(label = "Observed cover per species")

# Boxplot
veg_tsc_species %>%
  filter(species %in% focal_species) %>%
  ggplot(., aes(x = as_factor(survey_year), y = cover)) +
  facet_wrap(~species) +
  geom_boxplot() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Survey year", y = "Cover") +
  ggtitle(label = "Observed cover per species")



## Species cover per stratum

# Dotplot with generalised linear model trendline
veg_tsc_species %>%
  filter(scientific_name %in% focal_species) %>%
  ggplot(., aes(x = survey_date, y = cover)) +
  facet_wrap(~stratum) +
  geom_point() +
  geom_smooth(method = glm) +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Survey date", y = "Cover") +
  ggtitle(label = "Observed cover per species - ", subtitle = paste0(focal_species, collapse = ", "))

# Boxplot
veg_tsc_species %>%
  filter(scientific_name %in% focal_species) %>%
  ggplot(., aes(x = as_factor(survey_year), y = cover)) +
  facet_wrap(~stratum) +
  geom_boxplot() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Survey year", y = "Cover") +
  ggtitle(label = "Observed cover per species - ", subtitle = paste0(focal_species, collapse = ", "))



## Species cover per stratum (multiple species)

# Dotplot with generalised linear model trendline
veg_tsc_species %>%
  filter(scientific_name %in% focal_species) %>%
  ggplot(., aes(x = survey_date, y = cover, colour = scientific_name)) +
  facet_wrap(~stratum) +
  geom_smooth(method = glm) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Survey date", y = "Cover") +
  ggtitle(label = "Observed cover per species - ", subtitle = paste0(focal_species, collapse = ", "))

# Boxplot
veg_tsc_species %>%
  filter(scientific_name %in% focal_species) %>%
  ggplot(., aes(x = as_factor(survey_year), y = cover, colour = scientific_name)) +
  facet_wrap(~stratum) +
  geom_boxplot() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Survey year", y = "Cover") +
  ggtitle(label = "Observed cover per species - ", subtitle = paste0(focal_species, collapse = ", "))



## Species cover per stratum for focal species and target of interest
# (Note: does not distinguish between species if focal_species includes more than one value)

# Dotplot with generalised linear model trendline
veg_tsc_species %>%
  filter(scientific_name %in% focal_species) %>%
  filter(site_target_name %in% focal_target) %>%
  ggplot(., aes(x = survey_date, y = cover)) +
  facet_wrap(~stratum) +
  geom_smooth(method = glm) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Survey date", y = "Cover") +
  ggtitle(label = paste0("Observed cover per species - ", paste0(focal_target, collapse = ", ")), subtitle = paste0(focal_species, collapse = ", "))

# Boxplot
veg_tsc_species %>%
  filter(scientific_name %in% focal_species) %>%
  ggplot(., aes(x = as_factor(survey_year), y = cover)) +
  facet_wrap(~stratum) +
  geom_boxplot() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Survey year", y = "Cover") +
  ggtitle(label = "Observed cover per species - ", subtitle = paste0(focal_species, collapse = ", "))



### Native species

# Native species across all strata, all targets - dotplot with generalised linear model trendline
veg_tsc_species %>%
  filter(native == TRUE) %>%
  ggplot(., aes(x = survey_date, y = cover)) +
  facet_wrap(~species) +
  geom_point() +
  geom_smooth(method = glm) +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Survey date", y = "Cover") +
  ggtitle(label = "Observed cover per species")

# Aggregate native species cover - by target, all strata
veg_tsc_native %>%
  ggplot(., aes(x = survey_date, y = cover, colour = stratum)) +
  facet_wrap(~site_target_name) +
  geom_point() +
  geom_smooth(method = glm) +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Survey date", y = "Cover") +
  ggtitle(label = "Observed cover of native species")

# Aggregate native species cover - all strata, for target of interest
veg_tsc_native %>%
  filter(site_target_name %in% focal_target) %>%
  ggplot(aes(x = survey_date, y = cover, colour = stratum, xmin = date_filter_start, xmax = date_filter_end)) +
  geom_point() +
  geom_smooth(method = glm) +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Survey date", y = "Cover") +
  ggtitle(label = "Observed cover of native species", subtitle = paste0(project_name, " - ", focal_target))

# Aggregate native species cover - by target, for stratum of interest
veg_tsc_native %>%
  filter(stratum %in% focal_stratum) %>%
  ggplot(., aes(x = survey_date, y = cover)) +
  facet_wrap(~site_target_name) +
  geom_point() +
  geom_smooth(method = glm) +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Survey date", y = "Cover") +
  ggtitle(label = "Observed cover of native species", subtitle = focal_stratum)

# Aggregate native species cover - for target and stratum of interest
veg_tsc_native %>%
  filter(stratum %in% focal_stratum & site_target_name %in% focal_target) %>%
  ggplot(., aes(x = survey_date, y = cover)) +
  facet_wrap(~site_target_name) +
  geom_point() +
  geom_smooth(method = glm) +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Survey date", y = "Cover") +
  ggtitle(label = "Observed cover of native species", subtitle = focal_stratum)



### All species 

# Species across all strata, dotplot with generalised linear model trendline
veg_tsc_species %>%
  filter(scientific_name != "Unrecorded" & !is.na(scientific_name)) %>%
  ggplot(., aes(x = survey_date, y = cover)) +
  facet_wrap(~scientific_name) +
  geom_point() +
  geom_smooth(method = glm) +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Survey date", y = "Cover") +
  ggtitle(label = "Observed cover per species")



### Taxonomic family

# Taxonomic family across all strata, with generalised linear model trendline
veg_tsc_family %>%
  filter(family != "Unknown") %>%
  ggplot(., aes(x = survey_date, y = cover)) +
  facet_wrap(~family) +
  geom_point() +
  geom_smooth(method = glm) +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Survey date", y = "Cover") +
  ggtitle(label = "Observed cover per taxonomic family")



### Stratum

# Per stratum across all focal targets, with glm trendline
veg_tsc_strata %>%
  ggplot(., aes(x = survey_date, y = cover)) +
  facet_wrap(~stratum) +
  geom_point() +
  geom_smooth(method = glm) +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  labs(x="Survey date", y="Cover") +
  ggtitle(label = "Observed cover per stratum")


# Per stratum for a focal target of interest, with glm trendline
veg_tsc_strata %>%
  filter(site_target_name %in% focal_target) %>%
  ggplot(., aes(x = survey_date, y = cover)) +
  facet_wrap(~stratum) +
  geom_point() +
  geom_smooth(method = glm) +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  labs(x="Survey date", y="Cover") +
  ggtitle(label = paste0("Observed cover per stratum - ", focal_target))


focal_target = "Grassy Box-Gum Woodlands"
focal_target = "Shrubby Eucalypt Woodland"


# For a stratum and focal target of interest, with glm trendline
veg_tsc_strata %>%
  filter(stratum %in% focal_stratum & site_target_name %in% focal_target) %>%
  ggplot(aes(x = survey_date, y = cover, xmin = date_filter_start, xmax = date_filter_end)) +
  geom_point(aes(colour = site_id)) +
  geom_smooth(method = glm) +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  labs(x="Survey date", y="Cover") +
  ggtitle(label = "Observed cover", subtitle = paste0(focal_stratum, ", ", focal_target))


# Per target for a stratum or strata of interest, with glm trendline

# Define stratum or strata of interest
focal_stratum <- c("Shrub-stratum", "Mid-stratum")
focal_stratum <- c("Shrub-stratum")
focal_stratum <- "Canopy"

veg_tsc_strata %>%
  filter(stratum %in% focal_stratum) %>%
  ggplot(., aes(x = survey_date, y = cover)) +
  facet_wrap(~site_target_name) +
  geom_point() +
  geom_smooth(method = glm) +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  labs(x="Survey date", y="Cover") +
  ggtitle(label = paste0("Observed cover per target - ", paste0(focal_stratum, collapse=", ")))

veg_tsc_strata %>%
  filter(stratum %in% focal_stratum) %>%
  group_by(site_target_name, survey_year) %>%
  summarise(cover_mean = mean(cover), cover_sd = sd(cover)) %>%
  ggplot(., aes(x = survey_year, y = cover_mean)) +
  facet_wrap(~site_target_name) +
  geom_point() +
  geom_errorbar(aes(ymin = cover_mean - cover_sd, ymax = cover_mean + cover_sd)) +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  labs(x="Survey date", y="Cover") +
  ggtitle(label = paste0("Observed cover per target - ", paste0(focal_stratum, collapse=", ")))




### Transect

## Total cover per target
# Dotplot of targets, with glm trendline
veg_tsc_transect %>%
  ggplot(., aes(x = survey_date, y = total_cover)) +
  facet_wrap(~site_target_name) +
  geom_point(aes(colour = site_id)) +
  geom_smooth(method = glm) +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  labs(x="Survey date", y="Cover") +
  ggtitle(label = "Observed total cover")

# Boxplot
veg_tsc_transect %>%
  ggplot(., aes(x = as_factor(survey_year), y = total_cover)) +
  facet_wrap(~site_target_name) +
  geom_boxplot() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  labs(x="Survey date", y="Cover") +
  ggtitle(label = "Observed total cover")

# Dotplot - single target
veg_tsc_transect %>%
  filter(site_target_name %in% focal_target) %>%
  ggplot(., aes(x = survey_date, y = total_cover, colour = site_id)) +
  geom_point() +
  geom_smooth(method = glm) +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  labs(x="Survey date", y="Cover") +
  ggtitle(label = "Observed total cover")


## Average cover per section per target
# Dotplot with glm trendline
veg_tsc_transect %>%
  ggplot(., aes(x = survey_date, y = avg_cover)) +
  facet_wrap(~site_target_name) +
  geom_point() +
  geom_smooth(method = glm) +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  labs(x="Survey date", y="Cover") +
  ggtitle(label = "Observed average section cover")

# Boxplot
veg_tsc_transect %>%
  ggplot(., aes(x = as_factor(survey_year), y = avg_cover)) +
  facet_wrap(~site_target_name) +
  geom_boxplot() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  labs(x="Survey date", y="Cover") +
  ggtitle(label = "Observed average section cover")


## Average section gap proportion (patchiness?) per target
# Dotplot with glm trendline
veg_tsc_transect %>%
  ggplot(., aes(x = survey_date, y = avg_gap_pct)) +
  facet_wrap(~site_target_name) +
  geom_point() +
  geom_smooth(method = glm) +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  labs(x="Survey date", y="Cover") +
  ggtitle(label = "Observed average section gap (%)")

# Boxplot
veg_tsc_transect %>%
  ggplot(., aes(x = as_factor(survey_year), y = avg_gap_pct)) +
  facet_wrap(~site_target_name) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::percent) +
  labs(x="Survey date", y="Cover") +
  ggtitle(label = "Observed average section gap (%)")



