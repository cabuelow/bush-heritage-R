# Title: EOM Vegetation Quadrat
# Author details: Gareth Davies
# Script and data info: This script loads, edits, visualises and returns statistics for vegetation cover methods:
# - M06.080 Vegetation Quadrat
# - M06.071 Tree Health Quadrat?
# Data obtained from Sharepoint exports of data hosted in the Echo portal

# Libraries
install.packages("here")
install.packages("janitor")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("taxize")
install.packages("galah")
install.packages("vegan")

library(here)
library(janitor)
library(tidyverse) 
library(lubridate)
library(taxize)
library(galah)
library(vegan)



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

Echo_M06080_project %>%
  filter(site_id == "SCT001") %>%
  View()

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

# Echo data does not currently provide categories by which to group species - e.g. genus, NVIS growthform, indigeneity/weediness.
# galah (an R package interface to Atlas of Living Australia data) can provide taxonomic family data and may have some advantages
# over solutions developed outside Australia, but seems to encounter problems with homonyms.
# The R package taxize can be used to find taxonomic family from a recorded scientific name via scraping of The Plant List
# data, but is slow.
# It may be useful to view density and relative abundance over time for all species within a site target of interest,
# and for individual species.


### Species

## Relative abundance
# Create simplified species data table
veg_qdt_species <- Echo_M06080_project %>%
  select(site_target_name, site_id, survey_year, survey_date, scientific_name, quadrat_count_of_species) %>%
  filter(!is.na(quadrat_count_of_species))

# Calculate relative abundance
veg_qdt_species <- veg_qdt_species %>%
  group_by(site_target_name, site_id, survey_year, survey_date) %>%
  mutate(rel_abund = quadrat_count_of_species / sum(quadrat_count_of_species))

veg_qdt_species %>% glimpse()


### Taxonomic family

# Using package galah, scrape the Atlas of Living Australia to find taxonomic family from recorded scientific name, if possible.
veg_qdt_species <- veg_qdt_species %>%
  rowwise() %>%
  mutate(family = if_else(is.null(select_taxa(scientific_name)$family), NA, select_taxa(scientific_name)$family))

# Using package taxize, fill any gaps in galah outputs
veg_qdt_species <- veg_qdt_species %>%
  rowwise() %>%
  mutate(family = if_else(is.na(family), plantminer(scientific_name, messages = FALSE)$family, family))

# taxize leaves null string results if it can't find a match - replace these with NA
veg_qdt_species <- veg_qdt_species %>%
  mutate(family = na_if(family, ""))

# # Alternatively, the order of application could be reversed:
# # Using package taxize, scrape web database to add taxonomic family from recorded scientific name, if possible.
# veg_qdt_species <- veg_qdt_species %>%
#   mutate(family = plantminer(scientific_name, messages = FALSE)$family)
# # galah could also be applied as a second step after a first pass of taxize:
# veg_qdt_species <- veg_qdt_species %>%
#   mutate(family = ifelse(family == "", ifelse(is.null(select_taxa(scientific_name)$family), NA, select_taxa(scientific_name)$family), family))

# Summarise observations per family (check NAs - these may need further investigation)
veg_qdt_species %>%
  group_by(family) %>%
  summarise(count = n()) %>%
  print(n = nrow(.))

# Create simplified family data table
veg_qdt_family <- veg_qdt_species %>%
  group_by(site_target_name, site_id, survey_year, survey_date, family) %>%
  summarise(quadrat_count_of_family = sum(quadrat_count_of_species), n = n())

# Calculate relative abundance
veg_qdt_family <- veg_qdt_family %>%
  group_by(site_target_name, site_id, survey_date) %>%
  mutate(rel_abund = quadrat_count_of_family / sum(quadrat_count_of_family))


### Quadrat

## Quadrat species richness
# Species richness is the number of species observed at a site.

# Create quadrat table and calculate species richness
veg_qdt <- veg_qdt_species %>%
  filter(scientific_name != "Unrecorded") %>%
  group_by(site_target_name, site_id, survey_year, survey_date) %>%
  summarise(species_richness = n())

## Site species diversity
# Several metrics are available for ordination of diversity. I've picked two to include here:
# - The Shannon diversity (aka Shannon-Weiner diversity) index is a metric relating to the amount of disorder present in a dataset,
#   with a basis in information theory and entropy
# - The Simpson diversity index used here (1 - D, the arithmetic complement of the Simpson dominance index) returns the probability
#   that two randomly selected individuals belong to different species.
# Also possible with the vegan package diversity function, but not included for now, are inverse Simpson (1/D) and Fisher metrics.

# Create matrix-format dataset required by vegan
veg_qdt_species_pivot <- veg_qdt_species %>%
  filter(scientific_name != "Unrecorded") %>%
  select(site_target_name, site_id, survey_year, survey_date, scientific_name, quadrat_count_of_species) %>%
  pivot_wider(., names_from = scientific_name, values_from = quadrat_count_of_species, values_fill = 0)

# For each diversity metric, calculate per quadrat, convert to a single-column tibble, and add to the quadrat table
veg_qdt <- veg_qdt %>%
  add_column(as_tibble_col(diversity(veg_qdt_species_pivot[,-(1:4)]), column_name = "species_diversity_shannon"))

veg_qdt <- veg_qdt %>%
  add_column(as_tibble_col(diversity(veg_qdt_species_pivot[,-(1:4)], index = "simpson"), column_name = "species_diversity_simpson"))


#####
# Plots for report
#####

# Optional: define a focal target of interest. Change manually as required.
# focal_target <- "Grassy Box-Gum Woodlands"


## Subset of species - one or several

# Define species (single or multiple) of interest
focal_species <- c("Themeda triandra", "Rytidosperma sp.")
# focal_target <- "Grassy Box-Gum Woodlands"

# Species density
veg_qdt_species %>%
  filter(scientific_name %in% focal_species) %>%
  group_by(survey_date, scientific_name) %>%
  ggplot(., aes(x = survey_date, y = quadrat_count_of_species)) +
  facet_wrap(~scientific_name) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_classic() +
  labs(x="Survey date", y="Species density") +
  ggtitle(label = "Species density", subtitle = paste0(focal_species,collapse=", "))

# Species density for a focal target of interest
# veg_qdt_species %>%
#   filter(scientific_name %in% focal_species) %>%
#   filter(site_target_name %in% focal_target) %>%
#   group_by(survey_date, scientific_name) %>%
#   ggplot(., aes(x = survey_date, y = quadrat_count_of_species)) +
#   facet_wrap(~scientific_name) +
#   geom_point() +
#   geom_smooth(method = lm) +
#   theme_classic() +
#   labs(x="Survey date", y="Species density") +
#   ggtitle(label = paste0("Species density - ",focal_target), subtitle = paste0(focal_species,collapse=", "))


# Relative abundance
veg_qdt_species %>%
  filter(scientific_name %in% focal_species) %>%
  group_by(survey_date, scientific_name) %>%
  ggplot(., aes(x = survey_date, y = rel_abund)) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~scientific_name) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_classic() +
  labs(x="Survey date", y="Relative abundance") +
  ggtitle(label = "Relative abundance", subtitle = paste0(focal_species, collapse=", "))

# Relative abundance for a focal target of interest
# veg_qdt_species %>%
#   filter(scientific_name %in% focal_species) %>%
#   filter(site_target_name %in% focal_target) %>%
#   group_by(survey_date, scientific_name) %>%
#   ggplot(., aes(x = survey_date, y = rel_abund)) +
#   scale_y_continuous(labels = scales::percent) +
#   facet_wrap(~scientific_name) +
#   geom_point() +
#   geom_smooth(method = lm) +
#   theme_classic() +
#   labs(x="Survey date", y="Relative abundance") +
#   ggtitle(label = paste0("Relative abundance - ",focal_target), subtitle = paste0(focal_species, collapse=", "))




## All species

# Plot species density over time
veg_qdt_species %>%
  group_by(survey_year, scientific_name) %>%
  ggplot(., aes(x = as.factor(survey_year), y = quadrat_count_of_species)) +
  facet_wrap(~scientific_name) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs (x = "Year") +
  ggtitle("Species density")

# Species density over time for a focal target of interest
# veg_qdt_species %>%
#   filter(site_target_name %in% focal_target) %>%
#   group_by(survey_year, scientific_name) %>%
#   ggplot(., aes(x = as.factor(survey_year), y = quadrat_count_of_species)) +
#   facet_wrap(~scientific_name) +
#   geom_boxplot() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   labs (x = "Year") +
#   ggtitle(paste("Species density -", focal_target))

# Plot relative abundance over time
veg_qdt_species %>%
  filter(scientific_name != "Unrecorded") %>%
  # filter(site_target_name %in% focal_target) %>%
  group_by(survey_year, scientific_name) %>%
  ggplot(., aes(x = as.factor(survey_year), y = rel_abund)) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~scientific_name) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs (x = "Year", y = "Relative abundance") +
  ggtitle("Species relative abundance")

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

# Family density boxplot
veg_qdt_family %>%
  filter(!is.na(family)) %>%
  group_by(survey_year, family) %>%
  ggplot(., aes(x = as.factor(survey_year), y = quadrat_count_of_family)) +
  facet_wrap(~family) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs (x = "Year") +
  ggtitle("Taxonomic family density")

# Family density boxplot for target of interest
# veg_qdt_family %>%
#   filter(!is.na(family)) %>%
#   filter(site_target_name == focal_target) %>%
#   group_by(survey_year, family) %>%
#   ggplot(., aes(x = as.factor(survey_year), y = quadrat_count_of_family)) +
#   facet_wrap(~family) +
#   geom_boxplot() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   labs (x = "Year") +
#   ggtitle(paste("Taxonomic family density -", focal_target))


# Family relative abundance boxplot
veg_qdt_family %>%
  filter(!is.na(family)) %>%
  group_by(survey_year, family) %>%
  ggplot(., aes(x = as.factor(survey_year), y = rel_abund)) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~family) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs (x = "Year") +
  ggtitle("Taxonomic family relative abundance")

# Family relative abundance boxplot for target of interest
# veg_qdt_family %>%
#   filter(!is.na(family)) %>%
#   filter(site_target_name == focal_target) %>%
#   group_by(survey_year, family) %>%
#   ggplot(., aes(x = as.factor(survey_year), y = rel_abund)) +
#   facet_wrap(~family) +
#   geom_boxplot() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   labs (x = "Year") +
#   ggtitle(paste("Taxonomic family relative abundance -", focal_target))



## Site measures - species richness

# Species richness observations and linear regression modelled trend over time
veg_qdt %>%
  ggplot(aes(x = survey_date, y = species_richness)) +
  facet_wrap(~site_target_name) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_classic() +
  labs(x = "Year", y = "Species richness")

# Species richness boxplots
veg_qdt %>%
  ggplot(aes(x = as.factor(survey_year), y = species_richness)) +
  facet_wrap(~site_target_name) +
  geom_boxplot() +
  theme_classic() +
  labs(x = "Year", y = "Species richness")


## Site measures - species diversity

# Species diversity observations and linear regression modelled trend over time
# Shannon index
veg_qdt %>%
  ggplot(aes(x = survey_date, y = species_diversity_shannon)) +
  facet_wrap(~site_target_name) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_classic() +
  labs(x = "Survey date", y = "Species diversity (Shannon index)")

# Simpson index
veg_qdt %>%
  ggplot(aes(x = survey_date, y = species_diversity_simpson)) +
  facet_wrap(~site_target_name) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_classic() +
  labs(x = "Survey date", y = "Species diversity (Simpson index)")

# Species diversity boxplot
# Shannon index
veg_qdt %>%
  ggplot(aes(x = as.factor(survey_year), y = species_diversity_shannon)) +
  facet_wrap(~site_target_name) +
  geom_boxplot() +
  theme_classic() +
  labs(x = "Year", y = "Species diversity (Shannon index)")

# Simpson index
veg_qdt %>%
  ggplot(aes(x = as.factor(survey_year), y = species_diversity_simpson)) +
  facet_wrap(~site_target_name) +
  geom_boxplot() +
  theme_classic() +
  labs(x = "Year", y = "Species diversity (Simpson index)")




#####
# Statistical analysis
#####

# Could be used for hypothesis testing, assessment of statistical power of available project data, introduction of covariates, etc.


## Simple linear model

# Example - change in species diversity over time for a focal target of interest

# Build dataset for model
# Simpson index, Grassy Box-Gum Woodlands

lm_data <- veg_qdt %>%
  filter(site_target_name == "Grassy Box-Gum Woodlands")

lm_data %>% glimpse()

# Initial visualisation
lm_data %>%
  ggplot(aes(x = survey_date, y = species_diversity_simpson)) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_classic() +
  labs(x = "Survey date", y = "Species diversity (Simpson index)")


# Generate linear model
# Assumptions: linearity, independence, normality, equal variance (homoscedasticity)
lm_output <- lm(species_diversity_simpson ~ survey_date, data = lm_data)


## Summary statistics
# Key statistics of interest are the t-value, p-value (Pr(>|t|)), and degrees of freedom
# - The t statistic indicates the size of difference relative to variation in sample data
# - The p-value records the probability of obtaining a value with a distance from the distribution peak greater than or equal to t
summary(lm_output)


### Standard linear model diagnostics

# Display all diagnostic plots
par(mfrow = c(2,2))
plot(lm_output)
par(mfrow=c(1,1))

## Assess linearity and equality of variance
# Residuals vs fitted
# - mean should be ~0, evenly spread, lack of obvious pattern
plot(lm_output, 1)
# Scale-location plot
# - line should be approximately horizontal
# - spread around red line shouldn't vary with fitted values
plot(lm_output, 3)

## Assess normality of errors
# Histogram of residuals
# - should display a normal distribution
hist(resid(lm_output))
# Normal quantile-quantile plot (aka Normal Q-Q)
# - plotted values should follow the dotted line
plot(lm_output, 2)
# Shapiro-Wilk normality test
# - p-value should be relatively high in order to permit us to reject the null hypothesis of a lack of normality
# - e.g. a significance level of .05
shapiro.test(resid(lm_output))

# Test failed? It may help to re-run with a transformed response variable, e.g. log-transform, and repeat these diagnostics
# lm_output <- lm(log(species_diversity_simpson) ~ survey_date, data = lm_data)
# If the test still fails, try other transforms or change approach


## Assess outliers and points with high leverage
# Residuals vs leverage
# - Spread of residuals shouldn't change as a function of leverage
# - Values with high Cook's distance (e.g. outside the dotted red line) are highly influential on the model - may represent errors
plot(lm_output,5)


# Further interrogation of outliers/influential points:
# Record labels with high Cook's distance from residuals vs. leverage plot
outlier_labels <- c(4, 9)

# Find these data points in vgt_qdt
lm_data[outlier_labels,]

# Find species observations which produced these data points
veg_qdt[outlier_labels,] %>%
  select(site_target_name, site_id, survey_year, survey_date) %>%
  left_join(veg_qdt_species) %>%
  print(n=nrow(.))



