# Title: Soil surface quadrat
# Author details: Justin McCann, Gareth Davies
# Script and data info: This script loads, pre-processes, and visualises data for soil surface method:
# - M04.020 Soil Surface Condition
# Data obtained from Sharepoint exports of data hosted in the Echo portal

# The following output tables can be produced by this script:
# - Echo_M04020_project contains raw M04.020 data for the selected project and date range
# - soil_qdt_all contains preprocessed M04.020 data, with common errors such as zero-inflation repaired where possible,
#    and includes observed cover estimates of bare ground; cryptogams; bryophytes, lichens and fungi; and litter.
# - soil_qdt_native contains data relating to projective cover of native and introduced species, where observed, in
#    crosstabular form
# - soil_qdt_native_cover makes soil_qdt_native data relating to observed native/introduced cover abundance plot-ready
# - soil_qdt_native_prop makes soil_qdt_native data relating to observed native/introduced cover proportion plot-ready

# This script is designed to be run interactively - select and run lines based on your requirements.
# The script is not designed to be run end-to-end.



# Libraries
library(here)
library(janitor)
library(tidyverse) 
library(lubridate)
# library(glue)



#####
# Define variables
#####

### Specify details of project and date range
project_code_for_data_analysis <- "SCOT"
project_name <- "Scottsdale Reserve"
date_filter_start <- as_date("2006-01-01")
date_filter_end <- as_date("2020-01-01")
# bom_rain_datafile <- "Data/IDCJAC0009_070348_1800_Data.csv"
# bom_station_id <- 070348



#####
# Data import
#####
# Import the data file (left join to site information could be added here)
# Manually specify column types to avoid CSV miscasts
Echo_M04020 <- read_csv("Data/M04_020_Soil_Surface_Quadrat_Analysis_Join_Parent_Child.csv",
                        col_types="icccccccccciiiiiiiiiiiiiiiiiddcccccicccccccccc") %>%
  clean_names() %>%
  mutate(survey_year = year(dmy_hms(created_date_local_adjusted)),
         survey_date = date(dmy_hms(created_date_local_adjusted)))

## View all project codes
# Echo_M04020 %>%
#   distinct(prj_cde) %>%
#   print(n=nrow(.))

# Summary table of survey years for the selected project code
Echo_M04020 %>%
  filter(prj_cde == project_code_for_data_analysis) %>%
  group_by(survey_year, created_user) %>%
  summarise(n = n())


## Bar plot - number of records per project 
# ggplot(Echo_M04020) + geom_bar(mapping = aes(x = fct_rev(prj_cde))) + coord_flip()

# Create a subset of the data file for the selected project
Echo_M04020_project <- Echo_M04020 %>%
  filter(prj_cde == project_code_for_data_analysis) %>%
  filter(survey_date >= date_filter_start) %>%
  filter(survey_date < date_filter_end)

Echo_M04020_project %>% glimpse()

# Remove Echo_M04020 from memory
rm(Echo_M04020)


#####
# Visualise raw data
#####

# Bar plot - number of records per target
ggplot(Echo_M04020_project) + geom_bar(mapping = aes(x = fct_rev(site_target_name))) + coord_flip()

# Percent cover values
Echo_M04020_project %>%
  ggplot(., aes(x = survey_date)) +
  # geom_point(aes(y = bare_ground_percent)) + # Bare ground
  # geom_point(aes(y = cryptogams_percent)) + # Cryptogams
  # geom_point(aes(y = bryphyte_lichn_mac_fungi_percent)) + # Bryophites, lichens and macro fungi
  geom_point(aes(y = litter_percent)) + # Litter
  theme_classic() +
  labs(x = "Time")

# Histograms
# hist(Echo_M04020_project$bare_ground_percent)
# hist(Echo_M04020_project$cryptogams_percent)
# hist(Echo_M04020_project$bryphyte_lichn_mac_fungi_percent)
# hist(Echo_M04020_project$litter_percent)

# Surface herbage values
Echo_M04020_project %>%
  ggplot(., aes(x = survey_date)) +
  # geom_point(aes(y = plant_base_percent, colour = site_id)) + # Plant cover at level of soil surface, including basal area
  geom_point(aes(y = annual_biennial_perc_folige_cvr)) + # Projective cover of annuals and biennials
  # geom_point(aes(y = annual_biennial_perc_folige_cvr * annual_biennial_weed_percent / 100)) + # Annual/biennial weediness
  # geom_point(aes(y = perennial_percent_foliage_cover)) + # Projective cover of perennials
  # geom_point(aes(y = perennial_weed_percent)) + # Percent of observed perennial cover which are weeds
  # geom_point(aes(y = perennial_percent_foliage_cover * perennial_weed_percent / 100)) + # Perennial weediness
  theme_classic() +
  labs(x = "Time")

# Histograms
# hist(Echo_M04020_project$plant_base_percent)
# hist(Echo_M04020_project$annual_biennial_perc_folige_cvr)
# hist(Echo_M04020_project$annual_biennial_weed_percent)
# hist(Echo_M04020_project$perennial_percent_foliage_cover)
# hist(Echo_M04020_project$perennial_weed_percent)


Echo_M04020_project %>%
  filter(site_target_name == "Native Grasslands") %>%
  group_by(site_id, survey_year) %>%
  summarise(n = n())


#####
# Preprocessing
#####

# Changes to the M04.020 method over time may mean that some values were not collected prior to a certain date.
# However, these may currently be reflected in Echo data as zero value observations rather than NA.
# Variables most likely to be affected by this issue are annual_biennial_perc_folige_cvr
# and perennial_percent_foliage_cover, which have dependent variables annual_biennial_weed_percent and
# perennial_weed_percent..

# Create table of all observations, excluding unneeded columns
soil_qdt_all <- Echo_M04020_project %>%
  select(-oid, -project_name, -staff_name, -observation_status, -created_date, -quadrat_function, -site_mga_zone, -site_prj_targt_id, -(length_m:survey123_edit_link))


### OPTIONAL BUT RECOMMENDED ###
# Find monitoring days for which all observed values of annual_biennial_perc_folige_cvr and perennial_percent_foliage_cover
# are zero, and update to NA.

# # Find zero-sum days
# zero_obs_dates <- soil_qdt_all %>%
#   group_by(survey_date) %>%
#   summarise(test = sum(across(c(annual_biennial_perc_folige_cvr, perennial_percent_foliage_cover))), n = n()) %>%
#   filter(test == 0)

# Find day-site combinations where all recorded values for plant cover and weediness are either zero or 100
soil_qdt_error_dates <- soil_qdt_all %>%
  group_by(survey_year,
           survey_date,
           site_id,
           annual_biennial_perc_folige_cvr,
           annual_biennial_weed_percent,
           perennial_percent_foliage_cover,
           perennial_weed_percent
           ) %>%
  summarise(n = n()) %>%
  filter(annual_biennial_perc_folige_cvr %in% c(0,100),
         annual_biennial_weed_percent %in% c(0,100),
         perennial_percent_foliage_cover %in% c(0,100),
         perennial_weed_percent  %in% c(0,100),
         n == 5)

# View records for matching days
soil_qdt_all %>%
  filter(survey_date %in% qdt_error_dates$survey_date, site_id %in% soil_qdt_error_dates$site_id) %>%
  View()

# Update matching observations (and dependent observed weediness proportions) to NA
# Update weediness observations to NA if observed cover is 0
soil_qdt_all <- soil_qdt_all %>%
  left_join(soil_qdt_error_dates) %>%
  rowwise() %>%
  mutate(annual_biennial_perc_folige_cvr = ifelse(is.na(n), annual_biennial_perc_folige_cvr, NA)) %>%
  mutate(perennial_percent_foliage_cover = ifelse(is.na(n), perennial_percent_foliage_cover, NA)) %>%
  mutate(annual_biennial_weed_percent = ifelse(is.na(n), ifelse(annual_biennial_perc_folige_cvr == 0, NA, annual_biennial_weed_percent), NA)) %>%
  mutate(perennial_weed_percent = ifelse(is.na(n), ifelse(perennial_percent_foliage_cover == 0, NA, perennial_weed_percent), NA)) %>%
  select(-n) %>%
  ungroup()



# ### OPTIONAL - at your discretion ###
# # Find monitoring days for which all observed values for a particular variable are zero ('zero-sum days'), 
# # and overwrite zero values with NA.
# 
# # plant_base_percent
# # Find dates
# zero_obs_dates <- soil_qdt_all %>%
#   group_by(survey_date) %>%
#   summarise(test = sum(plant_base_percent), n = n()) %>%
#   filter(test == 0)
# # Show dates and zero observation count
# zero_obs_dates
# # Replace zero values with NA
# soil_qdt_all <- soil_qdt_all %>%
#   mutate(plant_base_percent = if_else(survey_date %in% zero_obs_dates$survey_date, na_if(plant_base_percent, 0), plant_base_percent))
# 
# 
# # annual_biennial_perc_folige_cvr
# # Find dates
# zero_obs_dates <- soil_qdt_all %>%
#   group_by(survey_date) %>%
#   summarise(test = sum(annual_biennial_perc_folige_cvr), n = n()) %>%
#   filter(test == 0)
# # Show dates and zero observation count
# zero_obs_dates
# # Replace zero values with NA
# soil_qdt_all <- soil_qdt_all %>%
#   mutate(annual_biennial_perc_folige_cvr = if_else(survey_date %in% zero_obs_dates$survey_date, na_if(annual_biennial_perc_folige_cvr, 0), annual_biennial_perc_folige_cvr))
# 
# 
# # perennial_percent_foliage_cover
# # Find dates
# zero_obs_dates <- soil_qdt_all %>%
#   group_by(survey_date) %>%
#   summarise(test = sum(perennial_percent_foliage_cover), n = n()) %>%
#   filter(test == 0)
# # Show dates and zero observation count
# zero_obs_dates
# # Replace zero values with NA
# soil_qdt_all <- soil_qdt_all %>%
#   mutate(perennial_percent_foliage_cover = if_else(survey_date %in% zero_obs_dates$survey_date, na_if(perennial_percent_foliage_cover, 0), perennial_percent_foliage_cover))


# Create native/non-native base table
soil_qdt_native <- soil_qdt_all %>%
  filter(!is.na(annual_biennial_perc_folige_cvr) | !is.na(perennial_percent_foliage_cover)) %>%
  select(survey_year, survey_date, prj_cde:quadrat_id, annual_biennial_perc_folige_cvr:perennial_weed_percent) %>%
  mutate(cover_weed_annual = replace_na(annual_biennial_perc_folige_cvr * (annual_biennial_weed_percent / 100), 0)) %>%
  mutate(cover_weed_peren = replace_na(perennial_percent_foliage_cover * (perennial_weed_percent / 100), 0)) %>%
  mutate(cover_weed = cover_weed_annual + cover_weed_peren) %>%
  mutate(cover_native_annual = annual_biennial_perc_folige_cvr - cover_weed_annual) %>%
  mutate(cover_native_peren = perennial_percent_foliage_cover - cover_weed_peren) %>%
  mutate(cover_native = cover_native_annual + cover_native_peren) %>%
  mutate(surface_herbage = cover_native + cover_weed) %>%
  mutate(prop_weed_annual = cover_weed_annual / na_if(surface_herbage, 0) * 100) %>%
  mutate(prop_weed_peren = cover_weed_peren / na_if(surface_herbage, 0) * 100) %>%
  mutate(prop_weed = cover_weed / na_if(surface_herbage, 0) * 100) %>%
  mutate(prop_native_annual = cover_native_annual / na_if(surface_herbage, 0) * 100) %>%
  mutate(prop_native_peren = cover_native_peren / na_if(surface_herbage, 0) * 100) %>%
  mutate(prop_native = cover_native / na_if(surface_herbage, 0) * 100)
  
# Create cover abundance and proportion pivots
soil_qdt_native_cover <- soil_qdt_native %>%
  select(survey_year, survey_date, prj_cde:quadrat_id, cover_weed_annual:cover_native) %>%
  pivot_longer(cover_weed_annual:cover_native, names_to = "cover_type", names_prefix = "cover_", values_to = "percentage") %>%
  mutate(cover_type = str_replace(cover_type, "native", "Native")) %>%
  mutate(cover_type = str_replace(cover_type, "weed", "Introduced")) %>%
  mutate(cover_type = str_replace(cover_type, "_annual", " Annual/Biennial")) %>%
  mutate(cover_type = str_replace(cover_type, "_peren", " Perennial"))

soil_qdt_native_prop <- soil_qdt_native %>%
  select(survey_year, survey_date, prj_cde:quadrat_id, prop_weed_annual:prop_native) %>%
  pivot_longer(prop_weed_annual:prop_native, names_to = "prop_type", names_prefix = "prop_", values_to = "percentage") %>%
  mutate(prop_type = str_replace(prop_type, "native", "Native")) %>%
  mutate(prop_type = str_replace(prop_type, "weed", "Introduced")) %>%
  mutate(prop_type = str_replace(prop_type, "_annual", " Annual/Biennial")) %>%
  mutate(prop_type = str_replace(prop_type, "_peren", " Perennial"))



#####
# Plots for reporting
#####

# Bare ground

soil_qdt_all %>%
  group_by(survey_year, site_target_name) %>%
  ggplot(., aes(x = as.factor(survey_year), y = bare_ground_percent)) +
  facet_wrap(~site_target_name) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs (x = "Year", y = "Percent bare ground") +
  ggtitle("Percent bare ground")
# ggsave("Outputs/Soil Surface bare ground.png", dpi = 600, width = 8, height = 6)
  
soil_qdt_all %>%
  group_by(survey_year, site_target_name) %>%
  summarise(percent_bare_mean = mean(bare_ground_percent),
            percent_bare_sd = sd(bare_ground_percent)) %>%
  ggplot(., aes(x = survey_year, y = percent_bare_mean)) +
  facet_grid(site_target_name ~.) +
  geom_errorbar(aes(ymin = percent_bare_mean - percent_bare_sd, ymax = percent_bare_mean + percent_bare_sd, width = 0.2)) +
  geom_point() +
  geom_line() +
  ggtitle("Mean percent bare with standard deviation bars")

# Dotplot with trendline
soil_qdt_all %>%
  group_by(survey_date, site_target_name) %>%
  ggplot(., aes(x = survey_date, y = bare_ground_percent)) +
  facet_wrap(~site_target_name) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs (x = "Year", y = "Percent bare ground") +
  ggtitle("Percent bare ground")



# Cryptogam

soil_qdt_all %>%
  group_by(survey_year, site_target_name) %>%
  ggplot(., aes(x = as.factor(survey_year), y = cryptogams_percent)) +
  facet_wrap(~site_target_name) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs (x = "Year", y = "Percent cryptogam cover") +
  ggtitle("Cryptogam cover")
# ggsave("Outputs/Soil Surface Cryptogam cover.png", dpi = 600, width = 8, height = 6)

soil_qdt_all %>%
  group_by(survey_year, site_target_name) %>%
  summarise(percent_cryptogam_mean = mean(cryptogams_percent),
            percent_cryptogam_sd = sd(cryptogams_percent)) %>%
  ggplot(., aes(x = survey_year, y = percent_cryptogam_mean)) +
  facet_grid(site_target_name ~.) +
  geom_errorbar(aes(ymin = percent_cryptogam_mean - percent_cryptogam_sd, ymax = percent_cryptogam_mean + percent_cryptogam_sd, width = 0.2)) +
  geom_point() +
  geom_line() +
  ggtitle("Mean percent cryptogam cover with standard deviation bars")



# Bryophytes, lichens and macroscopic fungi

soil_qdt_all %>%
  group_by(survey_year, site_target_name) %>%
  ggplot(., aes(x = as.factor(survey_year), y = bryphyte_lichn_mac_fungi_percent)) +
  facet_wrap(~site_target_name) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs (x = "Year", y = "Percent cover of bryophytes,\nlichens and macroscopic fungi") +
  ggtitle("Bryophytes, lichens and fungi")
# ggsave("Outputs/Soil Surface Cryptogam cover.png", dpi = 600, width = 8, height = 6)

soil_qdt_all %>%
  group_by(survey_year, site_target_name) %>%
  summarise(percent_bryo_mean = mean(bryphyte_lichn_mac_fungi_percent),
            percent_bryo_sd = sd(bryphyte_lichn_mac_fungi_percent)) %>%
  ggplot(., aes(x = survey_year, y = percent_bryo_mean)) +
  facet_grid(site_target_name ~.) +
  geom_errorbar(aes(ymin = percent_bryo_mean - percent_bryo_sd, ymax = percent_bryo_mean + percent_bryo_sd, width = 0.2)) +
  geom_point() +
  geom_line() +
  ggtitle("Mean percent cover of bryophytes, lichens and macroscopic fungi with standard deviation bars")


 
# Litter

soil_qdt_all %>%
  group_by(survey_year, site_target_name) %>%
  ggplot(., aes(x = as.factor(survey_year), y = litter_percent)) +
  facet_wrap(~site_target_name) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs (x = "Year", y = "Percent litter cover") +  
  ggtitle("Litter cover")
# ggsave("Outputs/Soil surface Litter cover.png", dpi = 600, width = 8, height = 6)

soil_qdt_all %>%
  group_by(survey_year, site_target_name) %>%
  summarise(percent_litter_mean = mean(litter_percent),
            percent_litter_sd = sd(litter_percent)) %>%
  ggplot(., aes(x = survey_year, y = percent_litter_mean)) +
  facet_grid(site_target_name ~.) +
  geom_errorbar(aes(ymin = percent_litter_mean - percent_litter_sd, ymax = percent_litter_mean + percent_litter_sd, width = 0.2)) +
  geom_point() +
  geom_line() +
  ggtitle("Mean percent litter with standard deviation bars")





# Plant cover at level of soil surface, including basal area

soil_qdt_all %>%
  group_by(survey_year, site_target_name) %>%
  ggplot(., aes(x = as.factor(survey_year), y = plant_base_percent)) +
  facet_wrap(~site_target_name) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs (x = "Year", y = "Plant cover percent") +  
  ggtitle("Plant cover percent at level of soil surface, including basal area")
# ggsave("Outputs/Soil surface plant cover.png", dpi = 600, width = 8, height = 6)

soil_qdt_all %>%
  group_by(survey_year, site_target_name) %>%
  summarise(plant_base_mean = mean(plant_base_percent),
            plant_base_sd = sd(plant_base_percent)) %>%
  ggplot(., aes(x = survey_year, y = plant_base_mean)) +
  facet_grid(site_target_name ~.) +
  geom_errorbar(aes(ymin = plant_base_mean - plant_base_sd, ymax = plant_base_mean + plant_base_sd, width = 0.2)) +
  geom_point() +
  geom_line() +
  ggtitle("Mean percent plant cover at soil surface with standard deviation bars")



# Projected foliage cover of annual/biennial and perennial spp

surface_herbage_table <- soil_qdt_all %>%
  select(survey_year, site_target_name, "Annual and Biennial" = annual_biennial_perc_folige_cvr, "Perennial" = perennial_percent_foliage_cover) %>% 
  pivot_longer(c(-survey_year, -site_target_name), names_to = "veg_type", values_to = "percentage") %>% 
  group_by(survey_year, site_target_name) 

surface_herbage_table %>% glimpse()

surface_herbage_table %>%
  ggplot(., aes(x = as.factor(survey_year), y = percentage, fill = veg_type)) +
  facet_wrap(~site_target_name) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs (x = "Year", y = "Percent herbage cover", fill = "Vegetation type") +  
  ggtitle("Annual and Perennial herbage")
# ggsave("Outputs/Soil surface Annual and Perennial herbage.png", dpi = 600, width = 8, height = 6)

# Create line plot with standard deviation error bars
surface_herbage_table %>%
  group_by(survey_year, site_target_name, veg_type) %>%
  summarise(percent_cover_mean = mean(percentage),
            percent_cover_sd = sd(percentage)) %>%
  ggplot(., aes(x = survey_year, y = percent_cover_mean, fill = veg_type)) +
  facet_grid(site_target_name ~.) +
  geom_errorbar(aes(ymin = percent_cover_mean - percent_cover_sd, ymax = percent_cover_mean + percent_cover_sd, width = 0.2)) +
  geom_line() +
  geom_point(shape=21, size = 2) +
  ggtitle("Mean percent surface herbage with standard deviation bars")

# Summary table for Miradi
surface_herbage_table %>% 
  group_by(site_target_name, survey_year, veg_type) %>% 
  summarise(mean_percentage = mean(percentage)) %>% 
  pivot_wider(id_cols = c(site_target_name, veg_type), names_from = survey_year, values_from = mean_percentage) %>% 
  write_csv("Outputs/surface herbage table.csv", na = "")

# Data table for Miradi
soil_qdt_all %>% 
  select(survey_year, survey_date, site_target_name, site_id, cryptogams_percent, bare_ground_percent) %>% 
  group_by(site_target_name, survey_year, site_id) %>% 
  summarise(mean_percent_cryptogam = mean(cryptogams_percent), mean_percent_bare = mean(bare_ground_percent)) %>% 
  select(-site_id) %>% 
  summarise(mean_percent_cryptogam = mean(mean_percent_cryptogam), mean_percent_bare = mean(mean_percent_bare)) %>% 
  pivot_wider(id_cols = c(site_target_name), names_from = survey_year, values_from = c(mean_percent_cryptogam, mean_percent_bare)) %>% 
  write_csv("Outputs/soil surface summary table.csv", na = "")



### Projective foliage cover

## By target

# Boxplot
soil_qdt_native %>%
  filter(!is.na(surface_herbage)) %>%
  ggplot(aes(x = as.factor(survey_year), y = surface_herbage)) +
  facet_wrap(~site_target_name) +
  geom_boxplot() +
  theme_classic() +
  labs(x = "Year", y = "Percent cover") +
  ggtitle("Surface herbage", subtitle = project_name)

# Dotplot with error bars
soil_qdt_native %>%
  filter(!is.na(surface_herbage)) %>%
  group_by(site_target_name, survey_year) %>%
  summarise(native_mean = mean(surface_herbage), native_sd = sd(surface_herbage)) %>%
  ggplot(aes(x = as.factor(survey_year))) +
  facet_wrap(~site_target_name) +
  geom_point(aes(y = native_mean)) +
  # geom_smooth(method = "glm") +
  geom_errorbar(aes(ymin = native_mean - native_sd, ymax = native_mean + native_sd, width = 0.2)) +
  theme_classic() +
  labs(x = "Year", y = "Percent cover") +
  ggtitle("Surface herbage", subtitle = project_name)

# Dotplot with glm trendline
soil_qdt_native %>%
  ggplot(aes(x = survey_date, y = surface_herbage)) +
  facet_wrap(~site_target_name) +
  geom_point() +
  geom_smooth(method = "glm") +
  theme_classic() +
  labs(x = "Date", y = "Percent cover") +
  ggtitle("Surface herbage", subtitle = project_name)



### Native species cover

## By target - total cover

# Boxplot
soil_qdt_native %>%
  ggplot(aes(x = as.factor(survey_year), y = cover_native)) +
  facet_wrap(~site_target_name) +
  geom_boxplot() +
  theme_classic() +
  labs(x = "Year", y = "Percent cover") +
  ggtitle("Native species cover", subtitle = project_name)

# Dotplot with error bars
soil_qdt_native %>%
  filter(!is.na(cover_native)) %>%
  group_by(site_target_name, survey_year) %>%
  summarise(native_mean = mean(cover_native), native_sd = sd(cover_native)) %>%
  ggplot(aes(x = as.factor(survey_year))) +
  facet_wrap(~site_target_name) +
  geom_point(aes(y = native_mean)) +
  # geom_smooth(method = "glm") +
  geom_errorbar(aes(ymin = native_mean - native_sd, ymax = native_mean + native_sd, width = 0.2)) +
  theme_classic() +
  labs(x = "Year", y = "Percent cover") +
  ggtitle("Native species cover", subtitle = project_name)

# Dotplot with glm trendline
soil_qdt_native %>%
  ggplot(aes(x = survey_date, y = cover_native)) +
  facet_wrap(~site_target_name) +
  geom_point() +
  geom_smooth(method = "glm") +
  theme_classic() +
  labs(x = "Date", y = "Percent cover") +
  ggtitle("Native species cover", subtitle = project_name)



### Target of interest

focal_target <- "Grassy Box-Gum Woodlands"
focal_target <- "Native Grasslands"
focal_target <- "Shrubby Eucalypt Woodland"

# Dotplot with glm trendline
soil_qdt_native %>%
  filter(site_target_name %in% focal_target) %>%
  ggplot(aes(x = survey_date, y = cover_native, xmin = date_filter_start, xmax = date_filter_end)) +
  geom_point() +
  geom_smooth(method = "glm") +
  theme_classic() +
  labs(x = "Date", y = "Percent cover") +
  ggtitle("Native species cover", subtitle = paste0(project_name, " - ", focal_target))

# Dotplot with glm trendline, split by site
soil_qdt_native %>%
  filter(site_target_name %in% focal_target) %>%
  ggplot(aes(x = survey_date, y = cover_native, group = site_id, xmin = date_filter_start, xmax = date_filter_end)) +
  facet_wrap(~site_id) +
  geom_point() +
  geom_smooth(method = "glm") +
  theme_classic() +
  labs(x = "Date", y = "Percent cover") +
  ggtitle("Native species cover", subtitle = paste0(project_name, " - ", focal_target))



### Native species proportion of observed cover - aggregated

## By target

# Boxplot
soil_qdt_native_prop %>%
  filter(prop_type == "Native") %>%
  ggplot(aes(x = as.factor(survey_year), y = percentage)) +
  facet_wrap(~site_target_name) +
  geom_boxplot() +
  theme_classic() +
  labs(x = "Year", y = "Percent cover") +
  ggtitle("Native species cover proportion", subtitle = project_name)

# Dotplot with error bars
soil_qdt_native_prop %>%
  filter(prop_type == "Native") %>%
  group_by(site_target_name, survey_year) %>%
  summarise(native_mean = mean(percentage), native_sd = sd(percentage)) %>%
  ggplot(aes(x = as.factor(survey_year))) +
  facet_wrap(~site_target_name) +
  geom_point(aes(y = native_mean)) +
  # geom_smooth(method = "glm") +
  geom_errorbar(aes(ymin = native_mean - native_sd, ymax = native_mean + native_sd, width = 0.2)) +
  theme_classic() +
  labs(x = "Year", y = "Percent cover") +
  ggtitle("Native species cover proportion", subtitle = project_name)

# Dotplot with glm trendline
soil_qdt_native_prop %>%
  filter(prop_type == "Native") %>%
  ggplot(aes(x = survey_date, y = percentage)) +
  facet_wrap(~site_target_name) +
  geom_point() +
  geom_smooth(method = "glm") +
  theme_classic() +
  labs(x = "Date", y = "Percent cover") +
  ggtitle("Native species cover proportion", subtitle = project_name)


### Target of interest

focal_target <- "Shrubby Eucalypt Woodland"

# Dotplot with glm trendline
soil_qdt_native_prop %>%
  filter(site_target_name %in% focal_target) %>%
  filter(prop_type == "Native") %>%
  ggplot(aes(x = survey_date, y = percentage, xmin = date_filter_start, xmax = date_filter_end)) +
  geom_point() +
  geom_smooth(method = "glm") +
  theme_classic() +
  labs(x = "Date", y = "Proportion of observed cover", colour = "Vegetation type") +
  ggtitle("Native species cover proportion", subtitle = paste0(project_name, " - ", focal_target))

# Dotplot with glm trendline, split by site
soil_qdt_native_prop %>%
  filter(site_target_name %in% focal_target) %>%
  filter(prop_type == "Native") %>%
  ggplot(aes(x = survey_date, y = percentage, group = site_id, xmin = date_filter_start, xmax = date_filter_end)) +
  facet_wrap(~site_id) +
  geom_point() +
  geom_smooth(method = "glm") +
  theme_classic() +
  labs(x = "Date", y = "Proportion of observed cover", colour = "Vegetation type") +
  ggtitle("Native species cover proportion", subtitle = paste0(project_name, " - ", focal_target))



### Native species proportion of observed cover - split

## By target

# Boxplot
soil_qdt_native_prop %>%
  filter(prop_type %in% c("Native Annual/Biennial", "Native Perennial")) %>%
  ggplot(aes(x = as.factor(survey_year), y = percentage, colour = prop_type)) +
  facet_wrap(~site_target_name) +
  geom_boxplot() +
  theme_classic() +
  labs(x = "Year", y = "Percent cover") +
  ggtitle("Native species cover proportion by vegetation type", subtitle = project_name)

# Dotplot with error bars
soil_qdt_native_prop %>%
  filter(prop_type %in% c("Native Annual/Biennial", "Native Perennial")) %>%
  group_by(site_target_name, survey_year, prop_type) %>%
  summarise(native_mean = mean(percentage), native_sd = sd(percentage)) %>%
  ggplot(aes(x = as.factor(survey_year), colour = prop_type)) +
  facet_wrap(~site_target_name) +
  geom_point(aes(y = native_mean)) +
  # geom_smooth(method = "glm") +
  geom_errorbar(aes(ymin = native_mean - native_sd, ymax = native_mean + native_sd, width = 0.2)) +
  theme_classic() +
  labs(x = "Year", y = "Percent cover") +
  ggtitle("Native species cover proportion by vegetation type", subtitle = project_name)

# Dotplot with glm trendline
soil_qdt_native_prop %>%
  filter(prop_type %in% c("Native Annual/Biennial", "Native Perennial")) %>%
  ggplot(aes(x = survey_date, y = percentage, colour = prop_type, xmin = date_filter_start, xmax = date_filter_end)) +
  facet_wrap(~site_target_name) +
  geom_point() +
  geom_smooth(method = "glm") +
  theme_classic() +
  labs(x = "Date", y = "Percent cover") +
  ggtitle("Native species cover proportion by vegetation type", subtitle = project_name)


### Target of interest

focal_target <- "Grassy Box-Gum Woodlands"

# Dotplot with glm trendline
soil_qdt_native_prop %>%
  filter(site_target_name %in% focal_target) %>%
  filter(prop_type %in% c("Native Annual/Biennial", "Native Perennial")) %>%
  ggplot(aes(x = survey_date, y = percentage, colour = prop_type, xmin = date_filter_start, xmax = date_filter_end)) +
  geom_point() +
  geom_smooth(method = "glm") +
  theme_classic() +
  labs(x = "Date", y = "Percent cover", colour = "Vegetation type") +
  ggtitle("Native species cover proportion by vegetation type", subtitle = paste0(project_name, " - ", focal_target))

# Dotplot with glm trendline, split by site
soil_qdt_native_prop %>%
  filter(site_target_name %in% focal_target) %>%
  filter(prop_type %in% c("Native Annual/Biennial", "Native Perennial")) %>%
  ggplot(aes(x = survey_date, y = percentage, colour = prop_type, group = site_id, xmin = date_filter_start, xmax = date_filter_end)) +
  facet_wrap(~site_id) +
  geom_point() +
  geom_smooth(method = "glm") +
  theme_classic() +
  labs(x = "Date", y = "Percent cover", colour = "Vegetation type") +
  ggtitle("Native species cover proportion by vegetation type", subtitle = paste0(project_name, " - ", focal_target))



#####
# Weediness
#####

# Create a table of weed cover values
surface_herbage_weed_table <- soil_qdt_all %>%
  mutate(annual_biennial_weed_cover = (annual_biennial_perc_folige_cvr * annual_biennial_weed_percent / 100), perennial_weed_cover = (perennial_percent_foliage_cover * perennial_weed_percent / 100)) %>%
  select(survey_year, site_target_name, "Annual and Biennial" = annual_biennial_weed_cover, "Perennial" = perennial_weed_cover) %>%
  pivot_longer(c(-survey_year, -site_target_name), names_to = "veg_type", values_to = "percentage") %>%
  group_by(survey_year, site_target_name)

surface_herbage_weed_table %>% glimpse()

# Create weediness boxplot
surface_herbage_weed_table %>%
  ggplot(., aes(x = as.factor(survey_year), y = percentage, fill = veg_type)) +
  facet_wrap(~site_target_name) +
  geom_boxplot() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs (x = "Year", y = "Percent weed cover", fill = "Vegetation type") +  
  ggtitle("Annual and Perennial weediness")

# Create weediness line plot with standard deviation error bars
surface_herbage_weed_table %>%
  group_by(survey_year, site_target_name, veg_type) %>%
  summarise(percent_weediness_mean = mean(percentage),
            percent_weediness_sd = sd(percentage)) %>%
  ggplot(., aes(x = survey_year, y = percent_weediness_mean, fill = veg_type)) +
  facet_grid(site_target_name ~.) +
  geom_errorbar(aes(ymin = percent_weediness_mean - percent_weediness_sd, ymax = percent_weediness_mean + percent_weediness_sd, width = 0.2)) +
  geom_line() +
  geom_point(shape=21, size = 2) +
  ggtitle("Mean percent weediness with standard deviation bars")




