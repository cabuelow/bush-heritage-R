#####
# Title: Scottsdale vegetation analysis for evaluation report 1/1/2006-31/12/2019
# Author: Gareth Davies
#####

# Required: interim outputs for vegetation quadrat and transect, tree health, and soil surface

### Libraries
install.packages("ggpmisc")
# install.packages("gginnards")

library(tidyverse)
library(ggpmisc)
library(readxl)
# library(gginnards)


# Indicator: Native ground cover - proportion
# Sideline: Native ground cover (non-proportional)
# Targets: Grassy Box-Gum Woodlands, Native Grasslands, Shrubby Eucalypt Woodland

# Iterate through the following values of focal_target:
focal_targets = list("Grassy Box-Gum Woodlands", "Native Grasslands", "Shrubby Eucalypt Woodland")

for (x in focal_targets) {
  
  ### Native groundcover proportion
  
  # Aggregated native groundcover
  dotplot <- soil_qdt_native_prop %>%
    filter(site_target_name == x) %>%
    filter(prop_type == "Native") %>%
    ggplot(aes(x = survey_date, y = percentage, xmin = date_filter_start, xmax = date_filter_end)) +
    geom_smooth(method = "lm") +
    # stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(',')~~"))) + # Add comma-separated eq and R^2 label
    stat_poly_eq(aes(label = paste(after_stat(rr.label), # Add comma-separated R^2, P and F values
                                   after_stat(p.value.label),
                                   after_stat(f.value.label),
                                   sep = "*', '*"))) +
    geom_point() +
    theme_classic() +
    labs(x = "Date", y = "Proportion of observed cover (%)", colour = "Vegetation type") +
    ggtitle("Native species cover proportion", subtitle = paste0(project_name, " - ", x))
  
  # test_stats <- dotplot +
  #   stat_poly_eq(aes(label = paste(after_stat(rr.label), # Add comma-separated R^2, P and F values
  #                                  after_stat(p.value.label),
  #                                  after_stat(f.value.label),
  #                                  sep = "*', '*"))) +
  #   stat_poly_eq(output.type = "numeric", geom = "debug")
  # 
  # test_stats

  print(dotplot)
  
  # if(after_stat(p.value.label) < 0.05) {
  #   layer_data()
  #   write_excel_csv(dotplot,1)
  # }
  
  # Split by site - no summary stats
  print(soil_qdt_native_prop %>%
    filter(site_target_name == x) %>%
    filter(prop_type == "Native") %>%
    ggplot(aes(x = survey_date, y = percentage, group = site_id, xmin = date_filter_start, xmax = date_filter_end)) +
    facet_wrap(~site_id) +
    geom_point() +
    geom_smooth(method = "lm") +
    theme_classic() +
    labs(x = "Date", y = "Proportion of observed cover", colour = "Vegetation type") +
    ggtitle("Native species cover proportion", subtitle = paste0(project_name, " - ", x))
  )
  
  # Annual/biennial and perennial
  print(soil_qdt_native_prop %>%
    filter(site_target_name == x) %>%
    filter(prop_type %in% c("Native Annual/Biennial", "Native Perennial")) %>%
    ggplot(aes(x = survey_date, y = percentage, colour = as_factor(prop_type), xmin = date_filter_start, xmax = date_filter_end)) +
    geom_smooth(method = "lm") +
    # stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(',')~~"))) + # Add comma-separated eq and R^2 label
    stat_poly_eq(aes(label = paste(after_stat(rr.label), # Add comma-separated R^2, P and F values
                                   after_stat(p.value.label),
                                   after_stat(f.value.label),
                                   sep = "*', '*"))) +
    geom_point() +
    theme_classic() +
    labs(x = "Date", y = "Proportion of observed cover (%)", colour = "Vegetation type") +
    ggtitle("Native species cover proportion by vegetation type", subtitle = paste0(project_name, " - ", x))
  )
  
  # Split by site - annual/biennial and perennial, no summary stats
  print(soil_qdt_native_prop %>%
    filter(site_target_name == x) %>%
    filter(prop_type %in% c("Native Annual/Biennial", "Native Perennial")) %>%
    ggplot(aes(x = survey_date, y = percentage, colour = as_factor(prop_type), group = site_id, xmin = date_filter_start, xmax = date_filter_end)) +
    facet_wrap(~site_id) +
    geom_point() +
    geom_smooth(method = "lm") +
    theme_classic() +
    labs(x = "Date", y = "Percent cover", colour = "Vegetation type") +
    ggtitle("Native species cover proportion by vegetation type", subtitle = paste0(project_name, " - ", x))
  )
  
  # Additional plots
  # Observed native cover (non-proportional)
  print(soil_qdt_native %>%
    filter(site_target_name == x) %>%
    ggplot(aes(x = survey_date, y = cover_native, xmin = date_filter_start, xmax = date_filter_end)) +
    geom_point() +
    geom_smooth(method = "lm") +
    stat_poly_eq(aes(label = paste(after_stat(rr.label), # Add comma-separated R^2, P and F values
                                   after_stat(p.value.label),
                                   after_stat(f.value.label),
                                   sep = "*', '*"))) +
    theme_classic() +
    labs(x = "Date", y = "Percent cover") +
    ggtitle("Native species cover", subtitle = paste0(project_name, " - ", x))
  )
  
  # Split by site, no summary stats
  print(soil_qdt_native %>%
    filter(site_target_name == x) %>%
    ggplot(aes(x = survey_date, y = cover_native, group = site_id, xmin = date_filter_start, xmax = date_filter_end)) +
    facet_wrap(~site_id) +
    geom_point() +
    geom_smooth(method = "glm") +
    theme_classic() +
    labs(x = "Date", y = "Percent cover") +
    ggtitle("Native species cover", subtitle = paste0(project_name, " - ", x))
  )
  
  ## Bare ground
  # Dotplot with trendline
  print(soil_qdt_all %>%
    filter(site_target_name == x) %>%
    group_by(survey_date, site_target_name) %>%
    ggplot(., aes(x = survey_date, y = bare_ground_percent)) +
    geom_point() +
    geom_smooth(method = "lm") +
    stat_poly_eq(aes(label = paste(after_stat(rr.label), # Add comma-separated R^2, P and F values
                                   after_stat(p.value.label),
                                   after_stat(f.value.label),
                                   sep = "*', '*"))) +
    theme_classic() +
    labs (x = "Year", y = "Percent cover") +
    ggtitle("Bare ground", subtitle = paste0(project_name, " - ", x))
  )
  
  ## Litter cover
  print(soil_qdt_all %>%
    filter(site_target_name == x) %>%
    group_by(survey_date, site_target_name) %>%
    ggplot(., aes(x = survey_date, y = litter_percent)) +
    geom_point() +
    geom_smooth(method = "lm") +
    stat_poly_eq(aes(label = paste(after_stat(rr.label), # Add comma-separated R^2, P and F values
                                   after_stat(p.value.label),
                                   after_stat(f.value.label),
                                   sep = "*', '*"))) +
    theme_classic() +
    labs (x = "Year", y = "Percent cover") +
    ggtitle("Litter cover", subtitle = paste0(project_name, " - ", x))
  )
  
}

# Sideline: veg transect method observed native cover per stratum
for (x in focal_targets) {
  print(veg_tsc_native %>%
    filter(site_target_name == x) %>%
    ggplot(aes(x = survey_date, y = cover, colour = stratum, xmin = date_filter_start, xmax = date_filter_end)) +
    geom_point() +
    geom_smooth(method = lm) +
    theme_classic() +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "Survey date", y = "Cover") +
    ggtitle(label = "Observed cover of native species", subtitle = paste0(project_name, " - ", x))
  )
  
  print(veg_tsc_native %>%
    filter(site_target_name == x) %>%
    ggplot(aes(x = survey_date, y = cover, xmin = date_filter_start, xmax = date_filter_end)) +
    facet_wrap(~stratum) +
    geom_point() +
    geom_smooth(method = lm) +
    theme_classic() +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "Survey date", y = "Cover") +
    ggtitle(label = "Observed cover of native species", subtitle = paste0(project_name, " - ", x))
  )
}

# Sideline: veg quadrat method observed relative frequency of native species
for (x in focal_targets) {
  print(veg_qdt_native %>%
    filter(native == "Native") %>%
    filter(site_target_name == x) %>%
    ggplot(aes(x = survey_date, y = rel_freq, xmin = date_filter_start, xmax = date_filter_end)) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    geom_smooth(method = "lm") +
    stat_poly_eq(aes(label = paste(after_stat(rr.label), # Add comma-separated R^2, P and F values
                                   after_stat(p.value.label),
                                   after_stat(f.value.label),
                                   sep = "*', '*"))) +
    geom_point() +
    theme_classic() +
    labs (x = "Date", y = "Relative Frequency") +
    ggtitle("Relative frequency of native species", subtitle = paste0(project_name, " - ", x))
  )
}

# Indicator: Tree health
# Target: Grassy Box-Gum Woodlands, Shrubby Eucalypt Woodland

# Iterate through the following values of focal_target:
focal_targets = list("Grassy Box-Gum Woodlands", "Shrubby Eucalypt Woodland")

for (x in focal_targets) {
  # Dotplot with trendline
  print(tree_all %>%
    filter(site_target_name == x) %>%
    ggplot(aes(x = survey_date, y = score, xmin = date_filter_start, xmax = date_filter_end)) +
    scale_y_reverse() +
    geom_smooth(method = "lm") +
    stat_poly_eq(aes(label = paste(after_stat(rr.label),
                                   after_stat(p.value.label),
                                   after_stat(f.value.label),
                                   sep = "*', '*"))) +
    # geom_jitter(width = 21, height = 0, alpha = 1/5) +
    geom_point(alpha = 1/10) +
    theme_classic() +
    labs(x = "Date", y = "Tree health score") +
    labs(title = "Tree health", subtitle = paste0(project_name, " - ", x), caption = "Score range: 1 (best) to 5 (worst).")
  )
}


# Indicator: Woody species recruitment

# Copy to clipboard for woody spp classification in Excel

recr_target_species %>%
  write.table(., "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)

recr_target_species_scot <- read_excel("Outputs/Scottsdale recruitment 2006-2019.xlsx") %>%
  clean_names() %>%
  filter(woody_stemmed == "y" & native == "y")

recr_target_species_scot %>%
  group_by(survey_year, site_target_name) %>%
  summarise(total_recruits = sum(recruits_sp_total)) %>%
  ggplot(aes(x = survey_year, y = total_recruits, xmin = 2006, xmax = 2020, colour = site_target_name, group = site_target_name)) +
  geom_point() +
  geom_path() +
  theme_classic() +
  labs(x = "Year", y = "Total recruits", colour = "Target") +
  ggtitle("Observed native woody species recruitment", subtitle = paste0(project_name))
  
