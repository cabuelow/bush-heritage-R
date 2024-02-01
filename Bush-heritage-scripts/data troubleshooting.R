library(tidyverse)


# What percentage of M06.080 observations are for species 'Unrecorded' and NA?
left_join(
  Echo_M06080 %>%
    group_by(prj_cde) %>%
    summarise(count = n()),
  Echo_M06080 %>%
    filter(scientific_name == "Unrecorded") %>%
    group_by(prj_cde) %>%
    summarise(unrec_count = n()),
  by = "prj_cde"
  ) %>%
left_join(
  .,
  Echo_M06080 %>%
    filter(is.na(scientific_name)) %>%
    group_by(prj_cde) %>%
    summarise(na_count = n())
  ) %>%
  mutate(unrec_count = replace_na(unrec_count, 0),
         na_count = replace_na(na_count, 0),
         unrec_perc = unrec_count / count * 100,
         na_perc = na_count / count * 100, 
         invalid_perc = (unrec_count + na_count) / count * 100
         ) %>%
  # View()
  write.table(., "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)

# Plot frequency of 'NA' and 'Unrecorded' species observations per year
plot_diag_a <- Echo_M06080 %>%
  filter(scientific_name == "Unrecorded" | is.na(scientific_name)) %>%
  group_by(survey_year) %>%
  summarise(count_invalid_sci_name = n()) %>%
  ggplot(., aes(x = survey_year, y = count_invalid_sci_name)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks=seq(2005,2022,1)) +
  labs(x = "Survey year", y = "Invalid scientific name count") +
  theme_bw() + theme(panel.grid.minor.x = element_blank()) +
  ggtitle("M06.080")

# Plot percentage of 'NA' and 'Unrecorded' species observations per year
plot_diag_b <- left_join(
  Echo_M06080 %>%
    group_by(survey_year) %>%
    summarise(n = n()),
  Echo_M06080 %>%
    filter(scientific_name == "Unrecorded" | is.na(scientific_name)) %>%
    group_by(survey_year) %>%
    summarise(n_invalid_sci_name = n())
) %>%
  ggplot(., aes(x = survey_year, y = (n_invalid_sci_name / n * 100))) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks=seq(2005,2022,1)) +
  labs(x = "Survey year", y = "Invalid scientific name %") +
  theme_bw() + theme(panel.grid.minor.x = element_blank())

cowplot::plot_grid(plot_diag_a, plot_diag_b, align = "v", ncol = 1)


# What percentage of M06.090 observations are for species 'Unrecorded' and NA?
left_join(
  Echo_M06090 %>%
    group_by(prj_cde) %>%
    summarise(count = n()),
  Echo_M06090 %>%
    filter(scientific_name == "Unrecorded") %>%
    group_by(prj_cde) %>%
    summarise(unrec_count = n())
) %>%
  left_join(
    .,
    Echo_M06090 %>%
      filter(is.na(scientific_name)) %>%
      group_by(prj_cde) %>%
      summarise(na_count = n())
  ) %>%
  mutate(unrec_count = replace_na(unrec_count, 0),
         na_count = replace_na(na_count, 0),
         unrec_perc = unrec_count / count * 100,
         na_perc = na_count / count * 100, 
         invalid_perc = (unrec_count + na_count) / count * 100
  ) %>%
  #View()
  write.table(., "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)

# Plot frequency of 'NA' and 'Unrecorded' species observations per year
plot_diag_a <- Echo_M06090 %>%
  filter(scientific_name == "Unrecorded" | is.na(scientific_name)) %>%
  group_by(survey_year) %>%
  summarise(count_invalid_sci_name = n()) %>%
  ggplot(., aes(x = survey_year, y = count_invalid_sci_name)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks=seq(2005,2022,1)) +
  labs(x = "Survey year", y = "Invalid scientific name count") +
  theme_bw() + theme(panel.grid.minor.x = element_blank()) +
  ggtitle("M06.090")
  

# Plot percentage of 'NA' and 'Unrecorded' species observations per year
plot_diag_b <- left_join(
  Echo_M06090 %>%
    group_by(survey_year) %>%
    summarise(n = n()),
  Echo_M06090 %>%
    filter(scientific_name == "Unrecorded" | is.na(scientific_name)) %>%
    group_by(survey_year) %>%
    summarise(n_invalid_sci_name = n())
) %>%
  ggplot(., aes(x = survey_year, y = (n_invalid_sci_name / n * 100))) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks=seq(2005,2022,1)) +
  labs(x = "Survey year", y = "Invalid scientific name %") +
  theme_bw() + theme(panel.grid.minor.x = element_blank())

cowplot::plot_grid(plot_diag_a, plot_diag_b, align = "v", ncol = 1)


# Are particular users contributing 'NA' and 'Unrecorded' species observations?
Echo_M06090 %>%
  filter(scientific_name == "Unrecorded" | is.na(scientific_name)) %>%
  count(created_user, survey_year, sort = TRUE) %>%
  View()



# How many zero-sum M04.020 observation days are there for plant base, annual/biennial cover, and perennial cover at each reserve?
left_join(
  Echo_M04020 %>%
    group_by(prj_cde) %>%
    summarise(count = n()),
  Echo_M04020 %>%
    group_by(prj_cde, survey_date) %>%
    summarise(test = sum(plant_base_percent), n = n()) %>%
    filter(test == 0) %>%
    group_by(prj_cde) %>%
    summarise(plant_base_zero_days = n(), plant_base_zero_n = sum(n)),
  by = "prj_cde"
  ) %>%
  left_join(
    .,
    Echo_M04020 %>%
      group_by(prj_cde, survey_date) %>%
      summarise(test = sum(annual_biennial_perc_folige_cvr), n = n()) %>%
      filter(test == 0) %>%
      group_by(prj_cde) %>%
      summarise(annual_biennial_zero_days = n(), annual_biennial_zero_n = sum(n)),
    by = "prj_cde"
  ) %>%
  left_join(
    .,
    Echo_M04020 %>%
      group_by(prj_cde, survey_date) %>%
      summarise(test = sum(perennial_percent_foliage_cover), n = n()) %>%
      filter(test == 0) %>%
      group_by(prj_cde) %>%
      summarise(perennial_zero_days = n(), perennial_zero_n = sum(n)),
    by = "prj_cde"
  ) %>%
  mutate(plant_base_zero_days = replace_na(plant_base_zero_days, 0),
         plant_base_zero_n = replace_na(plant_base_zero_n,0),
         annual_biennial_zero_days = replace_na(annual_biennial_zero_days,0),
         annual_biennial_zero_n = replace_na(annual_biennial_zero_n,0),
         perennial_zero_days = replace_na(perennial_zero_days,0),
         perennial_zero_n = replace_na(perennial_zero_n,0)
         ) %>%
  # View()
  write.table(., "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)


left_join(
Echo_M04020_project %>%
  group_by(survey_date) %>%
  summarise(test = sum(plant_base_percent)) %>%
  filter(test == 0),
Echo_M04020_project %>%
  group_by(survey_date, site_id) %>%
  select(survey_date, site_id, quadrat_id, plant_base_percent, annual_biennial_perc_folige_cvr, perennial_percent_foliage_cover)
) %>% View()