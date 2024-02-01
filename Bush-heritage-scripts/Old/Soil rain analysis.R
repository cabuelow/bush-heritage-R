# Title: EOM soil surface quadrat rainfall analysis
# Author details: Justin McCann, Gareth Davies


# # Deprecated - bomrang (Bureau of Meteorology data client)
# ## As of March 2021, BOM policy prevents automated weather data retrieval, and some bomrang functions return errors.
# ## bomrang has been archived from CRAN and may be permanently broken. See https://docs.ropensci.org/bomrang/
# 
# ## Try first:
# install.packages("bomrang") # Produces an error as of March 2022
# 
# ## If the above fails, bomrang can be installed with
# if (!require("remotes")) {
#   install.packages("remotes", repos = "http://cran.rstudio.com/")
#   library("remotes")
# }
# install_github("ropensci/bomrang", build_vignettes = TRUE)
# 
# ## Once bomrang is installed, load with:
# library(bomrang)



#####
# Is rain a factor to think about here?
#####

# using rainfall when analysing vegetation trends and looking at rainfall in the previous 3 months (for annuals) and then the rainfall in the 12 months prior to each vegetation sampling trip for other analyses

# Automated lookup of BOM station on project or site coordinates could go here if bomrang was working
# (would require conversion of eastings and northings to lats and longs)
# bomrang::sweep_for_stations(lat_long)

# Semi-automated lookup could likewise go here, e.g.
# sweep_for_stations(c(-35.899, 149.131)) # Select most appropriate result - shortest distance with most similar elevation
# bomrang::get_historical(bom_station_id, type = "rain") %>% 
#   write_csv(glue("Data/rain_{station_id}.csv"))

# In the interim, for manual station search and data download, visit http://www.bom.gov.au/climate/data/stations/ and download daily rainfall data

# Create plot of monthly rainfall (with rolling averages)
plot_a <- read_csv(bom_rain_datafile) %>%
  clean_names() %>%
  mutate(date = ymd(str_glue("{year}-{month}-{day}"))) %>%
  select(date, rain = rainfall_amount_millimetres) %>%
  filter(date >= ymd(min(soil_qdt_all$survey_year),truncated=2)) %>% # year of first observation converted to date (Jan 1st)
  # filter(date >= min(soil_qdt_all$survey_date) %m-% months(3)) %>% # 3 months before first observation
  filter(date < max(soil_qdt_all$survey_date)) %>%
  group_by(rain.month = floor_date(date, "month")) %>%
  summarise(rain_monthly = sum(rain, na.rm = TRUE)) %>%
  ggplot(., aes(x = rain.month)) +
  geom_line(aes(y = rain_monthly), size = 0.2, colour = "blue") +
  geom_line(aes(y = zoo::rollmean(rain_monthly, 6, na.pad = T, align = "right")), colour = "red") +
  theme_minimal() +
  labs(x = element_blank(), y = bquote('Rainfall ('*'mm day'^-1*')'))

# Create axis-aligned plot of plant ground cover percentage with standard deviation
plot_b <- soil_qdt_all %>%
  mutate(year = ymd(survey_year, truncated = 2)) %>% # convert year to January 1st date (to make axis alignment simpler)
  group_by(year) %>%
  mutate(mean_plant = mean(plant_base_percent), sd_plant = sd(plant_base_percent)) %>% 
  ggplot(., aes(x = year, y = mean_plant)) +
  geom_errorbar(aes(ymin = mean_plant - sd_plant, ymax = mean_plant + sd_plant), width = 0.2) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  coord_cartesian(xlim = as_date(layer_scales(plot_a)$x$range$range)) + # Copy x range from plot_a to ensure axis alignment
  labs (x = "Year", y = "Percent plant cover at surface\n with standard deviation bars") 

# Alternate: Create axis-aligned plot of annual/biennial cover percentage with standard deviation
plot_b <- soil_qdt_all %>%
  mutate(year = ymd(survey_year, truncated = 2)) %>% # convert year to January 1st date (to make axis alignment simpler)
  group_by(year) %>%
  mutate(mean_annbi = mean(annual_biennial_perc_folige_cvr), sd_annbi = sd(annual_biennial_perc_folige_cvr)) %>% 
  ggplot(., aes(x = year, y = mean_annbi)) +
  geom_errorbar(aes(ymin = mean_annbi - sd_annbi, ymax = mean_annbi + sd_annbi), width = 0.2) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  coord_cartesian(xlim = as_date(layer_scales(plot_a)$x$range$range)) + # Copy x range from plot_a to ensure axis alignment
  labs (x = "Year", y = "Percent annual/biennial cover\n with standard deviation bars") 

# Show aligned plots
cowplot::plot_grid(plot_b, plot_a, align = "v", ncol = 1)


#####
# Statistical analysis for bare ground and cryptogam with rainfall and year as covariates
#####
# functions can be found at https://www.statmethods.net/stats/index.html
# and https://www.statmethods.net/advstats/index.html


#####
# Bare ground percentage relationship to time since ownership and rain
#####
# get rain data
rain_project <- read_csv(bom_rain_datafile) %>%
  clean_names() %>%
  mutate(date = ymd(str_glue("{year}-{month}-{day}"))) %>%
  select(date, rain = rainfall_amount_millimetres) %>%
  filter(date >= date_filter_start) %>%
  filter(date < date_filter_end) %>%
  mutate(roll.rain = zoo::rollmean(rain, 60, na.pad = T, align = "right"))

# Make dataset for model
lm_data <- soil_qdt_all %>%
  select(year = survey_year, date = survey_date, site_target_name, site_id, bare_ground_percent) %>%
  left_join(rain_project)

# Visualise model (1 explanatory variable)
ggplot( aes(y = bare_ground_percent, x = date, colour = site_target_name), data = lm_data) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x,2))

# Code up model
model_output <- lm(bare_ground_percent ~ date + roll.rain, data = lm_data)

# Temporal autocorrelation?
acf(rstandard(model_output), lag.max = 7)

# Model summary
summary(model_output)
plot(model_output, 2)
plot(model_output, 3)


#####
# Cryptogam ground percentage relationship to time since ownership and rain
#####
# get rain data
rain_project <- read_csv(bom_rain_datafile) %>%
  clean_names() %>%
  mutate(date = ymd(str_glue("{year}-{month}-{day}"))) %>% 
  select(date, rain = rainfall_amount_millimetres) %>% 
  filter(date >= date_filter_start) %>%
  filter(date < date_filter_end) %>%
  # group_by(rain.month = floor_date(date, "month")) %>% 
  # summarise(rain_monthly = sum(rain, na.rm = TRUE)) %>%
  mutate(roll.rain = zoo::rollmean(rain, 60, na.pad = T, align = "right"))

# rain_bon_bon <- read_csv("Data/rain_16041.csv") %>% 
#   mutate(date = ymd(str_glue("{year}-{month}-{day}"))) %>% 
#   select(date, rain = rainfall) %>% 
#   filter(date <= "2018-12-31") %>%
#   filter(date > "2005-01-01") %>%
#   # group_by(rain.month = floor_date(date, "month")) %>% 
#   # summarise(rain_monthly = sum(rain, na.rm = TRUE)) %>%
#   mutate(roll.rain = zoo::rollmean(rain, 60, na.pad = T, align = "right"))

# Make dataset for model
lm_data <- soil_qdt_all %>% 
  select(year = survey_year, date = survey_date, site_target_name, site_id, cryptogams_percent) %>% 
  left_join(rain_project) #%>% 
# filter(target.from.lookup == "Western Myall over Pearl Bluebush") #Western Myall over Pearl Bluebush #Chenopod Shrublands


# Visualise model (1 explanatory variable)
ggplot( aes(y = cryptogams_percent, x = date, colour = site_target_name), data = lm_data) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x,2)) +
  labs(x = "Date", y = "% Cryptogam")

# Code up model
model_output <- lm(cryptogams_percent ~ poly(date,2) + roll.rain, data = lm_data) # No polynomial

# Temporal autocorrelation?
acf(rstandard(model_output), lag.max = 7)

# Model summary
summary(model_output)
plot(model_output, 2) 
plot(model_output, 3)

# Do we have enough points?
lm_data %>% 
  select(year, site_id) %>% 
  distinct() %>%
  group_by(year) %>% 
  summarise(n())

