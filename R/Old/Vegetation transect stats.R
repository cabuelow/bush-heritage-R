### Revision endpoint


#####
# Statistical analysis
#####
# functions can be found at https://www.statmethods.net/stats/index.html
# and https://www.statmethods.net/advstats/index.html


# 1.	Species composition each survey (year) in each target 
# and an indication of the dominant species in each target 
# and proportion that dominants cover (filtering out grasses)
vegetation_report %>% 
  group_by(year, target.from.lookup) %>% 
  filter(replaced.growth.form != "Grass") %>% 
  count(dominant_species) %>% 
  arrange(year, target.from.lookup)

# 2.	Changes in cover over time (by target) 
# for each growth form (chenopod, shrub etc) (filtering out grasses)
cover_change <- vegetation_report %>% 
  filter(replaced.growth.form != "Grass") %>% 
  group_by(year, target.from.lookup, site, replaced.growth.form) %>% 
  summarise(site.total.cover = sum(cover)) %>% 
  ungroup() %>% 
  group_by(year, target.from.lookup, replaced.growth.form) %>% 
  summarise(cover.mean = mean(site.total.cover),
            cover.sd = sd(site.total.cover, na.rm = TRUE))


# Change over time with grass filtered out
cover_change %>% 
  ggplot(., aes(x = year, y = cover.mean, colour = replaced.growth.form)) +
  facet_grid(replaced.growth.form ~ target.from.lookup) +
  geom_errorbar(aes(ymin = cover.mean - cover.sd, ymax = cover.mean + cover.sd, width = 0.2)) +
  geom_point() +
  geom_line() +
  ggtitle("Mean cover with standard deviation bars") +
  labs(x = "Time", y = "Average cover", colour = "Growth form")
# ggsave("Outputs/Mean cover with standard deviation bars.png", dpi = 600, width = 13, height = 7)

# Table for Miradi
cover_change_wide <- cover_change %>% 
  select(target.from.lookup, replaced.growth.form, cover.mean) %>% 
  pivot_wider(names_from = year, values_from = cover.mean)

write_csv(cover_change_wide, "Outputs/cover change table.csv", na = "")


# 3.	Changes in grasses by target 
# (probably going to be a struggle with the dataset!)
grass_data <- vegetation_report %>% 
  filter(replaced.growth.form == "Grass") %>% 
  group_by(year, target.from.lookup)

# Count of grass observations
grass_data %>% 
  count() %>% 
  ggplot(., aes(x = year, y = n)) +
  facet_wrap(~target.from.lookup) +
  geom_col() +
  labs(x = "Time", y = "Grass data observations")

# Grass cover (intercept length, all transects limited to first 50m) over time
grass_data_for_plot_or_report <- grass_data %>%
  group_by(year, target.from.lookup, site, replaced.growth.form) %>% 
  summarise(site.total.cover = sum(cover)) %>% 
  ungroup() %>% 
  group_by(year, target.from.lookup, replaced.growth.form) %>% 
  summarise(cover.mean = mean(site.total.cover),
            cover.sd = sd(site.total.cover, na.rm = TRUE)) 
grass_data_for_plot_or_report %>% 
  ggplot(., aes(x = year, y = cover.mean)) +
  facet_wrap(~target.from.lookup) +
  geom_errorbar(aes(ymin = cover.mean - cover.sd, ymax = cover.mean + cover.sd, width = 0.2)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  ggtitle("Mean grass cover with standard deviation bars")
# ggsave("Outputs/Mean grass cover with standard deviation bars.png", dpi = 600, width = 10, height = 7)


# Same data as plot above but to feed into Miradi
grass_data_for_report <- grass_data_for_plot_or_report %>% 
  filter(target.from.lookup == "Mulga Woodland on sand plain and/or ranges")


#####
# Statistics for 5 year report
#####

# Linear model for a) Chenopods, b) forb and c) shrub with rainfall and year as covariates

# get rain data
rain_bon_bon <- read_csv("Data/rain_16041.csv") %>% 
  mutate(date = ymd(str_glue("{year}-{month}-{day}"))) %>% 
  select(date, rain = rainfall) %>% 
  filter(date <= "2018-12-31") %>%
  filter(date > "2005-01-01") %>%
  # group_by(rain.month = floor_date(date, "month")) %>% 
  # summarise(rain_monthly = sum(rain, na.rm = TRUE)) %>%
  mutate(roll.rain = zoo::rollmean(rain, 90, na.pad = T, align = "right"))

# Make dataset for model
lm_data <- vegetation_report %>% 
  # filter(replaced.growth.form != "Grass") %>% 
  group_by(year, date, target.from.lookup, site, replaced.growth.form) %>% 
  summarise(site.total.cover = sum(cover)) %>% 
  ungroup() %>% 
  filter(replaced.growth.form == "Grass") %>% 
  left_join(rain_bon_bon)

# Visualise model (1 explanatory variable)
ggplot( aes(y = site.total.cover, x = date), data = lm_data) +
  geom_point() +
  stat_smooth(method = lm)#, formula = y ~ poly(x,2))

# Code up model
model_output <- lm(site.total.cover ~ date, data = lm_data)

# Temporal autocorrelation?
acf(rstandard(model_output))

# Model summary
summary(model_output)
plot(model_output, 2) 
plot(model_output, 3)

# Where should the rain data come from for Bon Bon?  # 16041 - short by a few years
# What rolling average for rain? When were the plots surveyed # Rolling 3 months
# Should year be treated as a linear predictor? # yes
# Are 2010 observations valid if collected in April not September? # 2011 dodgy. Perennial - shouldn't mess it up

