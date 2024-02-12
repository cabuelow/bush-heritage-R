# Title: Standardised summary of bird monitoring field trip and trends
# Author details: Michelle Hall, Angela Recalde Salas & Justin McCann
# Script and data info: This script loads and analyses bird survey data, including bird minutes and external data if present
# The data download should be saved to the 'Data' subfolder of this R Project folder


#### Before compiling the trip report... ####
# Save bird survey data from sharepoint 'Analysis outputs/Bird surveys' to the 'Data' subfolder of this R Project folder
# Save bird taxonomic info from sharepoint 'Analysis outputs/Species' to the 'Data' subfolder of this R Project folder. File is called:_Species_Observations_Birds_withTaxonomic&Introduced_updated2023-07-17.csv
# Save site data from sharepoint 'Analysis outputs/Sites' to the 'Data' subfolder of your own folder for site info, and define filepath below
# Save weather data from BoM climate data online to the 'Data' subfolder of your own folder for weather info, and define filepath below
# Make sure all dates are in ymd format eg 2024-01-15 (or review code below and edit where needed)
# Define variables below:
project_code <- "DARW" # Specify details of Reserve/Project and Session to create Report for
project_name <- "Charles Darwin Reserve"
recent_session <- 2023 #assuming one survey session per year
stratification <- "target_name" # use "target_name" to stratify by Conservation Target (as defined in Sites table); 
                    # alternatively use "management" and insert column titled "management" in Sites table, with values for each site e.g. restored/intact, baited/unbaited, etc
                    # or use "management" to combine target and management, and define values accordingly eg IntactWoodland/RestoredWoodland/IntactHeathland/RestoredHeathland (for descriptive data exploration; noting that for statistical analysis it is better to use factorial design and test management*target interaction)
Sys.timezone() #get code for local timezone (for reserve, to calculate survey start time relative to sunrise times)
projtimezone <- "Australia/Perth" 
#sitedatpath <- "C:/Users/michelle.hall/OneDrive - Bush Heritage Australia/1 Ecology/Monitoring & Analysis/Sites/Site Analysis/"
proofed <- "" # always start with "", change to "yes" when errors have been fixed, including species names tidied to allow match with taxonomic info from GBIF
owntaxonomicinfo <- "" # "yes" if own taxonomic info in Data folder, created using GBIF option below, otherwise ""

# Options
external <- "yes" # to include birdlife data not in database use "yes", otherwise change to ""
birdmin <- "yes" # to include bird minutes data use "yes", otherwise change to ""
weatherdatpath <- "C:/Users/michelle.hall/OneDrive - Bush Heritage Australia/1 Ecology/Monitoring & Analysis/Weather/Data/" # to include weather info, provide path do data, otherwise ""

# Notes
# Site data, from Sharepoint Analysis Outputs/Sites, can be in a different 'Site Analysis' folder, specified by the path above, to avoid duplicating stored data used in many analyses
#   However, if site variables, eg management category, are specific to this analysis, then
# External birdlife data to be saved as "{project_code} BirdlifeSurveys external.csv", and formatted with the same column headings as the Sharepoint Analysis Outputs export 
# Birdminutes data should be in the standard bird minutes export format from sharepoint Analysis outputs (can have multiple records per species per minute per survey)
# Weather data, from BoM Climate Data Online, can be in a different 'Weather Analysis' folder, specified by the path above, to avoid duplicating stored data used in many analyses
## Fix messy bird names to get meaningful species lists & taxonomic information!!! 
#     Can use species list in first run of field trip report (or excel pivot table) to identify problems
#     Common problems include - multiple names for a single species eg due to name changes, species vs subspecies, etc
#                             - missing scientific names
#     FIX PROBLEM NAMES in the central database (or other input files), then use fresh data export to re-run field trip report with correct details (and edit proofed=yes above incorporate taxonomic info into report).

#+ message=FALSE

#### Load Libraries ####
library(here)
library(RcppRoll) #
library(janitor)
library(tidyverse) 
library(readxl)
library(glue)
library(suncalc) # get sunrise and sunset times
library(rgbif) # species taxonomic and occurrence info from Global Biodiversity Information Facility (GBIF)
library(vegan) #to add cumulative curve
# library(BiodiversityR) # Checking if this is required. It was crashing on load of a dependency
library(data.table) 
library(cowplot)
library(iNEXT) 
library(gridExtra) # for arranging plots

#### Import Data ####

###### Bird Data #####

# Birdlife data - get database export from Sharepoint 'Analysis Outputs' and save to 'Data/{project_code}' folder of this project
bird_export <- read_csv(here(glue("Data/{project_code}/M05_050_Bird_Survey_Analysis_Join_Parent_Child_{project_code}.csv")), guess_max = 10000) %>% 
  clean_names() %>% # tidy variable names
  filter(prj_cde==project_code) %>% # filter to reserve-specific data
  mutate(Method="BirdLife", datetime=ymd_hms(date, tz=projtimezone)) %>% # 
  separate(date, into = c("date", "time"), sep = " ") %>% # fix date & time fields
  mutate(date=ymd(date)) %>%
  mutate(survey_duration_minutes=as.numeric(gsub("\\D", "", survey_duration_minutes))) %>%
  mutate(observer = case_when(staff_name =="" ~ additional_personnel, # use all available info on observer - will need proofing in central database
                             TRUE ~ staff_name)) %>%
  dplyr::select(oid,prj_cde,Method,site_id,year,datetime,date,time,parent_id,observer,additional_personnel,number_of_observers,
         all_species_recorded,water_level,site_field_notes,wind_strength,cloud_cover,survey_duration_minutes,
         english_name,scientific_name,count=birdlife_bird_count,bird_within_survey_area,field_notes=birdlife_field_notes) # select columns to match birdlife data

# IF there is external birdlife data, import it and add to main dataset
if(external=="yes"){ 
  birdlife_external <- read_csv(here(glue("Data/{project_code}/{project_code} BirdlifeSurveys external.csv")), guess_max = 10000) %>% 
    clean_names() %>% # tidy variable names
    mutate(time=as.character(time_start)) %>%
    mutate(Method="BirdLife", datetime=ymd_hms(paste(date,time_start), tz=projtimezone)) %>% # tidy variable names & select needed ones
    mutate(date=ymd(date)) %>%
    mutate(parent_id=paste(site_id,datetime,staff_name)) %>%
    mutate(bird_within_survey_area="Within survey limits") %>%
    mutate(observer = case_when(staff_name =="" ~ additional_personnel,
                                TRUE ~ staff_name)) %>%
    dplyr::select(source,prj_cde,Method,site_id,year,datetime,date,time,parent_id,observer,number_of_observers,
           all_species_recorded,site_field_notes,wind_strength,cloud_cover,survey_duration_minutes,
           english_name,scientific_name,count=birdlife_bird_count,bird_within_survey_area,field_notes=birdlife_field_notes) # select columns to match birdlife data

  # Combine external data with birdlife data
  bird_export <- bird_export %>%
    bind_rows(birdlife_external)
}


# IF there is Bird Minutes data - get database export from Sharepoint 'Analysis Outputs' and save to 'Data/{project_code}' folder of this project
if(birdmin=="yes"){ # If there is Bird Minutes data, import and add to birdlife data
  birdmin_export <- read_csv(here(glue("Data/{project_code}/M05_040_Bird_Minutes_Survey_Analysis_Join_Parent_Child_{project_code}.csv")), guess_max = 10000) %>% 
    clean_names() %>% # tidy variable names
    mutate(Method="BirdMinutes", datetime=ymd_hms(date, tz=projtimezone)) %>%
    separate(date, into = c("date", "time"), sep = " ") %>% # fix date & time fields
    mutate(date=ymd(date))%>%
    mutate(observer = case_when(is.na(lead_staff) ~ additional_personnel,
                                TRUE ~ lead_staff)) %>%
    dplyr::select(oid,prj_cde,Method,site_id,year,datetime,date,time,parent_id,observer,additional_personnel,number_of_observers,
           all_species_recorded,water_level,site_field_notes,wind_strength,cloud_cover,survey_duration_minutes,
           english_name,scientific_name,count,field_notes=bird_minutes_field_notes,minute) # select columns to match birdlife data
  
  # First summarise observations per minute, then  per survey, to join to birdlife data
  bird_minute_sppsummary <- birdmin_export %>%
    # aggregate to one row PER MINUTE per species & get total count
    group_by(prj_cde,Method,site_id,year,datetime,date,time,parent_id,observer,additional_personnel,number_of_observers,
             all_species_recorded,water_level,site_field_notes,wind_strength,cloud_cover,survey_duration_minutes,
             english_name,scientific_name,field_notes,minute) %>%
    summarise(minutecount=sum(count),minoid=min(oid))%>% # deal with distance estimates later/separately
    # aggregate to one row PER SURVEY per species & use maximum per-minute count, also calculate Bird Minutes 'Activity Index', and detection metrics
    group_by(prj_cde,Method,year,site_id,datetime,date,time,parent_id,observer,additional_personnel,number_of_observers,
             all_species_recorded,water_level,site_field_notes,wind_strength,cloud_cover,survey_duration_minutes,
             english_name,scientific_name,field_notes) %>%
    summarise(count=max(minutecount),summedminutecounts=sum(minutecount),activitysqrt=sqrt(sum(minutecount)),oid=min(minoid)) %>%
    mutate(survey_duration_minutes=as.numeric(gsub("\\D", "", survey_duration_minutes)))

  # Combine birdminutes data with birdlife data
  bird_export <- bird_export %>%
    bind_rows(bird_minute_sppsummary)
}

###### Taxonomic Data #####
# If you have species not on this list, use OPTION GBIF below to get taxonomic info for your species list
if(owntaxonomicinfo=="yes"){
  taxonomy_list <- read_csv(here(glue("Data/_Species List - bird surveys_withGBIF {project_code}.csv"))) %>% 
    clean_names()
} else {
  taxonomy_list <- read_csv(here(glue("Data/_Species_Observations_Birds_withTaxonomic&Introduced_updated2023-07-17.csv"))) %>% 
  clean_names()
  }

###### Site Data #####
# Get database export from Sharepoint 'Analysis Outputs' and save to 'Data' folder of your Site folder (as per filepath defined above)
sitedat <- read_csv(here(glue("Data/{project_code}/Sites_Point_Reference_Table_{project_code}.csv"))) %>% 
  clean_names() %>% # tidy variable names
  filter(prj_cde==project_code) # filter to reserve-specific data

##### Tidy up #####
# Filter to project of interest and tidy dates, covariates, etc
project.data <- bird_export %>% 
  filter(prj_cde == project_code) %>% # Filter to reserve of interest
  left_join(sitedat[,c('site_code','latitude_gda2020', 'longitude_gda2020','easting_mga','northing_mga',{{stratification}})], by=c('site_id'='site_code')) %>% # add conservation targets & locations in utm
#  filter(!is.na(stratification)) %>% # Filter to only sites with target name FIRST CHECK WHY MISSING!!
  mutate(wind_strength = fct_relevel(wind_strength, "Calm", "Leaves", "Twigs", "Branches"))%>% # set correct order
  mutate(wind = recode(wind_strength, `Calm`=0,`Leaves`=1,`Twigs`=2,`Branches`=3)) %>% # convert to linear score
  mutate(survey_hour = hour(datetime)) %>%
  mutate(survey_month = month(datetime),Season = case_when(survey_month %in% 3:5 ~ 'Autumn', #assign months to the appropriate season
                                                           survey_month %in% 6:8 ~ 'Winter',
                                                           survey_month %in% 9:11 ~ 'Spring',
                                                           TRUE ~ 'Summer')) %>%
  mutate(scientific_name=case_when(scientific_name=='Corvid sp.' ~ 'Corvus sp.', # fix names eg to match GBIF
                                   TRUE~scientific_name))
hist(project.data$survey_hour)

# tidy observer (case-by-case basis for manual corrections - ideally do them centrally in ESRI database!)
summary(as.factor(project.data$observer))
if(project_code == "FSTR"){
  project.data <- project.data %>% 
    mutate(observer = case_when(grepl('Sanders',observer) ~ 'Angela Sanders', #replace if observer CONTAINS 'Sanders'
                                grepl('Bilney',observer) ~ 'Vicky Bilney',
                                grepl('Zadow',observer) ~ 'Wayne Zadow',
                                grepl('ZW',observer) ~ 'Wayne Zadow',
                                observer == "ANGELA SANDERS" ~ 'Angela Sanders',
                                observer == "AS" ~ 'Angela Sanders',
                                TRUE ~ observer))
}
if(project_code == "DARW"){
  project.data <- project.data %>% 
    mutate(observer = case_when(grepl('Sanders',observer) ~ 'Angela Sanders', #replace if observer CONTAINS 'Sanders'
                                grepl('Parkhurst',observer) ~ 'Ben Parkhurst',
                                grepl('Radford',observer) ~ 'Jim Radford',
                                grepl('Parkhurst',observer) ~ 'Ben Parkhurst',
                                grepl('Stewart',observer) ~ 'Stewart Ford',
                                grepl('Boyd',observer) ~ 'Boyd Wykes',
                                grepl('Chris',observer) ~ 'Chris Wilder', # need to check all are him!!
                                grepl('Kady',observer)~ 'Kady Grosser', 
                                grepl('Kate',observer)~ 'Kate', # need to find out full name & check if she was lead (she is common denominator of remaining CCWA groups)
                                grepl('Brendan',observer) ~ 'Brendan Kinsella', # need to check if this is him!!
                                year=2022 & observer=='Volunteer - Western Australia' ~ 'Brendan Kinsella', # need to check if this is him!!
                                year=2023 & observer=='Volunteer - Western Australia' ~ 'Andrew Dickinson',
                                TRUE ~ observer))
}


###### Sun rise times #####
# get sunrise times for date range based on survey dates and location based on site locs and timezone defined at start
sundat <-
  getSunlightTimes(
    date = seq.Date(min(project.data$date), max(project.data$date), by = 1), #define time period, based on survey dates
    keep = c("sunrise", "sunriseEnd", "sunset", "sunsetStart"),
    lat = mean(project.data$latitude_gda2020,na.rm=T), # define location, based on survey lat/longs
    lon = mean(project.data$longitude_gda2020,na.rm=T),
    tz = projtimezone)

# add sunrise times and use to calculate survey start time relative to sunrise
project.data <- project.data %>% 
  left_join(sundat[,c("date","sunrise")],by="date") %>% 
  mutate(sunrise=lubridate::round_date(sunrise, unit="minute")) %>%
  mutate(starttime=ymd_hms(paste(date,time),tz=projtimezone)) %>%
  mutate(start_rel_sunrise = as.double(difftime(starttime,sunrise,units="hours")))

#### Add GBIF taxonomic info ####
# After tidying species names, add taxonomic info from GBIF
if(proofed == "yes"){
  project.data <- project.data %>% 
    left_join(taxonomy_list, by=c("scientific_name","english_name"))
}

# Optional - get taxonomic info for your species directly from GBIF (thanks Gareth!)
# If our GBIF list is missing some of your species, get your own taxonomic list (and update our list for BHA bird species if you like :) )
# taxonomy_list <- project.data %>%
#   group_by(english_name,scientific_name) %>%
#   summarise(n=length(english_name)) %>%
#   select(english_name,scientific_name) %>%
#   filter(!is.na(scientific_name)) %>% # remove records without scientific name
#   rowwise() %>%
#   mutate(gbif = list(name_backbone(scientific_name, class='Aves'))) %>% #specify Class to reduce ambiguity eg mollusc Acanthiza
#   unnest(cols = c(gbif)) %>%
#   select(scientific_name, english_name, canonical_name = canonicalName, class, family, genus, species,rank, status, match_type = matchType) %>%
#   ungroup()
# write_csv(taxonomy_list,glue("Data/_Species List - bird surveys_withGBIF {project_code}.csv"), na="") # save out to data to csv if you would like

# Wide-format data
###STILL NEED TO DEAL WITH Q ABOUT BIRDS OUTSIDE SURVEY LIMITS - okay for sample-coverage-based analyses, otherwise need to standardise
projectdatwide <- project.data %>%
  filter(!is.na(english_name)) %>%
  group_by(.data[[stratification]],year,site_id,parent_id,english_name) %>%
  summarise(Count=sum(count)) %>%
  arrange(english_name) %>%
  pivot_wider(names_from=english_name, values_from=Count) %>%
  arrange(year) %>%
  mutate_if(is.numeric, ~replace_na(., 0))
  
# Species count data - only include birds within survey limits
projectdatcounts <- project.data %>%
  filter(!is.na(english_name))%>% # exclude records missing species names
  filter(Method=="BirdMinutes" | (Method=="BirdLife" & bird_within_survey_area=="Within survey limits")) %>% # exclude records missing species names
  group_by(.data[[stratification]],year,site_id,parent_id,Method,observer,starttime,survey_month,survey_hour,start_rel_sunrise,cloud_cover,wind_strength) %>%
  summarise(CountOfSpecies=length(unique(english_name))) 

#### Session data ####
session.data <- project.data %>%
  filter(year == recent_session)

# wide format
sessiondatwide <- projectdatwide %>% 
  filter(year == recent_session) %>%
  dplyr::select(where(~ any(. != 0))) # remove species that were not detected at any sites in this session

sessiondatcounts <- projectdatcounts %>%
  filter(year == recent_session)

# with missing scientific name records removed
session.data.na.rm <- session.data %>%
  filter(!is.na(scientific_name))
  
#### Species lists ####

# Create species list for Reserve
species_list_reserve <- project.data %>%
#  filter(!is.na(english_name))%>%
  group_by(english_name,scientific_name) %>%
  summarise(nYears=length(unique(year)),nSites=length(unique(site_id)),nSurveys=length(unique(parent_id)))
# save species list to proof each species is uniquely identified with english and scientfic names (eg not different spellings, old/new names, mix of species & sub-species names etc)
# fix all errors at source (ie central ArcGIS database, or external data import) and then re-run with corrected export
#write_csv(species_list_reserve,file = glue("Outputs/{project_code} Species List - bird surveys.csv"),na = "")

# Create species list for Session
species_list_session <- session.data %>%
  #  filter(!is.na(english_name))%>%
  group_by(english_name,scientific_name) %>%
  summarise(nSites=length(unique(site_id)),nSurveys=length(unique(parent_id)))


#### Summary values ####
# Save summary info to include in report
nSites <-length(unique(session.data$site_id))
nSurveys <-length(unique(session.data$parent_id))
nStratGroups <-length(unique(session.data[[stratification]]))
if(stratification=="target_name"){
  stratname <- "Conservation Targets"
  }else if(stratification=="management"){
  stratname <- "Management Zones"}
startdate <- format(min(session.data$date),format='%d %b %Y')
enddate <- format(max(session.data$date),format='%d %b %Y')
nObservers <- length(unique(session.data$observer))
nMethods <- length(unique(session.data$Method))
observerlist <- as.character(str_c(unique(unlist(session.data$observer)), collapse = ", "))
methodlist <- as.character(str_c(unique(unlist(session.data$Method)), collapse = ", "))
individual_count <- sum(session.data.na.rm$count) ###NB!!! **LOTS** OF ERRORS TO FIX IN COUNTS FOR DARW 2022

target_effort_summary <- session.data.na.rm %>%
  group_by(.data[[stratification]]) %>% 
  summarise(nSites=length(unique(site_id)),nSurveys=length(unique(parent_id)))
  
if(proofed == "yes"){
  # if taxonomic information has been added, get more accurate taxon counts
  # for species, only include genus-level IDs (eg fairywren sp) in the count when there are no species-level IDs present
taxon_count <- session.data.na.rm %>% # get species count without extras due to taxa not identified to species level
  group_by(family,genus,scientific_name,species) %>%
  summarise(Count=sum(count)) %>%
  group_by(family,genus) %>%
  summarise(nBirds=sum(Count),IDedSpeciesPerGenus=sum(!is.na(species)),unIDedSpeciesPerGenus=sum(is.na(species))) %>%
  mutate(SpeciesPerGenus=case_when(IDedSpeciesPerGenus=="0" ~unIDedSpeciesPerGenus,
                                   TRUE ~IDedSpeciesPerGenus))
species_count <- sum(taxon_count$SpeciesPerGenus)
family_count <- length(unique(session.data.na.rm$family))

# Table 1 taxon summary - can only do this after taxonomic info has been added
session_taxon_summary <- session.data.na.rm %>%
  group_by(.data[[stratification]],family, genus, species,scientific_name, english_name) %>% 
  summarise(Count=sum(count)) %>%
  group_by(.data[[stratification]],family,genus) %>%
  summarise(nBirds=sum(Count),IDedSpeciesPerGenus=sum(!is.na(species)),unIDedSpeciesPerGenus=sum(is.na(species))) %>%
  mutate(SpeciesPerGenus=case_when(IDedSpeciesPerGenus=="0" ~unIDedSpeciesPerGenus,
                                   TRUE ~IDedSpeciesPerGenus)) %>%
  group_by(.data[[stratification]]) %>%
  summarise(nFamilies=length(unique(family)),nSpecies=sum(SpeciesPerGenus),nBirds=sum(nBirds)) %>%
  left_join(target_effort_summary)

} else {
  species_count <- length(unique(session.data.na.rm$scientific_name)) # if taxonomic info not available, just get raw species counts
  session_taxon_summary <- session.data.na.rm %>%
    group_by(.data[[stratification]],scientific_name, english_name) %>% 
    summarise(Count=sum(count)) %>%
    group_by(.data[[stratification]]) %>%
    summarise(nBirds=sum(Count)) %>%
    left_join(target_effort_summary)
  }


# Figure 1 - plot number of species detected per survey, in each conservation target
sprichnessbytarget <- session.data.na.rm %>%
  group_by(.data[[stratification]],parent_id) %>%
  summarise(CountOfSpecies=length(unique(scientific_name))) %>%
  ggplot(aes(y=CountOfSpecies, x=.data[[stratification]], fill=.data[[stratification]]))+ 
  geom_boxplot() + 
  geom_jitter(width = 0.15) +
  labs(y="Number of species per survey", x="Conservation Target")+
  theme_classic() + #get clear background
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1)) + # angle target names to avoid overlap
  theme(legend.position = "none") #remove legend of target names (duplicates x-axis & takes too much space)


# Table 2 species list - include missing species names to facilitate proofing
session_species_list <- session.data %>%
  group_by(english_name,scientific_name) %>% 
  summarise(Count=sum(count),nTargets=length(unique(.data[[stratification]])),nSites=length(unique(site_id)),nSurveys=length(unique(parent_id))) %>%
  ungroup()

# Table 3 detections by method & observer
if(proofed == "yes"){ # if taxonomic info available, then factor in family and genus 
method_observer_detections <- session.data.na.rm %>%
  group_by(Method,observer,family, genus, species,scientific_name, english_name) %>% 
  summarise(Count=sum(count)) %>%
  group_by(Method,observer,family,genus) %>%
  summarise(nBirds=sum(Count),IDedSpeciesPerGenus=sum(!is.na(species)),unIDedSpeciesPerGenus=sum(is.na(species))) %>%
  mutate(SpeciesPerGenus=case_when(IDedSpeciesPerGenus=="0" ~unIDedSpeciesPerGenus,
                                   TRUE ~IDedSpeciesPerGenus)) %>%
  group_by(Method,observer) %>%
  summarise(nFamilies=length(unique(family)),nSpecies=sum(SpeciesPerGenus),nBirds=sum(nBirds))
} else {
  method_observer_detections <- session.data.na.rm %>%
    group_by(Method,observer,scientific_name, english_name) %>% #dropped species
    summarise(Count=sum(count)) %>%
    group_by(Method,observer) %>%
    summarise(nSpecies=length(unique(scientific_name)),nBirds=sum(Count))
}


#### Communities ####

library(vegan) # for multivariate analysis
library(MASS)  # for MDS
library(labdsv)  ## for identifying indicator species

communitydat <- sessiondatwide %>%
  group_by(.data[[stratification]],year,site_id) %>%
  summarise(across(where(is.numeric), sum))    ## NB this step pools all replicate surveys for a site (should reduce the number of surveys that don't match any others)

freqNMDS=metaMDS(communitydat[,-c(1:3)], # community matrix
                 distance = "bray", # method to calculate distance matrix
                 k = 2, # n dimensions - increase to improve high stress (>0.2)
                 trymax = 20) # n iterations - increase to improve convergence

#Check Shepard plot (scatter around  regression between the interpoint distances in the final configuration (i.e., the distances between each pair of communities) against original dissimilarities
#High scatter means high stress ie distances between communities not well preserved in reduced dimensions
stressplot(freqNMDS)
stressval <- round(freqNMDS$stress, digits=3)#get stress value (model fit, how well the data is represented by the reduced dimensions (stress < 0.05 is excellent, < 0.1 is great, < 0.2 is good/ok, > 0.3 is poor)

# Get scores and plot
speciesscores <- vegan::scores(freqNMDS, display="species")
sitescores <- as.data.frame(vegan::scores(freqNMDS, display="sites"))

nmdsplot <- sitescores %>%
  ggplot(aes(x = NMDS1, y = NMDS2, color = communitydat$target_name)) +
  geom_point() +
  #  geom_point(aes(size = wood_density), alpha = 0.5) + 
  stat_ellipse() +
#  scale_color_manual(values = c(F = "#993D00", WP = "#117733", NWP ="#FFCC00")) +
  labs(title="NMDS of Bird Communities",colour=stratname) +
  annotate("text", x=Inf, y = Inf, label=glue("Stress={stressval}"),vjust=1, hjust=1)+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))



##### Indicator values #####
# To deal with potential for flawed conclusions based on simple point-in-time metrics in highly variable metrics, provide:
# i) metrics with 95% confidence intervals -> only metrics with non-overlapping confidence intervals can be considered 'different'
# ii) metrics based on rolling averages -> where there is a lot of variability between samples, using rolling averages can give more robust indicators of current status
#       The concept of rolling averages should be reasonably familiar to people, thanks to reporting on covid-related mortalities and climate change-related temperature changes
#       The time-window for the rolling averages can be defined as desired, 5-years is used here, to include some year-to-year variability and fit with evalutation reporting time-frames

##### Species Diversity per Conservation Target #####
#### Prepare data for estimating diversity with iNext ####

#note, both abundance- and incidence-based approaches are included below, but incidence-based seems more robust and therefore only those results are included in the report
# For abundance data, the sampling unit is an individual. 
# # for iNEXT datatype="abundance", input should be the sample abundance vector
# When there are N assemblages, input data consist of an S by N abundance matrix
# or N lists of species abundances.

# For incidence data, the sampling unit is usually a trap, net, quadrat, plot, or timed survey.
# for iNEXT datatype="incidence_raw", input data should be a species-by-sampling-unit matrix; 
# when there are N assemblages, input data consist of N matrices via a list object, 
# with each matrix being a species-by-sampling-unit matrix. 

# This case, assemblages are the Conservation Targets
# need row for each species, and abundance in each Conservation Target
# or presence per survey, in 3 separate matrices

project.data.5y.narm <- project.data %>%
  filter(!is.na(english_name)) %>% #FIND OUT WHY THERE ARE MISSING SPECIES NAMES!!
  filter(!is.na(.data[[stratification]])) %>% # remove records with not allocated to conservation targets - PROOF AND FIX first
  filter(year > (recent_session-5))

# Get raw Incidence dataset to calculate diversity metrics
#   use loop to create table for each conservation target, 
#   then append each table to a list
div_incidraw <- list()  # Create empty list
for(t in unique(project.data.5y.narm[[stratification]])){   # start for-loop                                    # Head of for-loop
  new_list_element <- project.data.5y.narm %>%        # Create new list element
    filter(.data[[stratification]]==t) %>%
    group_by(english_name,parent_id) %>%
    summarise(Count=1) %>% # only presences/incidences in record currently, so put all as 1s
    pivot_wider(names_from = parent_id, values_from = Count) %>%
    column_to_rownames("english_name") %>% 
    mutate_if(is.numeric, ~replace_na(., 0)) %>%
    filter(rowSums(across(where(is.numeric)))!=0) %>% # take out rows with all zeros (species never detected in any survey in set)
    as.data.frame()  
  div_incidraw[[length(div_incidraw) + 1]] <- new_list_element    # Append new list element
}
names(div_incidraw) <- unique(project.data.5y.narm[[stratification]])

#### Estimate diversity indices ####
SurveyInfInc <- DataInfo(div_incidraw) # Check info on # individuals, # species, & Sample Coverage per site
c1inc <- iNEXT(div_incidraw, q=0, datatype="incidence_raw") # Get diversity estimates

#Check parts of output
#head(c1inc$DataInfo) # (i) $DataInfo: summary information;
#head(c1inc$iNextEst) # (ii) $iNextEst: diversity estimates and statistics for a series of rarefied and extrapolated samples; 
#head(c1inc$AsyEst) # (iii) $AsyEst: asymptotic diversity estimates with related statistics. 
#write_csv(c1$AsyEst, file = "Outputs/AsymtoticDiversityEstimates_extraRefSurveys.csv",na = "")
#write_csv(c1$AsyEst, file = "Outputs/AsymtoticDiversityEstimates_noextraRefSurveys.csv",na = "")
#write_csv(c1$AsyEst, file = "Outputs/AsymtoticDiversityEstimates_Incidence_noextraRefSurveys.csv",na = "")

# Plot
div_plot_inc <- iNEXT(div_incidraw, q=c(0, 1, 2), datatype="incidence_raw") 
ggiNEXT(div_plot_inc, type=1, facet.var="Assemblage") 
#ggsave("Outputs/DiversityEstimates_Incidence_noextraRefSurveys.jpg")

# ### Abundance-based approach
# # Get Abundance dataset to calculate diversity metrics
# div_abund <- project.data.5y.narm %>%
#   group_by(.data[[stratification]],english_name) %>%
#   summarise(Count=sum(count)) %>%
#   pivot_wider(names_from = {{stratification}}, values_from = Count) %>%
#   column_to_rownames("english_name") %>% 
#   mutate_if(is.numeric, ~replace_na(., 0)) %>%
#   as.data.frame()
# head(div_abund)
# 
# SurveyInfAbund <- DataInfo(div_abund) # Check info on # individuals, # species, & Sample Coverage per site
# c1abund <- iNEXT(div_abund, q=0, datatype="abundance") # Get diversity estimates
# 
# #Check parts of output
# head(c1abund$DataInfo) # (i) $DataInfo: summary information;
# head(c1abund$iNextEst) # (ii) $iNextEst: diversity estimates and statistics for a series of rarefied and extrapolated samples;
# head(c1abund$AsyEst) # (iii) $AsyEst: asymptotic diversity estimates with related statistics.
# 
# # Plot
# div_plot_abund <- iNEXT(div_abund, q=c(0, 1, 2), datatype="abundance") 
# ggiNEXT(div_plot_abund, type=1, facet.var="Assemblage")
# #ggsave("Outputs/DiversityEstimates_extraRefSurveys.jpg")
# #ggsave("Outputs/DiversityEstimates_noextraRefSurveys.jpg")


## Characteristic species ####
communitydat5y <- project.data.5y.narm %>%
  filter(!is.na(english_name)) %>%
  group_by(.data[[stratification]],year,site_id,english_name) %>% # NB this is grouping all replicate surveys per site per year
  summarise(Count=sum(count)) %>%
  arrange(english_name) %>%
  pivot_wider(names_from=english_name, values_from=Count) %>%
  arrange(year) %>%
  mutate_if(is.numeric, ~replace_na(., 0))


ind.comm<-indval(communitydat5y[,-c(1:3)], communitydat5y[[stratification]])
summary(ind.comm) 
#ind.comm
# 
# ind.mh$indval

## Plot change over time ####

# check how often targets have been surveyed
ctsurveys <- project.data %>%
  group_by(.data[[stratification]]) %>%
  summarise(nSurveys=length(unique(parent_id)))
  
# Figure 2 - plot number of species detected per survey over time, in each conservation target
if(birdmin == "yes"){
  sprichnessovertime <- project.data %>%
  left_join(ctsurveys) %>%
  filter(!is.na(scientific_name))%>% # exclude records missing species names
  filter(nSurveys>=10)%>% # exclude records from rarely surveyed conservation targets
  group_by(.data[[stratification]],year,parent_id,Method) %>%
  summarise(CountOfSpecies=length(unique(scientific_name))) %>%
  ggplot(aes(y=CountOfSpecies, x=year, colour=.data[[stratification]], shape=Method))+ 
    geom_count(aes(color = .data[[stratification]])) + # use instead of point to show number of observations at each point
    geom_smooth() +
  facet_wrap(~.data[[stratification]], ncol=2) +
  #    scale_colour_manual(values=c('green4', 'steelblue4')) + scale_shape_manual(values=c(1,2))+
    labs(title="Variation over time in number of bird species detected",x="Year", y = "Species Count (per 20 min survey ± 95% C.I.)")+
    theme_classic()+
    theme(panel.background = element_rect(colour = "black", linewidth=1))
}else{
  sprichnessovertime <- project.data %>%
    left_join(ctsurveys) %>%
    filter(!is.na(scientific_name))%>% # exclude records missing species names
    filter(nSurveys>=10)%>% # exclude records from rarely surveyed conservation targets
    group_by(.data[[stratification]],year,parent_id) %>%
    summarise(CountOfSpecies=length(unique(scientific_name))) %>%
    ggplot(aes(y=CountOfSpecies, x=year, colour=.data[[stratification]]))+ 
    geom_count(aes(color = .data[[stratification]])) + # use instead of point to show number of observations at each point
    geom_smooth() +
    facet_wrap(~.data[[stratification]], ncol=2) +
    #    scale_colour_manual(values=c('green4', 'steelblue4')) + scale_shape_manual(values=c(1,2))+
    labs(title="Change over time in number of bird species detected",x="Year", y = "Species Count (per 20 min survey ± 95% C.I.)")+
    theme_classic()+
    theme(panel.background = element_rect(colour = "black", size=1))
}

# Plot covariate effects - use full dataset
c1 <- ggplot(projectdatcounts,aes(y = CountOfSpecies, x = wind_strength)) +
  geom_boxplot(fill='azure2', outlier.shape = NA) +
  geom_jitter(alpha = 0.2,size=1)+
  labs(title = "Wind strength", y = "Number of Species", x = "Wind strength")+
  theme_classic()

c2 <- ggplot(projectdatcounts,aes(y = CountOfSpecies, x = as.factor(cloud_cover))) +
  geom_boxplot(fill='azure2', outlier.shape = NA) +
  geom_jitter(alpha = 0.2,size=1)+
  labs(title = "Cloud cover", y = "Number of Species", x = "Cloud cover")+
  theme_classic()

c3 <- ggplot(projectdatcounts,aes(y = CountOfSpecies, x = as.factor(survey_hour))) +
  geom_boxplot(fill='azure2', outlier.shape = NA) +
  geom_jitter(alpha = 0.2,size=1)+
  labs(title = "Hour of day", y = "Number of Species", x = "Survey hour (of day)")+
  theme_classic()

c4 <- ggplot(projectdatcounts,aes(y = CountOfSpecies, x = start_rel_sunrise)) +
  geom_count() + # use instead of point to show number of observations at each point
  geom_smooth() +
  labs(title = "Time relative to sunrise", y = "Number of Species", x = "Time of day (h relative to sunrise)")+
  theme_classic()

c5 <- ggplot(projectdatcounts,aes(y = CountOfSpecies, x = as.factor(survey_month))) +
  geom_boxplot(fill='azure2', outlier.shape = NA) +
  geom_jitter(alpha = 0.2,size=1)+
  labs(title = "Month", y = "Number of Species", x = "Survey month")+
  theme_classic()

c6 <- ggplot(projectdatcounts,aes(y = CountOfSpecies, x = observer)) +
  geom_boxplot(fill='azure2', outlier.shape = NA) +
  geom_jitter(alpha = 0.2,size=1)+
  labs(title = "Observer", y = "Number of Species", x = "Observer")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

if(birdmin == "yes"){
  c7 <- ggplot(projectdatcounts,aes(y = CountOfSpecies, x = Method)) +
    geom_boxplot(fill='azure2', outlier.shape = NA) +
    geom_jitter(alpha = 0.2,size=1)+
    labs(title = "Survey method", y = "Number of Species", x = "Method")+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}
