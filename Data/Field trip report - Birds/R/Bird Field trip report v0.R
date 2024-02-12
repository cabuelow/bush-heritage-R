#' ---
#' title: "Bird monitoring trip report"
#' author: "Michelle Hall, Angela Recalde-Salas & Justin McCann"
#' output:
#'    word_document:
#'      toc: true
#'      highlight: haddock  
#' ---

#+ setup, include=FALSE
knitr::opts_chunk$set(warning = FALSE, message = FALSE, echo = FALSE,fig.width=9)

# Some useful info about code for compiling reports from R scripts: 
# https://www.aliesdataspace.com/2018/12/using-r-studio-s-compile-report/
#  http://brooksandrew.github.io/simpleblog/articles/render-reports-directly-from-R-scripts/
#  https://happygitwithr.com/r-test-drive.html
#  https://bookdown.org/yihui/rmarkdown/appearance-and-style-1.html


library(here)
library(RcppRoll)
library(janitor)
library(tidyverse)
library(lubridate)
library(readxl)
library(glue)
library(gridExtra)
library(flextable)

source(here("R/Bird Field trip report base script v0.R")) # fun the 'workhorse' analysis script to generate content for the report

#'
#'#  Results of `r recent_session` bird monitoring at `r project_name`
#'## Bird counts and species detected
#' We surveyed the bird community at `r project_name` in `r recent_session`, detecting a total of `r individual_count` birds of `r species_count` species in `r nStratGroups` `r stratname` (Figure 1, Table 1 & 2). We conducted `r nSurveys` surveys of `r nSites` sites from `r startdate` to `r enddate`. Table 3 summarises birds detected by survey method and observer.
#'
#' **Figure 1. Number of species detected in `r stratname`.**
#' Plot shows variation in the number of species detected per survey across different `r stratname`. Each point shows the result of one survey. Boxes enclose the middle 50% of the data (inter-quartile range).
#'
print(sprichnessbytarget)
#'
#'
#' **Table 1. Survey summary.** 
#' Distribution of bird detections across `r stratname`.

session_taxon_summary %>%
  rename('Conservation Target' = target_name) %>%
  flextable() %>% #need to edit scientific name as italic
  autofit() %>%
  line_spacing(space = 0.6)

#'
#'
# Species list for recent trip   -> FOR FAMILY SUBHEADINGS: https://stackoverflow.com/questions/55360767/flextable-how-to-show-group-headings-as-the-value-only-rather-as-the-variable-n?rq=4
#'
#' **Table 2. `r project_name` species list for `r recent_session`.**
#' Species list showing the number of birds detected, and the number of `r stratname`, sites and surveys they were detected in.

session_species_list %>%
  rename('Species'=english_name,'Scientific Name'=scientific_name) %>%
  arrange(Species)%>%
  flextable() %>% #need to edit scientific name as italic
  autofit() %>%
  line_spacing(space = 0.6)
#'
#'
#'
#' **Table 3. Bird detections by methods and observers.**
#' Surveys were conducted by `r nObservers` observers using `r nMethods` methods. Counts from Birdlife surveys are the number of birds detected in a 2ha area during a 20-minute search period.
#' `r if(birdmin=="yes"){"Counts from Bird Minutes surveys are the maximum number of birds detected in one of 20 consecutive one-minute point counts (an 'instantaneous point-count'). Bird Minutes surveys include all detections by sight and sound within the vegetation type, regardless of distance from point (consistent with the standard 'point count' approach."}`
#'
method_observer_detections %>%
  flextable() %>%
  autofit() %>%
  line_spacing(space = 0.6)
#'
#'
#'
#'
#'## Communities
#'
#' The composition of bird communities may also differ among  `r stratname`. Figure 1 show bird communities in the `r stratname` in multidimensional space with each site positioned closest to other sites with which it shares the most species.
#' 
#' **Figure 1. Species diversity estimates for `r stratname`.**
#' Plot visualising bird communities in `r stratname` using non-metric multidimensional scaling (NMDS). Each point represents a site (any replicate surveys are pooled). Low stress values (<0.2) suggest distances in the 2-dimensional representation are a reasonable fit for actual dissimilarity between samples.
nmdsplot
#'
#'
#'
#'# Current Indicator values 

#'## Diversity Estimates for `r stratname`
#' Estimates of the number of species in the different `r stratname` (Fig. 2 and Table 4), considering the total number of species, the number of species that are common, and the number of species that are very common (Species Richness, Shannon Diversity, and Simpson diversity, respectively). A low number of very common species could indicate over-abundant species dominating the community.
#' Since each survey only detects a subset of the species present in the `r stratname`, more robust estimates were calculated by pooling all sites and surveys for `r stratname` in the last 5 years to encompass variation among years, increase sample sizes, and determine sample coverage (whether fewer or no new species are detected as more surveys are done).
#' Estimates are incidence-based Hills diversity estimates with 95% confidence intervals (from iNEXT R package).
#' 
#' **Figure 2. Species diversity estimates for `r stratname`.**
#' 
#' Plots show the number of species detected in `r stratname` relative to the number of surveys, with the total number of species shown in orange (Species richness = q0), the number of common species in blue (Shannon Diversity = q1), and the number of very common species in purple (Simpson diversity = q2). 
#' Solid lines show observed estimates (rarefaction-based) based on the surveys that were conducted and dotted lines show estimates based on extrapolating to likely numbers if more surveys had been conducted. Extrapolated lines with little or no increase in the number of species indicate high sample coverage (few or no new species detected as more surveys are conducted). Shaded bands show confidence intervals for estimates - narrow bands indicate high confidence that estimates lie within a narrow range. 
# div_plot_inc <- div_plot_inc %>%
#   recode(iNextEst$size_based$Order.q, `0` = "Species richness", `1` = "Shannon diversity", `2` = "Simpson diversity")

#+ fig.width=10, fig.height=10, dpi=200
ggiNEXT(div_plot_inc, type=1, facet.var="Assemblage") +
  #  facet_grid(Assemblage ~ .) +
  facet_wrap(~Assemblage, ncol=2) +
  labs(x="Number of surveys")+
  theme_bw()
#'
#'
#'
#' **Table 4. Species diversity estimates for `r stratname`.**
#' Observed values are the actual number of species detected in each category. Estimated values are estimates at the point where no new species are detected in new surveys (asymptotic estimates with standard error (s.e.) and lower and upper confidence intervals (95% probability that the actual value lies between LCL and UCL).
#' Non-overlapping confidence intervals suggest evidence that estimates differ.
c1inc$AsyEst %>%
  mutate_if(is.numeric, round, 1)%>% #round to fewer decimal points
  arrange(Diversity,Assemblage) %>% #to make comparison of species richness etc between groups easier 
  rename(`Conservation Target`=Assemblage, Estimated=Estimator) %>% #change table headers to be more meaningful
  flextable() %>%
  autofit() %>%
  line_spacing(space = 0.6)
#'
#'
#
#'# Trends over time 
#'Exploratory plots showing the number of species detected per survey across years, for all `r stratname`. To formally evaluate trends over time, use statistical modelling to assess temporal change while simultaneously controlling for multiple other covariates (e.g. time of day etc, see below).
#'
#' **Figure 3. Variation over time in number of species detected.**
#' 
#+ fig.width=10, fig.height=15, dpi=200
sprichnessovertime

#'# Variability in bird surveys
#' Bird surveys are 'snapshots' that naturally give highly variable results depending on bird behaviour at the time, environmental conditions, observer experience, etc. 
#' A better understanding of drivers of this variability can help improve survey design and increasing sample sizes (number of sites and surveys) can give more robust estimates. These are descriptive exploratory plots.
#' Some factors that can influence bird survey results are shown in Figure 4; temperature on the day and seasonal rainfall should also be considered.
#' 
#' **Figure 4. Some factors that may affect the number of bird species detected per survey.**
#' Descriptive plots showing variation in the number of species detected per 20-minute survey associated with a variety of factors, based on all surveys that have been conducted at `r project_name`. Each point shows the result of one survey. Boxes enclose the middle 50% of the data and the line within shows the mid-point (inter-quartile range and median respectively).
c1
c2
c3
c4
c5
c6
if(birdmin == "yes"){
  c7
}
#' 
#'# Appendix 
