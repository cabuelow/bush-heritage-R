---
editor_options: 
  markdown: 
    wrap: 72
---

# Bush Heritage R Workshop 2024

This repository contains code and data for the Bush Heritage R Workshop

Google planning doc link
[here](https://docs.google.com/document/d/1NH1JeWwIhgxLvKuUsRo7m14TJN-8Cb_egNLULsDWwFY/edit?usp=sharing)

# TODO

-   [x] Decide on course structure/topics
-   [x] Write draft notes
-   [ ] Finish notes - TODOs at top of qmd
-   [ ] Make a list of R packages participants will need to have
    installed prior
-   [ ] Provide links to course notes and download zip files of data

### Modelling Day1/2 packages

`install.packages('ggplot2', 'patchwork', 'DHARMa', 'MASS', 'Hmsc', 'tmap', 'sf', 'terra', 'tidyr', 'vegan', 'dplyr', 'ggcorrplot', 'fields')`

## PROPOSED COURSE OUTLINE

We will develop and lead a 1-day introductory and 2-day Advanced R
workshop for Bush Heritage Australia. The course material will build in
complexity, using datasets commonly used in Bush Heritage analyses and
reporting. Participants will need to commit to attending the full day
session. Participation in all three days is encouraged but not required.
Full course notes will be provided so researchers can revisit material
in their own time.

### Introductory course (1 day) - Data wrangling and visualisation in R

This course assumes little to no knowledge of R. It will teach
researchers the basic concepts, skills, and tools for working with
ecological data - in the hopes to get more done in less time, and with
less pain. You will briefly learn R syntax and data formats, including
the functionality of R projects, followed by data wrangling techniques
using the tidyverse R package and principles. You will also learn how to
plot data using the ggplot2 R package. The course will culminate with
developing summary statistics and a simple linear model. Further model
development and skills will be provided in the Advanced course for those
interested.

**Course Structure** Day 1 (Morning): R syntax, data formats, and data
wrangling Day 1 (Afternoon): Data wrangling continued, plotting and
visualization, summary statistics and simple models

### Advanced Course (2 days) -- Modelling in R

This course assumes familiarity with reading data into R and wrangling
it to produce basic summary statistics and plots. You will learn the
basics of fitting statistical models to data to answer ecological
questions. We will start by fitting simple models in a frequentist
framework and build towards using more advanced Bayesian hierarchical
models which are flexible enough to address a large number of modelling
challenges typical of ecological data such as: - non-independence of
samples, including spatial autocorrelation, - univariate or multivariate
response variables with non-normal error structures.

We will discuss how this modelling framework can be used to answer many
ecological questions, including: - testing for differences between
ecological indicators of interest between sites, - mapping joint
species' distributions, - identifying indicator species, and - how it is
a type of model-based ordination (as opposed to algorithmic approaches
to community analyses, e.g., nMDS).

In short, you will learn what the instructors think of as 'the one model
to rule them all' -- it can do almost everything! Finally, we will also
learn how to use R to plot and map spatial model output and make an
interactive dashboard to share with colleagues.
