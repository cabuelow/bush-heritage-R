## Bush Heritage R Workshop 2024

This repository contains code and data for the Bush Heritage R Workshops: a 1-day introductory and 2-day Advanced R workshop for Bush Heritage Australia. The course material builds in complexity, using datasets commonly used in Bush Heritage analyses and reporting. Participants will need to commit to attending the full day session. Participation in all three days is encouraged but not required. Full course notes will be provided so researchers can revisit material in their own time.

## TODO

- Add this to Advanced Part 2 notes: https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13407

## Feedback from participants on inital workshop:

Summary - 6 hours/day max, could potentially split 2 day Advanced workshop into 3 days. Alternatively, skip over some of the material and refer participants to the notes.

Potential for improvement:
- A bit more on multiple predictors and interaction between them.
- Perhaps running one or two of these statistical models on a collected dataset, managing things like unequal sampling sizes, or some other common problems like many zeroes vs zero inflation. 
- As someone new to R, it would be great to have some basic intro to the broad functionality of how R can support analyses.  Just so you understand the benefits of investing in learning the program. 
- So many courses are too slow paced and it is tricky with training delivered to students with varied skill levels but I did find this course a bit too fast, particularly day 3. The 2 day advanced would have worked well as a 3 day course (I would not make the days any longer, 6 hours is enough in a day!). However we are all busy and the excellent course notes made it work out and allow us to go back into it in our own time.
- It happens in all stats courses I have been to, but Bayesian gets introduced just as everyone's brain is fried. I understand that it is logical to introduce after frequentist, but perhaps it contributes to people's feelings that Bayesian is 'too hard'. 
- Perhaps as well, it would be cool to store the simulated data in the same dataframe as the collected data. Then we could run the simulated data in the model, and then easily and quickly swap to the collected data. 

### Introductory course (1 day) - Data wrangling and visualisation in R

This course assumes little to no knowledge of R. It will teach researchers the basic concepts, skills, and tools for working with ecological data - in the hopes to get more done in less time, and with less pain. You will briefly learn R syntax and data formats, including the functionality of R projects, followed by data wrangling techniques using the tidyverse R package and principles. You will also learn how to plot data using the ggplot2 R package. The course will culminate with developing summary statistics and a simple linear model. Further model development and skills will be provided in the Advanced course for those interested.

**Course Structure** Day 1 (Morning): R syntax, data formats, and data wrangling Day 1 (Afternoon): Data wrangling continued, plotting and visualization, summary statistics and simple models

### Advanced Course (2 days) -- Modelling in R

This course assumes familiarity with reading data into R and wrangling it to produce basic summary statistics and plots. You will learn the basics of fitting statistical models to data to answer ecological questions. We will start by fitting simple models in a frequentist framework and build towards using more advanced Bayesian hierarchical models which are flexible enough to address a large number of modelling challenges typical of ecological data such as: - non-independence of samples, including spatial autocorrelation, - univariate or multivariate response variables with non-normal error structures.

We will discuss how this modelling framework can be used to answer many ecological questions, including: - testing for differences between ecological indicators of interest between sites, - mapping joint species' distributions, - identifying indicator species, and - how it is a type of model-based ordination (as opposed to algorithmic approaches to community analyses, e.g., nMDS).

In short, you will learn what the instructors think of as 'the one model to rule them all' -- it can do almost everything! Finally, we will also learn how to use R to plot and map spatial model output.
