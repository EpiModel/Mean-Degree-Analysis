# Comparing Sexual Network Degree among Men who have Sex with Men by Measurement Timeframe


This repository contains the analysis scripts and tables for the following study:

> Chandra C, Van Meter C, Goodreau SM, Morris M, Sanchez T, Janulis P, Jenness SM. Gaps in Sexually Transmitted Infection Comparing Sexual Network Degree among Men who have Sex with Men by Measurement Timeframe. Under Review. 

<img src="https://github.com/EpiModel/Mean-Degree-Analysis/blob/master/Figures/SF1.png">

## Abstract

### Background

Mean degree is an important measure of concurrency (having 2 or more overlapping sexual partnerships) in HIV/STI epidemiology. This study compared two common measurement approaches to estimate mean degree in a sample of men who have sex with men (MSM) in the United States.

### Methods

We used cross-sectional survey data from ARTnet, an internet-based study of MSM in the U.S. from 2017 to 2019, to compare the measurement of mean degree using a question-based method (day-of-survey method) and a dates-based method (month-offset method) that utilizes partnership start and end dates to identify overlapping relationships. We used linear regression to assess observed trends stratified by key demographic groups and partnership characteristics.

### Results

Mean degree estimated with the day-of-survey method (1.19) was most similar to mean degree at 3- and 4-month offsets (1.20–1.19) across all partnerships. Day-of-survey mean degree was most similar to month-offsets for main partnerships between 0- and 2-month offsets (0.45 vs. 0.45–0.44) and for casual partnerships at 5- and 6-month offsets (0.75–0.73). Mean degree estimates declined as month offsets increased from 0 to 12 months, and multiple linear regression suggests this decline was associated with longer partnership duration and increased number of partnerships reported in the study.  

### Conclusions

The month-offset method may be a reasonable alternative to the day-of-survey method when mean degree is measured within 6 months of the survey date using partnership start and end dates. 

## Data

We used data from ARTnet - an anonymous cross-sectional online survey of HIV-related risk behaviors, testing, and use of prevention services among MSM in the United States. MSM were recruited from the American Mens’ Internet Survey (AMIS) Survey, so the dataset also includes variables from AMIS.

Additional documentation on ARTnet and information to accessing the data can be found [here](https://github.com/EpiModel/ARTnetData). Code to install the “ARTnetData” package can be found below, but it may require a [Github Personal Access Token](https://help.github.com/en/articles/creating-a-personal-access-token-for-the-command-line) since it is a private repository.

```r
install.packages("remotes")
remotes::install_github("EpiModel/ARTnetData")
```

## Code Organization

The analysis script `MeanDegreeComparison.R` includes all R code used in this analysis. A `renv.lock` file is provided to aid in reproducibility and can be used to install necessary versions of packages used when running the R script. 
