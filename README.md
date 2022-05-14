# Comparing Measurement Times for Sexual Network Degree among Men who have Sex with Men


This repository contains the analysis scripts and tables for the following study:

> Chandra C, Morris M, Van Meter C, Goodreau SM, Sanchez T, Janulis P, Birkett M, Jenness SM. Comparing Sexual Network Mean Active Degree Measurement Metrics among Men who have Sex with Men. Under Review. 

<img src="https://github.com/EpiModel/Mean-Degree-Analysis/blob/master/Figures/Figure1.png">

## Abstract

### Background

Mean active degree is an important proxy measure of cross-sectional network connectivity commonly used in HIV/STI epidemiology research. No current studies have compared measurement methods of mean degree using cross-sectional surveys for men who have sex with men (MSM) in the United States.

### Methods

We compared mean degree estimates based on reported ongoing main and casual sexual partnerships (current method) against dates of first and last sex (retrospective method) from 0–12 months prior to survey date in ARTnet, a cross-sectional survey of MSM in the U.S. (2017–2019). ARTnet collected data on the number of sexual partners in the past year but limited reporting on details used for calculating mean degree to the 5 most recent partners. We used linear regression to understand the impact of truncated partnership data on mean degree estimation.

### Results

Retrospective method mean degree systematically decreased as the month at which it was calculated increased from 0–12 months prior to survey date. Among participants with >5 partners in the past year compared to those with ≤5, the average change in main degree between 12 and 0 months prior to survey date was -0.05 (95% CI:  0.08, -0.03) after adjusting for race/ethnicity, age, and education. The adjusted average change in casual degree was -0.40 (95% CI:  0.45,  0.35).

### Conclusions

The retrospective method underestimates mean degree for MSM in surveys with truncated partnership data, especially for casual partnerships. The current method is less prone to bias from partner truncation when the target population experiences higher cumulative partners per year.

## Data

We used data from ARTnet - an anonymous cross-sectional online survey of HIV-related risk behaviors, testing, and use of prevention services among MSM in the United States. MSM were recruited from the American Mens’ Internet Survey (AMIS) Survey, so the dataset also includes variables from AMIS.

Additional documentation on ARTnet and information to accessing the data can be found [here](https://github.com/EpiModel/ARTnetData). Code to install the “ARTnetData” package can be found below, but it may require a [Github Personal Access Token](https://help.github.com/en/articles/creating-a-personal-access-token-for-the-command-line) since it is a private repository.

```r
install.packages("remotes")
remotes::install_github("EpiModel/ARTnetData")
```

## Code Organization

The analysis script `MeanDegreeComparison.R` includes all R code used in this analysis. A `renv.lock` file is provided to aid in reproducibility and can be used to install necessary versions of packages used when running the R script. 
