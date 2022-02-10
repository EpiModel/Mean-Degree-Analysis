# Comparing Measurement Times for Sexual Network Degree among Men who have Sex with Men


This repository contains the analysis scripts and tables for the following study:

> Chandra C, Van Meter C, Goodreau SM, Morris M, Sanchez T, Janulis P, Jenness SM. Gaps in Sexually Transmitted Infection Comparing Sexual Network Degree among Men who have Sex with Men by Measurement Timeframe. Under Review. 

<img src="https://github.com/EpiModel/Mean-Degree-Analysis/blob/master/Figures/SF1.png">

## Abstract

### Background

Mean active degree is an important proxy measure of network connectivity in HIV/STI epidemiology. The performance of different degree estimands are not known for men who have sex with men (MSM) in the United States, especially within the context of fixed choice designs in behavioral surveys.

### Methods

We compared estimates of mean active degree based on reported ongoing main and casual partnerships (current method) against dates of first and last sex (retrospective method) in ARTnet, a cross-sectional survey of U.S. MSM with partnership reporting limited to the 5 most recent partners. We used linear regression to understand the impact of this data truncation on differences between the current and retrospective methods. 

### Results

Retrospective estimates declined as the offset was shifted backwards in time. Among participants with more than 5 total past-year partners compared to those with 5 or fewer partners, the average change in main degree between 12- and 0-month offsets was -0.05 (95% CI:  0.08, -0.03) after adjusting for demographics. The adjusted average change in casual degree was -0.40 (95% CI:  0.45,  0.35).

### Conclusions

The retrospective method underestimates mean degree in surveys that limit partner reporting, especially for offsets further from the survey date and for casual partnerships.

## Data

We used data from ARTnet - an anonymous cross-sectional online survey of HIV-related risk behaviors, testing, and use of prevention services among MSM in the United States. MSM were recruited from the American Mens’ Internet Survey (AMIS) Survey, so the dataset also includes variables from AMIS.

Additional documentation on ARTnet and information to accessing the data can be found [here](https://github.com/EpiModel/ARTnetData). Code to install the “ARTnetData” package can be found below, but it may require a [Github Personal Access Token](https://help.github.com/en/articles/creating-a-personal-access-token-for-the-command-line) since it is a private repository.

```r
install.packages("remotes")
remotes::install_github("EpiModel/ARTnetData")
```

## Code Organization

The analysis script `MeanDegreeComparison.R` includes all R code used in this analysis. A `renv.lock` file is provided to aid in reproducibility and can be used to install necessary versions of packages used when running the R script. 
