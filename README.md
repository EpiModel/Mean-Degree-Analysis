# Comparing Measurement Times for Sexual Network Degree among Men who have Sex with Men


This repository contains the analysis scripts and tables for the following study:

> Chandra C, Van Meter C, Goodreau SM, Morris M, Sanchez T, Janulis P, Jenness SM. Gaps in Sexually Transmitted Infection Comparing Sexual Network Degree among Men who have Sex with Men by Measurement Timeframe. Under Review. 

<img src="https://github.com/EpiModel/Mean-Degree-Analysis/blob/master/Figures/SF1.png">

## Abstract

### Background

Mean degree is an important measure of network connectivity commonly used in HIV/STI epidemiology and public health practice. However, the relative advantages of different degree estimands are not known, specifically for men who have sex with men (MSM) in the United States.

### Methods

With cross-sectional data from ARTnet, a web-based study of MSM in the U.S., we compared estimates of mean degree based on ongoing partnerships (day-of-survey method) against partnership start and end dates (month-offset method). We used linear regression to assess observed trends stratified by key demographic groups and partnership characteristics.

### Results

Mean degree estimated with the day-of-survey method (1.19) was most similar to mean degree at 3- and 4-month offsets (1.20–1.19) across all partnerships ([Figure 1](https://github.com/EpiModel/Mean-Degree-Analysis/blob/master/Figures/Figure1.png)). Day-of-survey estimands were most similar to month-offset estimands for main partnerships between 0- and 2-month offsets (0.45 vs. 0.45–0.44) and for casual partnerships at 5- and 6-month offsets (0.75 vs. 0.75–0.73). Mean degree estimates declined as month offsets increased from 0 to 12 months. Multiple linear regression suggests this decline was driven by partnership duration and cumulative number of reported partnerships ([Table 3](https://github.com/EpiModel/Mean-Degree-Analysis/blob/master/Figures/Table_3.pdf)).  

### Conclusions

The month-offset method may be a suitable alternative to the day-of-survey method when partnership dates are measured within 6 months of the survey overall, but biases may result when inferring degree stratified by demographics. 

## Data

We used data from ARTnet - an anonymous cross-sectional online survey of HIV-related risk behaviors, testing, and use of prevention services among MSM in the United States. MSM were recruited from the American Mens’ Internet Survey (AMIS) Survey, so the dataset also includes variables from AMIS.

Additional documentation on ARTnet and information to accessing the data can be found [here](https://github.com/EpiModel/ARTnetData). Code to install the “ARTnetData” package can be found below, but it may require a [Github Personal Access Token](https://help.github.com/en/articles/creating-a-personal-access-token-for-the-command-line) since it is a private repository.

```r
install.packages("remotes")
remotes::install_github("EpiModel/ARTnetData")
```

## Code Organization

The analysis script `MeanDegreeComparison.R` includes all R code used in this analysis. A `renv.lock` file is provided to aid in reproducibility and can be used to install necessary versions of packages used when running the R script. 
