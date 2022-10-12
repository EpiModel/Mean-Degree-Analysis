# Comparing Measurement Times for Sexual Network Degree among Men who have Sex with Men


This repository contains the analysis scripts and tables for the following study:

> Chandra C, Morris M, Van Meter C, et al. Comparing Sexual Network Mean Active Degree Measurement Metrics among Men who have Sex with Men. Sexually Transmitted Diseases. Published online September 14, 2022:10.1097/OLQ.0000000000001708. doi:10.1097/OLQ.0000000000001708

<img src="https://github.com/EpiModel/Mean-Degree-Analysis/blob/master/Figures/Figure1.png">

## Abstract

### Background

Mean active degree is an important proxy measure of cross-sectional network connectivity commonly used in HIV/STI epidemiology research. No current studies have compared measurement methods of mean degree using a cross-sectional study design for men who have sex with men (MSM) in the United States. We compared mean degree estimates based on reported ongoing main and casual sexual partnerships (current method) against dates of first and last sex (retrospective method).

### Methods

We used data from ARTnet, a cross-sectional survey of MSM in the U.S. (2017–2019). ARTnet collected data on the number and types of sexual partners in the past year, limited to the 5 most recent partners (data truncation). We quantified partnerships for months 0–12 prior to the survey date (retrospective method), and compared that to ongoing partnerships on the day of survey (current method). We used linear regression to understand the impact of truncated partnership data on mean degree estimation.

### Results

The retrospective method yielded similar degree estimates to the current for months proximate to the day of survey. The retrospective method mean degree systematically decreased as the month increased from 0–12 months prior to survey date. This was driven by data truncation: among participants with >5 partners in the past year compared to those with ≤5, the average change in main partnership degree between 12 and 0 months prior to survey date was -0.05 (95% CI:  0.08, -0.03) after adjusting for race/ethnicity, age, and education. The adjusted average change in casual partnership degree was -0.40 (95% CI:  0.45,  0.35).

### Conclusions

The retrospective method underestimates mean degree for MSM in surveys with truncated partnership data, especially for casual partnerships. The current method is less prone to bias from partner truncation when the target population has high rate of partners per year.

## Data

We used data from ARTnet - an anonymous cross-sectional online survey of HIV-related risk behaviors, testing, and use of prevention services among MSM in the United States. MSM were recruited from the American Mens’ Internet Survey (AMIS) Survey, so the dataset also includes variables from AMIS.

Additional documentation on ARTnet and information to accessing the data can be found [here](https://github.com/EpiModel/ARTnetData). Code to install the “ARTnetData” package can be found below, but it may require a [Github Personal Access Token](https://help.github.com/en/articles/creating-a-personal-access-token-for-the-command-line) since it is a private repository.

```r
install.packages("remotes")
remotes::install_github("EpiModel/ARTnetData")
```

## Code Organization

The analysis script `MeanDegreeComparison.R` includes all R code used in this analysis. A `renv.lock` file is provided to aid in reproducibility and can be used to install necessary versions of packages used when running the R script. 
