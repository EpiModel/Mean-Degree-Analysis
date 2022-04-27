# ------------------------------------------------------------------------------------------#
# Mean Degree of Ongoing Partnerships-Day-of-Survey versus Three-Month Offset Method
# Purpose: 
# 1) To compare two methods of determining the mean degree of ongoing casual and main
# partnerships using response data from the ARTnet study.
# 2) To understand the influence of fixed choice design (censoring of partnership data)
# on mean degree estimates
#-------------------------------------------------------------------------------------------#

## Load Data and Packages---------------------------------------------------------------------
library(ARTnetData, warn.conflicts=F, quietly=T)
library(tidyverse, warn.conflicts=F, quietly=T)
library(kableExtra, warn.conflicts=F, quietly=T)
library(lubridate)
library(ggpubr)
library(ggplot2)

# Load long data
ARTnet.long$ONGOING <- as.numeric(ARTnet.long$ONGOING)
ARTnet.long$ongoing2 <- ifelse(is.na(ARTnet.long$ONGOING), 0, #set 3017 missing values to not ongoing
                               ARTnet.long$ONGOING)

# Exclude observations where start.date > end.date
ARTnet.long <- ARTnet.long %>%
  filter(start.date <= end.date | is.na(start.date))
  
# Code for alternative definitions of ptype
# Alternative = all ONGOING.orig == 1 partnerships are considered active even 
# if partners only had sex with each other once

ARTnet.long$ptype.alt <- ifelse(ARTnet.long$ONGOING.orig == 1, 2, 
                                ARTnet.long$ptype)

table(ARTnet.long$ptype, ARTnet.long$ptype.alt)



# Code to randomly impute days for start dates and end dates
set.seed(12345)

ARTnet.long$start.date.2 <- ARTnet.long$start.date
ARTnet.long$end.date.2 <- ARTnet.long$end.date


for (i in 1:nrow(ARTnet.long)) {

  day(ARTnet.long[i,"end.date.2"]) <- 
    
    # randomly impute end date for those partnerships that are not "ongoing"
    # not "ongoing" partnerships MUST end before day of survey
    if(ARTnet.long[i, "end.date"] == ARTnet.long[i,"SUB_DATE"] & ARTnet.long[i, "ongoing2"] == 1){
      day(ARTnet.long[i,"end.date"])}
  
        else if (ARTnet.long[i, "end.date"] == ARTnet.long[i,"SUB_DATE"] & ARTnet.long[i, "ongoing2"] == 0) {
        sample(1:(day(ARTnet.long[i, "SUB_DATE"])-1), size=1, replace = T)}
          
          # if month and year are the same for end.date and SUB_DATE for not ongoing partnerships
          # make sure the partnership ends before the SUB_DATE
          else if (month(ARTnet.long[i, "end.date"]) == month(ARTnet.long[i,"SUB_DATE"]) &
          year(ARTnet.long[i, "end.date"]) == year(ARTnet.long[i,"SUB_DATE"]) & ARTnet.long[i, "ongoing2"] == 0 ){
          sample(1:(day(ARTnet.long[i, "SUB_DATE"])-1), size=1, replace = T)}
  
          # randomly impute all other days, but if month and year of survey date and end date are the same, 
          # then impute day between number 1 and survey date day 
          else if (month(ARTnet.long[i, "end.date"]) == month(ARTnet.long[i,"SUB_DATE"]) &
                   year(ARTnet.long[i, "end.date"]) == year(ARTnet.long[i,"SUB_DATE"])){
                   sample(1:day(ARTnet.long[i, "SUB_DATE"]), size=1, replace = T)}
                     
                     else {sample(1:30, size=1, replace = T)}
  
  
    #randomly impute start date such that start date is before the end date
    day(ARTnet.long[i,"start.date.2"]) <-
      ifelse(month(ARTnet.long[i, "end.date"]) == month(ARTnet.long[i, "start.date"]) &
         year(ARTnet.long[i, "end.date"]) == year(ARTnet.long[i,"start.date"]),
         sample(1:(day(ARTnet.long[i,"end.date.2"])-1), size=1, replace = T),
                sample(1:30, size=1, replace = T))
  
  
}

summary(day(ARTnet.long$start.date.2))
summary(day(ARTnet.long$end.date.2))

# Make start date = end date for ptype == 3 (one-time partnerhips)
ARTnet.long$start.date.2 <- ifelse(is.na(ARTnet.long$start.date.2), 
                                   ARTnet.long$end.date.2, ARTnet.long$start.date.2)

ARTnet.long$start.date.2 <- as_date(ARTnet.long$start.date.2)

# Check
temp <- ARTnet.long %>%
  filter(end.date.2 > SUB_DATE) %>%
  select(AMIS_ID, PARTNER_ID, ptype, ongoing2, SUB_DATE,start.date, end.date, start.date.2, end.date.2)

temp1 <- ARTnet.long %>%
  filter(start.date.2 > end.date.2) %>%
  select(AMIS_ID, PARTNER_ID, ptype, ongoing2, SUB_DATE,start.date, end.date, start.date.2, end.date.2)

temp2 <- ARTnet.long %>%
  filter(start.date > end.date) %>%
  select(AMIS_ID, PARTNER_ID, ptype, ongoing2, SUB_DATE,start.date, end.date, start.date.2, end.date.2)

temp3 <- ARTnet.long %>%
  filter(end.date.2 == SUB_DATE & ongoing2 == 0) %>%
  select(AMIS_ID, PARTNER_ID, ptype, ongoing2, SUB_DATE,start.date, end.date, start.date.2, end.date.2)

temp4 <- ARTnet.long %>%
  filter(start.date.2 > SUB_DATE) %>%
  select(AMIS_ID, PARTNER_ID, ptype, ongoing2, SUB_DATE,start.date, end.date, start.date.2, end.date.2)


# Identified one ID's where ongoing=0, SUB_DATE=end.date, but end.date.2 == SUB_DATE
# not sure why
# manually change dates to random date the month prior

# AMIS_ID = 2986787 and end.date.2 = "2018-11-26" and SUB_DATE = "2018-11-01"
month(ARTnet.long$end.date.2[ARTnet.long$AMIS_ID==2986787 & ARTnet.long$PARTNER_ID==1]) <- 10
day(ARTnet.long$end.date.2[ARTnet.long$AMIS_ID==2986787 & ARTnet.long$PARTNER_ID==1]) <- sample(1:30, size=1, replace=T)
ARTnet.long$end.date.2[ARTnet.long$AMIS_ID==2986787 & ARTnet.long$PARTNER_ID==1]



# ----------------------------------------------------------------------------#
# TABLE 1 
#-----------------------------------------------------------------------------#

## Individual-level characteristics

# Create Variable 'Age.Cat' to Create Age Categories for Ages
ARTnet.wide$age.cat <- ifelse(ARTnet.wide$age >= 15 & ARTnet.wide$age <= 24, '15-24',
                       ifelse(ARTnet.wide$age >= 25 & ARTnet.wide$age <= 34, '25-34',
                       ifelse(ARTnet.wide$age >= 35 & ARTnet.wide$age <= 44, '35-44',
                       ifelse(ARTnet.wide$age >= 45 & ARTnet.wide$age <= 54, '45-54',
                       ifelse(ARTnet.wide$age >= 55 & ARTnet.wide$age <= 65, '55-65',
                       ifelse(ARTnet.wide$age >= 66, '66+', 'unknown'))))))


addmargins(table(ARTnet.wide$age.cat, useNA = 'always'))

# Race
addmargins(table(ARTnet.wide$race.cat, useNA = 'always'))

# Census region
addmargins(table(ARTnet.wide$REGCODE, useNA = 'always'))

# Census division
addmargins(table(ARTnet.wide$DIVCODE, useNA = 'always'))

# Education
# Create variable 'HLEDUCAT_2' for new highest level fo education categories
# 0: High school or below
# 1: Some college
# 2: College and above
# NA: Missing ()

ARTnet.wide$HLEDUCAT_2 <- ifelse(ARTnet.wide$HLEDUCAT <= 3, 0,
                                 ifelse(ARTnet.wide$HLEDUCAT == 4, 1,
                                 ifelse(ARTnet.wide$HLEDUCAT == 5, 2, NA)))
addmargins(table(ARTnet.wide$HLEDUCAT_2, useNA = 'always'))

# Income
addmargins(table(ARTnet.wide$HHINCOME, useNA = 'always'))

ARTnet.wide$HHINCOME_2 <- ifelse(ARTnet.wide$HHINCOME >= 77, NA, ARTnet.wide$HHINCOME)

addmargins(table(ARTnet.wide$HHINCOME_2, useNA = 'always'))

### Partnership-level characteristics ###

# Partnership type
addmargins(table(ARTnet.long$ptype, useNA = 'always'))

# Self-reported ongoing status
temp_long <- ARTnet.long %>%
  filter(ptype %in% c(1,2)) %>%
  select(AMIS_ID, PARTNER_ID, ongoing2, ptype)

addmargins(table(temp_long$ongoing2, temp_long$ptype))

# Create new variables for number of main, casual, and one-time
# partnerships per participant
ARTnet.wide <-
  ARTnet.long %>%
  filter(ptype == 1) %>%
  group_by(AMIS_ID) %>%
  summarise(n.main = n()) %>%
  right_join(ARTnet.wide, by = "AMIS_ID")

ARTnet.wide <-
  ARTnet.long %>%
  filter(ptype == 2) %>%
  group_by(AMIS_ID) %>%
  summarise(n.casl = n()) %>%
  right_join(ARTnet.wide, by = "AMIS_ID")

ARTnet.wide <-
  ARTnet.long %>%
  filter(ptype == 3) %>%
  group_by(AMIS_ID) %>%
  summarise(n.one = n()) %>%
  right_join(ARTnet.wide, by = "AMIS_ID")

# Replace NA's with 0
ARTnet.wide$n.main[is.na(ARTnet.wide$n.main)] <- 0
ARTnet.wide$n.casl[is.na(ARTnet.wide$n.casl)] <- 0
ARTnet.wide$n.one[is.na(ARTnet.wide$n.one)] <- 0

# Create variable for total partners reported (out of 5 possible)
ARTnet.wide$n.all <- ARTnet.wide$n.main + ARTnet.wide$n.casl + ARTnet.wide$n.one

# Check data
table(ARTnet.wide$n.main, useNA = "always")
table(ARTnet.wide$n.casl, useNA = "always")
table(ARTnet.wide$n.one, useNA = "always")
table(ARTnet.wide$n.all, useNA = "always")

# Create dichotomous variable for participants reporting 5 or fewer partners
# overall compared with participants reporting more than 5 partners in the
# past year overall

ARTnet.wide$partners_bi <- ifelse(ARTnet.wide$M_MP12OANUM2 > 5, 1, 0)

# Crosstab of the dichotomous 
addmargins(table(ARTnet.wide$partners_bi, ARTnet.wide$n.all, useNA = "always"))

# Create a new dichotomous variable for participants reporting 5 or fewer partners
# regardless of they have 5 or more total partners 
# 0 = 5 or fewer partners
# 1 = 6+ partners
ARTnet.wide$partners_bi2 <- 
  ifelse(ARTnet.wide$n.all < 5 & ARTnet.wide$partners_bi == 1, 0, 
         ifelse(ARTnet.wide$partners_bi == 1, 1, 0))

# Check variable
table(ARTnet.wide$partners_bi2, ARTnet.wide$partners_bi, useNA = "always")

# For table 1

addmargins(table(ARTnet.wide$partners_bi2, useNA = "always"))

# ----------------------------------------------------------------------------#
# Generalized Function for Current and Retrospective Method
#-----------------------------------------------------------------------------#

# We calculate mean degree using the ARTnet long and wide datasets.
# The granularity of the ARTnet long dataset is the individual partnerships whereas
# the granularity of the ARTnet wide dataset is the surveyed individual.
# First, the number of ongoing partnerships per individual is assessed using the
# long dataset. Then, mean degree is calculated by summing ongoing partnerships
# by surveyed individual and dividing by total surveyed individuals using the
# wide dataset. This applies to both current and retrospective methods.

# Below is a helper function to determine mean degree by month by some categorical
# variable.

# To view overall mean degree using day-of-survey compared with n-month offset for
# offset months 0-12, use:
#  `n_month_offset(0, 12, filter_var='all', output_type='df')`

# To view mean degree by n-month offset by race, use:
#  `n_month_offset(0, 12, filter_var='race.cat', output_type='df')`

# Below, we evaluate mean degree by n-month offset overall (filter_var='all'),
# by total numbers reported, race, age category, income, education, and region
# and plot their results using the `plot_md_comparisons` helper function.

## N-Month Offset Method
# Note: Only single month steps are supported

n_month_offset <- function(start_month, end_month, filter_var='all',
                           output_type='df') {

  #Create Dummy Variable 'All' to Filter for all records
  ARTnet.wide$all <- rep(1,nrow(ARTnet.wide))

  #Initialize matrices for storing by-month mean degree calculations
  #these matrices will turn into the columns that we see in the tables
  #Total
  mean.degree.total <- matrix(nrow = nrow(unique(ARTnet.wide[filter_var]))
                              , ncol = end_month - start_month + 1)
  mean.degree.total.dos <- matrix(nrow = nrow(unique(ARTnet.wide[filter_var]))
                                  , ncol = end_month - start_month + 1)
  #Main
  mean.degree.main <- matrix(nrow = nrow(unique(ARTnet.wide[filter_var]))
                             , ncol = end_month - start_month + 1)
  mean.degree.main.dos <- matrix(nrow = nrow(unique(ARTnet.wide[filter_var]))
                                 , ncol = end_month - start_month + 1)
  #Casual
  mean.degree.casual <- matrix(nrow = nrow(unique(ARTnet.wide[filter_var]))
                               , ncol = end_month - start_month + 1)
  mean.degree.casual.dos <- matrix(nrow = nrow(unique(ARTnet.wide[filter_var]))
                                   , ncol = end_month - start_month + 1)
  #N
  n.main <- matrix(nrow = nrow(unique(ARTnet.wide[filter_var]))
                   , ncol = end_month - start_month + 1)
  n.casual <- matrix(nrow = nrow(unique(ARTnet.wide[filter_var]))
                     , ncol = end_month - start_month + 1)
  n.total <- matrix(nrow = nrow(unique(ARTnet.wide[filter_var]))
                    , ncol = end_month - start_month + 1)

  start_month_offset <- start_month
  end_month_offset <- end_month

  month <- seq(start_month_offset, end_month_offset)
  n_month <- paste(month, "M", sep="")

  unique_filter_var <- pull(unique(ARTnet.wide[filter_var]))

  for (j in 1:nrow(unique(ARTnet.wide[filter_var]))) {

    # Initalize data frames with appropriate row and column dimensions
    #creates a dataframe with 16198 (# of partnerships) rows and columns for each month offset
    ongoing.eval.m <- matrix(nrow = length(ARTnet.long$SUB_DATE),
                             ncol = end_month_offset - start_month_offset + 1)
    ongoing.eval.df <- data.frame(ongoing.eval.m)

    #creates a dataframe with 16198 (# of partnerships) rows and columns for each month offset
    ongoing4.m <- matrix(nrow = length(ARTnet.long$SUB_DATE),
                         ncol = end_month_offset - start_month_offset + 1)
    ongoing4.df <- data.frame(ongoing4.m)

    #sets all missing values for ongoing status to 0
    ARTnet.long$ONGOING <- as.numeric(ARTnet.long$ONGOING)
    ARTnet.long$ongoing2 <- ifelse(is.na(ARTnet.long$ONGOING), 0,
                                   ARTnet.long$ONGOING)

    #creates new objects for the adjusted ARTnet long and wide datasets
    ARTnet.long.adjusted2 <- ARTnet.long
    ARTnet.wide.adjusted2 <- ARTnet.wide

    for (i in start_month_offset:end_month_offset) {

      ongoing.eval <- paste("ongoing.evaluation.date.m",i,sep="")
      names(ongoing.eval.df)[i - start_month_offset + 1] <- ongoing.eval
      assign(ongoing.eval, ARTnet.long$SUB_DATE - round(i*30.44))
      ongoing.eval.df[,i - start_month_offset + 1] <- get(ongoing.eval)

      # this code calculates whether degree is 1 or 0 at specific N-month offset for each partnership
      # if N-month offset date falls within partnership duration then 1
      # if partnerships start OR end on N-month offset date then 1
      # keep in mind that start dates and end dates have been randomly imputed
      # end dates are also imputed unless partnership is ongoing and end date is set to day of survey
      
      ongoing4 <- paste("ongoing4.m",i,sep="")
      names(ongoing4.df)[i - start_month_offset + 1] <- ongoing4
      assign(ongoing4, ifelse(is.na(ifelse(ARTnet.long.adjusted2$start.date.2 <=
                                             get(ongoing.eval) &
                                             ARTnet.long.adjusted2$end.date.2 >= get(ongoing.eval),
                                           1, 0)), 0, ifelse(ARTnet.long.adjusted2$start.date.2 <=
                                                               get(ongoing.eval) &
                                                               ARTnet.long.adjusted2$end.date.2 >= get(ongoing.eval),
                                                             1, 0)))
      ongoing4.df[,i - start_month_offset + 1] <- get(ongoing4)

      ARTnet.long.adjusted2 <- cbind(ARTnet.long.adjusted2,
                                    ongoing.eval.df[i - start_month_offset + 1],
                                    ongoing4.df[i - start_month_offset + 1])

      # Total Partnerships
      ARTnet.wide.adjusted2 <- ARTnet.long.adjusted2 %>%
        filter(RAI == 1 | IAI == 1 | ROI == 1 | IOI == 1) %>%
        filter(ptype %in% c(1,2)) %>%
        group_by(AMIS_ID) %>%
        summarise(deg.total.dos = sum(ongoing2),
                  deg.total.n.month = sum(get(paste("ongoing4.m",i,sep="")))) %>%
        right_join(ARTnet.wide.adjusted2, by = "AMIS_ID")

      #Additional filtering
      ARTnet.wide.adjusted2 <- ARTnet.wide.adjusted2 %>%
        filter(UQ(as.symbol(filter_var)) == unique_filter_var[j])

      ARTnet.wide.adjusted2$deg.total.dos <- ifelse(is.na(
          ARTnet.wide.adjusted2$deg.total.dos), 0,
          ARTnet.wide.adjusted2$deg.total.dos)
      ARTnet.wide.adjusted2$deg.total.n.month <-
        ifelse(is.na(ARTnet.wide.adjusted2$deg.total.n.month),
               0, ARTnet.wide.adjusted2$deg.total.n.month)
      mean.degree.total[j, i - start_month_offset + 1] <-
        mean(ARTnet.wide.adjusted2$deg.total.n.month)
      mean.degree.total.dos[j, i - start_month_offset + 1] <-
        mean(ARTnet.wide.adjusted2$deg.total.dos)
      n.total[j, i - start_month_offset + 1] <- nrow(ARTnet.wide.adjusted2)
      deg.total.n.month.name <- paste("deg.total.n.month.m",i,sep="")
      names(ARTnet.wide.adjusted2)[names(ARTnet.wide.adjusted2) ==
                                    "deg.total.n.month"] <-
        deg.total.n.month.name

      # Main Partnerships
      ARTnet.wide.adjusted2 <- ARTnet.long.adjusted2 %>%
        filter(RAI == 1 | IAI == 1 | ROI == 1 | IOI == 1) %>%
        filter(ptype == 1) %>%
        group_by(AMIS_ID) %>%
        summarise(deg.main.dos = sum(ongoing2),
                  deg.main.n.month = sum(get(paste(
                    "ongoing4.m",i,sep="")))) %>%
        right_join(ARTnet.wide.adjusted2, by = "AMIS_ID")

      #Additional filtering
      ARTnet.wide.adjusted2 <- ARTnet.wide.adjusted2 %>%
        filter(UQ(as.symbol(filter_var)) == unique_filter_var[j])

      ARTnet.wide.adjusted2$deg.main.dos <- ifelse(is.na(ARTnet.wide.adjusted2$deg.main.dos),
                                                      0, ARTnet.wide.adjusted2$deg.main.dos)
      ARTnet.wide.adjusted2$deg.main.n.month <- ifelse(is.na(ARTnet.wide.adjusted2$deg.main.n.month),
                                                      0, ARTnet.wide.adjusted2$deg.main.n.month)
      mean.degree.main[j, i - start_month_offset + 1] <-
        mean(ARTnet.wide.adjusted2$deg.main.n.month)
      mean.degree.main.dos[j, i - start_month_offset + 1] <-
        mean(ARTnet.wide.adjusted2$deg.main.dos)
      n.main[j, i - start_month_offset + 1] <- nrow(ARTnet.wide.adjusted2)
      deg.main.n.month.name <- paste("deg.main.n.month.m",i,sep="")
      names(ARTnet.wide.adjusted2)[names(ARTnet.wide.adjusted2) ==
                                    "deg.main.n.month"] <-
        deg.main.n.month.name

      # Casual Partnerships
      ARTnet.wide.adjusted2 <- ARTnet.long.adjusted2 %>%
        filter(RAI == 1 | IAI == 1 | ROI == 1 | IOI == 1) %>%
        filter(ptype == 2) %>%
        group_by(AMIS_ID) %>%
        summarise(deg.casl.dos = sum(ongoing2),
                  deg.casl.n.month = sum(get(paste(
                    "ongoing4.m",i,sep="")))) %>%
        right_join(ARTnet.wide.adjusted2, by = "AMIS_ID")

      #Additional filtering
      ARTnet.wide.adjusted2 <- ARTnet.wide.adjusted2 %>%
        filter(UQ(as.symbol(filter_var)) == unique_filter_var[j])

      ARTnet.wide.adjusted2$deg.casl.dos <- ifelse(is.na(
        ARTnet.wide.adjusted2$deg.casl.dos),
        0, ARTnet.wide.adjusted2$deg.casl.dos)
      ARTnet.wide.adjusted2$deg.casl.n.month <- ifelse(is.na(ARTnet.wide.adjusted2$deg.casl.n.month),
                                                      0, ARTnet.wide.adjusted2$deg.casl.n.month)
      mean.degree.casual[j, i - start_month_offset + 1] <-
        mean(ARTnet.wide.adjusted2$deg.casl.n.month)
      mean.degree.casual.dos[j, i - start_month_offset + 1] <-
        mean(ARTnet.wide.adjusted2$deg.casl.dos)
      n.casual[j, i - start_month_offset + 1] <- nrow(ARTnet.wide.adjusted2)
      deg.casl.n.month.name <- paste("deg.casl.n.month.m",i,sep="")
      names(ARTnet.wide.adjusted2)[names(ARTnet.wide.adjusted2) ==
                                    "deg.casl.n.month"] <-
        deg.casl.n.month.name

      ARTnet.wide.adjusted2 <- select(ARTnet.wide.adjusted2, -c(deg.total.dos,
                                                              deg.main.dos,
                                                              deg.casl.dos))

    }

  }

  colnames(mean.degree.total) <- n_month
  rownames(mean.degree.total) <- unique_filter_var

  colnames(mean.degree.main) <- n_month
  rownames(mean.degree.main) <- unique_filter_var

  colnames(mean.degree.casual) <- n_month
  rownames(mean.degree.casual) <- unique_filter_var

  colnames(mean.degree.main.dos) <- n_month
  rownames(mean.degree.main.dos) <- unique_filter_var

  colnames(mean.degree.casual.dos) <- n_month
  rownames(mean.degree.casual.dos) <- unique_filter_var

  colnames(n.total) <- n_month
  rownames(n.total) <- unique_filter_var

  colnames(n.main) <- n_month
  rownames(n.main) <- unique_filter_var

  colnames(n.casual) <- n_month
  rownames(n.casual) <- unique_filter_var

  out <- list("total" = list("md" = mean.degree.total,
                             "md.dos" = mean.degree.total.dos,
                             "n" = n.total),
              "main" = list("md" = mean.degree.main,
                            "md.dos" = mean.degree.main.dos,
                            "n" = n.main),
              "casual" = list("md" = mean.degree.casual,
                              "md.dos" = mean.degree.casual.dos,
                              "n" = n.casual))

  if(length(unique_filter_var) > 1) {

    var_val <- vector()
    md_total <- vector()
    md_total_dos <- vector()
    md_main <- vector()
    md_main_dos <- vector()
    md_casl <- vector()
    md_casl_dos <- vector()
    num <- vector()

    for (i in 1:length(names(out$main$md[,1]))) {
      var_val <- c(var_val,
                   rep(names(out$main$md[,1][i]),
                       end_month_offset - start_month_offset + 1))
      md_total <- c(md_total, out$total$md[i,])
      md_total_dos <- c(md_total_dos, out$total$md.dos[i,])
      md_main <- c(md_main, out$main$md[i,])
      md_main_dos <- c(md_main_dos, out$main$md.dos[i,])
      md_casl <- c(md_casl, out$casual$md[i,])
      md_casl_dos <- c(md_casl_dos, out$casual$md.dos[i,])
      num <- c(num, out$main$n[i,])

    }

    var_name <- rep(filter_var,length(names(out$main$md[,1]))*length(n_month))
    month_num <- rep(month,length(names(out$main$md[,1])))
    month <- rep(n_month,length(names(out$main$md[,1])))

  } else {

    var_name <- rep(filter_var,length(n_month))
    var_val <- rep(filter_var,length(n_month))
    month_num <- month
    month <- n_month
    num <- out$main$n[1,]
    md_total <- out$total$md[1,]
    md_total_dos <- out$total$md.dos[1,]
    md_main <- out$main$md[1,]
    md_main_dos <- out$main$md.dos[1,]
    md_casl <- out$casual$md[1,]
    md_casl_dos <- out$casual$md.dos[1,]

  }

  md_total_ll <- md_total - 1.96 * sqrt(md_total/num)
  md_total_ul <- md_total + 1.96 * sqrt(md_total/num)
  md_total_dos_ll <- md_total_dos - 1.96 * sqrt(md_total_dos/num)
  md_total_dos_ul <- md_total_dos + 1.96 * sqrt(md_total_dos/num)
  md_main_ll <- md_main - 1.96 * sqrt(md_main/num)
  md_main_ul <- md_main + 1.96 * sqrt(md_main/num)
  md_main_dos_ll <- md_main_dos - 1.96 * sqrt(md_main_dos/num)
  md_main_dos_ul <- md_main_dos + 1.96 * sqrt(md_main_dos/num)
  md_casl_ll <- md_casl - 1.96 * sqrt(md_casl/num)
  md_casl_ul <- md_casl + 1.96 * sqrt(md_casl/num)
  md_casl_dos_ll <- md_casl_dos - 1.96 * sqrt(md_casl_dos/num)
  md_casl_dos_ul <- md_casl_dos + 1.96 * sqrt(md_casl_dos/num)


  out_df <- data.frame(var_name, var_val, month_num, month, num,
                       md_total, md_total_ll, md_total_ul,
                       md_total_dos, md_total_dos_ll, md_total_dos_ul,
                       md_main, md_main_ll, md_main_ul,
                       md_main_dos, md_main_dos_ll, md_main_dos_ul,
                       md_casl, md_casl_ll, md_casl_ul,
                       md_casl_dos, md_casl_dos_ll, md_casl_dos_ul)

  ifelse(output_type == 'list',
         return(out),
         return(out_df))

  }

# ----------------------------------------------------------------------------#
# Supplemental Table 1
#-----------------------------------------------------------------------------#

all <- n_month_offset(0,12)
race.cat <- n_month_offset(0,12,'race.cat')
age.cat <- n_month_offset(0,12,'age.cat')
region <- n_month_offset(0,12,'REGCODE')
income <- n_month_offset(0,12,'HHINCOME')
income <- income %>% filter(!is.na(var_val) &
                      var_val < 77)
education <- n_month_offset(0,12,'HLEDUCAT_2')
education <- education %>% filter(!is.na(var_val))

partners_bi2 <- n_month_offset(0,12,'partners_bi2')
partners_bi2 <- partners_bi2 %>% filter(!is.na(var_val))

# ----------------------------------------------------------------------------#
# Plot function for creating figures
#-----------------------------------------------------------------------------#

# The plot_md_comparisons function then uses the data frames generated from the 
# n_month_offset function to plot comparisons of mean degree by day of survey and 
# n-month offset for specified month offset ranges.

plot_md_comparisons <- function(md_df, cat, labels,
                                values2, colors2, shape2) {
  
  line_type <- c("LINE1" = 1, "LINE2" = 2)
  shape_type <- c("shape1"=1, "shape2" = 2)

    if (length(unique(md_df$var_val)) == 1) {

      total_plot_title <- paste("All Partnerships (Main & Casual)")
      main_plot_title <- paste("Main Partnerships")
      casl_plot_title <- paste("Casual Partnerships")

     total_plot <- ggplot(md_df,
                          aes(x=factor(month_num), y=md_total)) +
       
                     geom_pointrange(data = md_df,
                                     aes(ymin=md_total_ll, ymax=md_total_ul,
                                         x=factor(month_num), y=md_total,
                                         shape = "shape1"),
                                     fatten = 2, size = 0.5) +

                     labs(fill= "Category", title=total_plot_title,
                          x = "Months Prior to Survey Date", 
                          y = "Mean Degree") +
                     ylim(c(0.9, 1.3)) +
                     theme_minimal(base_size = 10) +
         
                    scale_shape_manual(name = "Retrospective Method",
                                       values = 16,
                                       labels = "Mean Degree & 95% CI") +

                    geom_hline(data = md_df,
                               aes(yintercept = unique(md_total_dos),
                               linetype = "LINE1")) +
       
                    geom_hline(data = md_df,
                               aes(yintercept = unique(md_total_dos_ll),
                               linetype = "LINE2")) +
       
                    geom_hline(data = md_df,
                               aes(yintercept = unique(md_total_dos_ul),
                               linetype = "LINE2")) +
       
                    scale_linetype_manual(name = "Current Method", 
                                          values = c("dotted","dashed"),
                                          labels = c("Mean Degree",
                                                     "95% CI")) 

       main_plot <- ggplot(md_df,
                            aes(x=factor(month_num), y=md_main),
                            theme(axis.text.x = month)) +
                    geom_pointrange(data = md_df,
                         aes(ymin=md_main_ll, ymax=md_main_ul,
                             x=factor(month_num), y=md_main,
                             shape = "shape1"),
                         fatten = 2, size = 0.5) +
         
                    labs(title=main_plot_title,
                          x = "Months Prior to Survey Date", 
                          y = "Mean Degree") +
                                        ylim(c(0.35, 0.5)) +
                    theme_minimal(base_size = 10) +

                    geom_hline(aes(yintercept = unique(md_main_dos),
                                   linetype = "LINE1"),
                                   data = md_df) +

                    geom_hline(aes(yintercept = unique(md_main_dos_ll),
                               linetype = "LINE2"),
                               data = md_df) +
      
                    geom_hline(aes(yintercept = unique(md_main_dos_ul),
                               linetype = "LINE2"),
                               data = md_df) +
         
                    theme(legend.position = "none") 

       casl_plot <- ggplot(md_df,
                            aes(x=factor(month_num), y=md_casl),
                            theme(axis.text.x = month)) +
         
                    geom_pointrange(data = md_df,
                         aes(ymin=md_casl_ll, ymax=md_casl_ul,
                             x=factor(month_num), y=md_casl,
                             shape = "shape1"),
                         fatten = 2, size = 0.5) +

                     labs(title=casl_plot_title,
                          x = "Months Prior to Survey Date", 
                          y = "Mean Degree") +
                     theme(legend.position = "none") +
                     ylim(c(0.6, 0.85)) +
                     theme_minimal(base_size = 10) +
       
                     geom_hline(aes(yintercept = unique(md_casl_dos),
                                linetype = "LINE1"),
                                data = md_df) +
       
                     geom_hline(aes(yintercept = unique(md_casl_dos_ll),
                                linetype = "LINE2"),
                                data = md_df) +

                     geom_hline(aes(yintercept = unique(md_casl_dos_ul),
                                linetype = "LINE2"),
                                data = md_df) +
         
                     theme(legend.position = "none") 
       
       #Arrange plots together
       ggarrange(total_plot, main_plot, casl_plot, ncol = 1, common.legend = T, legend = "right")
       

      } else {

            total_plot_title <- paste("All Partnerships (Main & Casual)")
            main_plot_title <- paste("Main Partnerships")
            casl_plot_title <- paste("Casual Partnerships")

             total_plot <- ggplot(md_df,
                                  aes(x=factor(month_num), y=md_total,
                                  group=var_val, color=var_val),
                                  theme(axis.text.x = month)) +
               
                           geom_line() +
               
                           labs(title=total_plot_title,
                                x = "Months Prior to Survey Date", y = "Mean Degree") +
               
                           geom_pointrange(data = md_df,
                               aes(ymin=md_total_ll, ymax=md_total_ul,
                               x=factor(month_num), y=md_total,
                               shape = var_val),
                               fatten = 2, size = 0.5) +
          
                           scale_shape_manual(name="Retrospective Method",
                                              labels=labels,
                                              values = shape2,
                                              guide=guide_legend(override.aes = list(color=colors2))) +
                     
                     ylim(c(0.5, 2)) +
                     
                     geom_hline(data = md_df,
                               aes(yintercept = md_total_dos,
                                   color = var_val,
                                   linetype = var_val)) +
                     
                     scale_linetype_manual(name = "Current Method",
                                           values = values2,
                                           labels = labels,
                                           guide=guide_legend(override.aes = list(color=colors2))) +
               
                     guides(color = "none") + theme_minimal(base_size = 10) 

       main_plot <- ggplot(md_df,
                            aes(x=factor(month_num), y=md_main,
                                group=var_val, color=var_val),
                            theme(axis.text.x = month)) +
         
                    geom_line() +
         
                    geom_pointrange(data = md_df,
                         aes(ymin=md_main_ll, ymax=md_main_ul,
                             x=factor(month_num), y=md_main,
                             shape = var_val),
                         fatten = 2, size = 0.5) +
         
                    scale_shape_manual(name="Retrospective Method",
                            labels=labels,
                            values = shape2,
                            guide=guide_legend(override.aes = list(color=colors2))) +
         
                     labs(title=main_plot_title,
                          x = "Months Prior to Survey Date", y = "Mean Degree") +
                     theme_minimal(base_size = 10) +
                     theme(legend.position = "none") +
                     ylim(c(0.2, 0.55)) +
         
                     geom_hline(data = md_df,
                                aes(yintercept = md_main_dos,
                                    color = var_val,
                                    linetype = var_val)) +
         
                     scale_linetype_manual(name = "Current Method",
                               values = values2,
                               labels = labels,
                               guide=guide_legend(override.aes = list(color=colors2)))


       casl_plot <- ggplot(md_df,
                            aes(x=factor(month_num), y=md_casl,
                                group=var_val, color=var_val),
                            theme(axis.text.x = month)) +
         
                    geom_line() +
                     
                    geom_pointrange(data = md_df,
                         aes(ymin=md_casl_ll, ymax=md_casl_ul,
                             x=factor(month_num), y=md_casl,
                             shape = var_val),
                         fatten = 2, size = 0.5) +
         
                    scale_shape_manual(name="Retrospective Method",
                            labels=labels,
                            values = shape2,
                            guide=guide_legend(override.aes = list(color=colors2))) +
         
         
                     labs(title=casl_plot_title,
                          x = "Months Prior to Survey Date", y = "Mean Degree") +
                     theme_minimal(base_size = 10) +
                     theme(legend.position = "none") +
                     ylim(c(0.1, 1.6)) +
                    
                     geom_hline(data = md_df,
                                aes(yintercept = md_casl_dos,
                                    color = var_val,
                                    linetype = var_val)) +
         
                     scale_linetype_manual(name = "Current Method",
                               values = values2,
                               labels = labels,
                               guide = guide_legend(override.aes = list(color=colors2)))


       #Arrange plots together
       ggarrange(total_plot, main_plot, casl_plot, ncol = 1, common.legend = T, legend = "right")

      }

}


# ----------------------------------------------------------------------------#
# FIGURE 2
#-----------------------------------------------------------------------------#

png('Figure2.png', width=2048, height=1536, res=300)
plot_md_comparisons(all)
dev.off()

# ----------------------------------------------------------------------------#
# FIGURE 3
#-----------------------------------------------------------------------------#

png('Figure3.png', width=2048, height=1536, res=300)
plot_md_comparisons(partners_bi2, "Total Partners",
                    c("\u2264 5 partners", 
                      "> 5 partners"), values2 = c(2,2),
                    colors2 = c("#F8766D", "#00BFC4"),
                    shape2 = c(16,16))
dev.off()

# ----------------------------------------------------------------------------#
# SUPPLEMENTAL FIGURE 1
#-----------------------------------------------------------------------------#

png('SF1.png', width=2048, height=1536, res=300)
plot_md_comparisons(race.cat, "Race/Ethnicity",
                    c("Black","Hispanic", "Other", "White"),
                    values2 = rep(2,4),
                    colors2 = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"),
                    shape2 = rep(16,4))
dev.off()

# ----------------------------------------------------------------------------#
# SUPPLEMENTAL FIGURE 2
#-----------------------------------------------------------------------------#

png('SF2.png', width=2048, height=1536, res=300)
plot_md_comparisons(age.cat, "Age Category",
                    c("15-24 years",
                      "25-34 years",
                      "35-44 years",
                      "45-54 years",
                      "55-65 years"),
                    values2 = rep(2,5),
                    colors2 = c("#F8766D", "#C49A00", "#7CAE00", "#00BFC4", "#C77CFF"),
                    shape2 = rep(16,5))
dev.off()

# ----------------------------------------------------------------------------#
# SUPPLEMENTAL FIGURE 3
#-----------------------------------------------------------------------------#

png('SF3.png', width=2048, height=1536, res=300)
plot_md_comparisons(region, "Census Region",
                    c("Northeast",
                      "Midwest",
                      "South",
                      "West"),
                    values2 = rep(2,4),
                    colors2 = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"),
                    shape2 = rep(16,4))
dev.off()

# ----------------------------------------------------------------------------#
# SUPPLEMENTAL FIGURE 4
#-----------------------------------------------------------------------------#

png('SF4.png', width=2900, height=1600, res=350)
plot_md_comparisons(education, "Education Level",
                    c("High school or below",
                      "Some college",
                      "College and above"),
                    values2 = rep(2,3),
                    colors2 = c("#F8766D", "#7CAE00", "#619CFF"),
                    shape2 = rep(16,3))
dev.off()

# ----------------------------------------------------------------------------#
# SUPPLEMENTAL FIGURE 5
#-----------------------------------------------------------------------------#

png('SF5.png', width=3000, height=1600, res=300)
plot_md_comparisons(income, "Annual Household Income",
                    c("$0 to $19,999",
                      "$20,000 to $39,999",
                      "$40,000 to $74,999",
                      "$75,000 or more"),
                    values2 = rep(2,4),
                    colors2 = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"),
                    shape2 = rep(16,4))
dev.off()

# ----------------------------------------------------------------------------#
# Creating New Variables for Simple and Multiple Linear Regression
#-----------------------------------------------------------------------------#

# Create object ARTnet.wide.adjusted for analysis by using code found in N-month offset function

# Specify start and end months for offsets
# Note: Only single month steps are supported
start_month_offset <- 0
end_month_offset <- 12

month <- seq(start_month_offset, end_month_offset)
n_month <- paste(month, "M", sep="")

# Initalize data frames with appropriate row and column dimensions
ongoing.eval.m <- matrix(nrow = length(ARTnet.long$SUB_DATE),
                         ncol = end_month_offset - start_month_offset + 1)
ongoing.eval.df <- data.frame(ongoing.eval.m)

ongoing3.m <- matrix(nrow = length(ARTnet.long$SUB_DATE),
                     ncol = end_month_offset - start_month_offset + 1)
ongoing3.df <- data.frame(ongoing3.m)

ARTnet.long.adjusted <- ARTnet.long
ARTnet.wide.adjusted <- ARTnet.wide

mean.degree.main <- c()
mean.degree.casual <- c()

for (i in start_month_offset:end_month_offset) {

  ongoing.eval <- paste("ongoing.evaluation.date.m",i,sep="")
  names(ongoing.eval.df)[i - start_month_offset + 1] <- ongoing.eval
  assign(ongoing.eval, ARTnet.long$SUB_DATE - round(i*30.44))
  ongoing.eval.df[,i - start_month_offset + 1] <- get(ongoing.eval)

  ongoing3 <- paste("ongoing3.m",i,sep="")
  names(ongoing3.df)[i - start_month_offset + 1] <- ongoing3
  assign(ongoing3, ifelse(is.na(ifelse(ARTnet.long.adjusted$start.date.2 <=
                                         get(ongoing.eval) &
                                         ARTnet.long.adjusted$end.date.2 >= get(ongoing.eval),
                                       1, 0)), 0, ifelse(ARTnet.long.adjusted$start.date.2 <=
                                                           get(ongoing.eval) &
                                                           ARTnet.long.adjusted$end.date.2 >= get(ongoing.eval),
                                                         1, 0)))
  ongoing3.df[,i - start_month_offset + 1] <- get(ongoing3)

  ARTnet.long.adjusted <- cbind(ARTnet.long.adjusted,
                                ongoing.eval.df[i - start_month_offset + 1],
                                ongoing3.df[i - start_month_offset + 1])

  ARTnet.wide.adjusted <- ARTnet.long.adjusted %>%
    filter(RAI == 1 | IAI == 1 | ROI == 1 | IOI == 1) %>%
    filter(ptype == 1) %>%
    group_by(AMIS_ID) %>%
    summarise(deg.main.n.month = sum(get(paste("ongoing3.m",i,sep="")))) %>%
    right_join(ARTnet.wide.adjusted, by = "AMIS_ID")

  # #Additional filtering
  # ARTnet.wide.adjusted <- ARTnet.wide.adjusted %>%
  #   filter(age >= 16 & age <= 29) %>%
  #   filter(city2 == "Chicago")

  ARTnet.wide.adjusted$deg.main.n.month <- ifelse(is.na(
    ARTnet.wide.adjusted$deg.main.n.month), 0,
    ARTnet.wide.adjusted$deg.main.n.month)
  mean.degree.main[i - start_month_offset + 1] <- mean(
    ARTnet.wide.adjusted$deg.main.n.month)
  deg.main.n.month.name <- paste("deg.main.n.month.m",i,sep="")
  names(ARTnet.wide.adjusted)[names(ARTnet.wide.adjusted) ==
                                "deg.main.n.month"] <- deg.main.n.month.name

  ARTnet.wide.adjusted <- ARTnet.long.adjusted %>%
    filter(RAI == 1 | IAI == 1 | ROI == 1 | IOI == 1) %>%
    filter(ptype == 2) %>%
    group_by(AMIS_ID) %>%
    summarise(deg.casl.n.month = sum(get(paste("ongoing3.m",i,sep="")))) %>%
    right_join(ARTnet.wide.adjusted, by = "AMIS_ID")

  ARTnet.wide.adjusted$deg.casl.n.month <- ifelse(is.na(
    ARTnet.wide.adjusted$deg.casl.n.month), 0,
    ARTnet.wide.adjusted$deg.casl.n.month)
  mean.degree.casual[i - start_month_offset + 1] <- mean(
    ARTnet.wide.adjusted$deg.casl.n.month)
  deg.casl.n.month.name <- paste("deg.casl.n.month.m",i,sep="")
  names(ARTnet.wide.adjusted)[names(ARTnet.wide.adjusted) ==
                                "deg.casl.n.month"] <- deg.casl.n.month.name

}

## Cross-tab of ongoing vs ongoing at month 0

addmargins(table(ARTnet.long.adjusted$ongoing3.m0, ARTnet.long.adjusted$ONGOING, useNA = "always"))

# Create dependent variable: slope (difference in degree for main and casual relationships between 12 and 0 months)

ARTnet.wide.adjusted <- ARTnet.wide.adjusted %>%
                            mutate(main.slope = deg.main.n.month.m12 - deg.main.n.month.m0,
                                   casl.slope = deg.casl.n.month.m12 - deg.casl.n.month.m0,
                                   main.mean = (deg.main.n.month.m0 + deg.main.n.month.m1 + deg.main.n.month.m2 +
                                     deg.main.n.month.m3 + deg.main.n.month.m4 + deg.main.n.month.m5 +
                                     deg.main.n.month.m6 + deg.main.n.month.m7 + deg.main.n.month.m8 +
                                     deg.main.n.month.m9 + deg.main.n.month.m10 + deg.main.n.month.m11 + deg.main.n.month.m12)/13,
                                   casl.mean = (deg.casl.n.month.m0 + deg.casl.n.month.m1 + deg.casl.n.month.m2 +
                                     deg.casl.n.month.m3 + deg.casl.n.month.m4 + deg.casl.n.month.m5 +
                                     deg.casl.n.month.m6 + deg.casl.n.month.m7 + deg.casl.n.month.m8 +
                                     deg.casl.n.month.m9 + deg.casl.n.month.m10 + deg.casl.n.month.m11 + deg.casl.n.month.m12)/13,
                                   total.mean = (deg.main.n.month.m0 + deg.main.n.month.m1 + deg.main.n.month.m2 +
                                                 deg.main.n.month.m3 + deg.main.n.month.m4 + deg.main.n.month.m5 +
                                                 deg.main.n.month.m6 + deg.main.n.month.m7 + deg.main.n.month.m8 +
                                                 deg.main.n.month.m9 + deg.main.n.month.m10 + deg.main.n.month.m11 + 
                                                 deg.main.n.month.m12 + deg.casl.n.month.m0 + deg.casl.n.month.m1 + 
                                                 deg.casl.n.month.m2 + deg.casl.n.month.m3 + deg.casl.n.month.m4 + 
                                                 deg.casl.n.month.m5 + deg.casl.n.month.m6 + deg.casl.n.month.m7 + 
                                                 deg.casl.n.month.m8 + deg.casl.n.month.m9 + deg.casl.n.month.m10 + 
                                                 deg.casl.n.month.m11 + deg.casl.n.month.m12)/13)


# Create variables for number of main, casual, and one-time partners reported

ARTnet.wide.adjusted <-
  ARTnet.long.adjusted %>%
  filter(ptype == 1) %>%
  group_by(AMIS_ID) %>%
  summarise(n.main = n()) %>%
  right_join(ARTnet.wide.adjusted, by = "AMIS_ID")

ARTnet.wide.adjusted <-
  ARTnet.long.adjusted %>%
  filter(ptype == 2) %>%
  group_by(AMIS_ID) %>%
  summarise(n.casl = n()) %>%
  right_join(ARTnet.wide.adjusted, by = "AMIS_ID")

ARTnet.wide.adjusted <-
  ARTnet.long.adjusted %>%
  filter(ptype == 3) %>%
  group_by(AMIS_ID) %>%
  summarise(n.one = n()) %>%
  right_join(ARTnet.wide.adjusted, by = "AMIS_ID")

# Replace NA's with 0
ARTnet.wide.adjusted$n.main[is.na(ARTnet.wide.adjusted$n.main)] <- 0
ARTnet.wide.adjusted$n.casl[is.na(ARTnet.wide.adjusted$n.casl)] <- 0
ARTnet.wide.adjusted$n.one[is.na(ARTnet.wide.adjusted$n.one)] <- 0

# Create variable for total partners reported
ARTnet.wide.adjusted$n.all <- ARTnet.wide.adjusted$n.main + ARTnet.wide.adjusted$n.casl + ARTnet.wide.adjusted$n.one

# Create variable for total main + casual partners reported
ARTnet.wide.adjusted$n.mc <- ARTnet.wide.adjusted$n.main + ARTnet.wide.adjusted$n.casl

# Check data
table(ARTnet.wide.adjusted$n.main, useNA = "always")
table(ARTnet.wide.adjusted$n.casl, useNA = "always")
table(ARTnet.wide.adjusted$n.one, useNA = "always")
table(ARTnet.wide.adjusted$n.all, useNA = "always")
table(ARTnet.wide.adjusted$n.mc, useNA = "always")


# ----------------------------------------------------------------------------#
# SUPPLEMENTAL TABLE 3
#-----------------------------------------------------------------------------#

# Crosstab of the dichotomous partners variable and total partners reported for all, main, and casual

addmargins(table(ARTnet.wide.adjusted$partners_bi, ARTnet.wide.adjusted$n.all, useNA = "always"))

#-----------------------------------------------------------------------------#

# Calculcate the prevalence of truncation for each count

# Slope by binary partners variable
ARTnet.wide.adjusted %>%
  group_by(partners_bi2) %>%
  summarize(mean.slope.m = mean(main.slope),
            mean.slope.c = mean(casl.slope))


# Create a new variable for levels of reported male partners
ARTnet.wide.adjusted$total_part <- 
  ifelse(ARTnet.wide.adjusted$n.all < 5, ARTnet.wide.adjusted$n.all, 
         ifelse(ARTnet.wide.adjusted$M_MP12OANUM2 == 5, ARTnet.wide.adjusted$n.all,
                "6+"))


# Check variable
table(ARTnet.wide.adjusted$partners_bi, ARTnet.wide.adjusted$total_part)

# Slope by new variable
ARTnet.wide.adjusted %>%
  group_by(total_part) %>%
  summarize(mean.slope.m = mean(main.slope),
            mean.slope.c = mean(casl.slope))


# Add main.mean, casl.mean, and total.mean to ### Supplemental Table 1 ###

mean(ARTnet.wide.adjusted$main.mean)
mean(ARTnet.wide.adjusted$casl.mean)

ARTnet.wide.adjusted %>%
  group_by(race.cat) %>%
  summarize(mean.main = mean(main.mean),
          mean.casl = mean(casl.mean))

ARTnet.wide.adjusted %>%
  group_by(age.cat) %>%
  summarize(mean.main = mean(main.mean),
            mean.casl = mean(casl.mean))

ARTnet.wide.adjusted %>%
  group_by(REGCODE) %>%
  summarize(mean.main = mean(main.mean),
            mean.casl = mean(casl.mean))

ARTnet.wide.adjusted %>%
  group_by(HLEDUCAT_2) %>%
  summarize(mean.main = mean(main.mean),
            mean.casl = mean(casl.mean))

ARTnet.wide.adjusted %>%
  group_by(HHINCOME_2) %>%
  summarize(mean.main = mean(main.mean),
            mean.casl = mean(casl.mean))

# ----------------------------------------------------------------------------#
# TABLE 2
#-----------------------------------------------------------------------------#

# Function for linear regression models

linear_reg <- function(outcome, exposure) {
  linear <- lm(outcome ~ exposure, data = ARTnet.wide.adjusted)
  s <- summary(linear)
  c <- round(coef(linear), 2)
  r <- round(confint(linear), 2)
  return(list(s,c,r))
}

linear_reg(ARTnet.wide.adjusted$main.slope, ARTnet.wide.adjusted$race.cat)
linear_reg(ARTnet.wide.adjusted$main.slope, ARTnet.wide.adjusted$age.cat)
linear_reg(ARTnet.wide.adjusted$main.slope, ARTnet.wide.adjusted$age)
linear_reg(ARTnet.wide.adjusted$main.slope, factor(ARTnet.wide.adjusted$HHINCOME_2))
linear_reg(ARTnet.wide.adjusted$main.slope, factor(ARTnet.wide.adjusted$HLEDUCAT_2))
linear_reg(ARTnet.wide.adjusted$main.slope, ARTnet.wide.adjusted$REGCODE)
linear_reg(ARTnet.wide.adjusted$main.slope, ARTnet.wide.adjusted$partners_bi2)

linear_reg(ARTnet.wide.adjusted$casl.slope, ARTnet.wide.adjusted$race.cat)
linear_reg(ARTnet.wide.adjusted$casl.slope, ARTnet.wide.adjusted$age.cat)
linear_reg(ARTnet.wide.adjusted$casl.slope, ARTnet.wide.adjusted$age)
linear_reg(ARTnet.wide.adjusted$casl.slope, factor(ARTnet.wide.adjusted$HHINCOME_2))
linear_reg(ARTnet.wide.adjusted$casl.slope, factor(ARTnet.wide.adjusted$HLEDUCAT_2))
linear_reg(ARTnet.wide.adjusted$casl.slope, ARTnet.wide.adjusted$REGCODE)
linear_reg(ARTnet.wide.adjusted$casl.slope, ARTnet.wide.adjusted$partners_bi2)

# ----------------------------------------------------------------------------#
# SUPPLEMENTAL TABLE 4
#-----------------------------------------------------------------------------#

linear_reg(ARTnet.wide.adjusted$main.slope, ARTnet.wide.adjusted$total_part)
linear_reg(ARTnet.wide.adjusted$casl.slope, ARTnet.wide.adjusted$total_part)

# ----------------------------------------------------------------------------#
# RESULTS
#-----------------------------------------------------------------------------#

# Multiple regression model

MLR_1 <- lm(main.slope ~ partners_bi2 + race.cat + age + 
              factor(HLEDUCAT_2), data = ARTnet.wide.adjusted)
summary(MLR_1)
round(confint(MLR_1), 2)

MLR_2 <- lm(casl.slope ~ partners_bi2 + race.cat + age + 
              factor(HLEDUCAT_2), data = ARTnet.wide.adjusted)
summary(MLR_2)
round(confint(MLR_2), 2)

# ----------------------------------------------------------------------------#
# SUPPLEMENTAL FIGURE 6
#-----------------------------------------------------------------------------#

main.outliers <- ARTnet.long %>%
  filter(AMIS_ID %in% c(2515935,3017617,27010355,2976652,32510928)) %>%
  select(AMIS_ID, ptype, SUB_DATE, start.date.2, end.date.2, ONGOING) %>%
  mutate(months.sub.start = (year(SUB_DATE) - year(start.date.2)) * 12 + month(SUB_DATE) - month(start.date.2),
         months.sub.end = (year(SUB_DATE) - year(end.date.2)) * 12 + month(SUB_DATE) - month(end.date.2))

# ----------------------------------------------------------------------------#
# SUPPLEMENTAL FIGURE 7
#-----------------------------------------------------------------------------#

casl.outliers <- ARTnet.long %>%
  filter(AMIS_ID %in% c(2568904,26610093,2853178,263264,2997173)) %>%
  select(AMIS_ID, ptype, SUB_DATE, start.date.2, end.date.2, ONGOING) %>%
  mutate(months.sub.start = (year(SUB_DATE) - year(start.date.2)) * 12 + month(SUB_DATE) - month(start.date.2),
         months.sub.end = (year(SUB_DATE) - year(end.date.2)) * 12 + month(SUB_DATE) - month(end.date.2))

# ----------------------------------------------------------------------------#
# SUPPLEMENTAL FIGURE 8
#-----------------------------------------------------------------------------#

ARTnet.wide.adjusted2 <- ARTnet.long.adjusted %>%
  filter(RAI == 1 | IAI == 1 | ROI == 1 | IOI == 1) %>%
  filter(ptype %in% c(1,2)) %>%
  group_by(AMIS_ID) %>%
  summarise(deg.total.dos = sum(ongoing2),
            deg.total.6.month = sum(ongoing3.m6)) 

x <- as.data.frame(table(ARTnet.wide.adjusted2$deg.total.dos))
colnames(x) <- c("Degree", "Count")            

y <- as.data.frame(table(ARTnet.wide.adjusted2$deg.total.6.month))
colnames(y) <- c("Degree", "Count")

z <- rbind(x, y)
z$method <- rep(c("Current", "Retrospective - 6 months"), each = 6)
z <- z %>%
  rowwise() %>%
  group_by(method) %>%
  mutate(perc = round(Count/sum(Count)*100,0))

png('SF8.png', width=2048, height=1550, res=300)
ggplot(z, aes(fill=method, y=perc, x=Degree)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = perc,
                perc = perc + 0.05),
            position = position_dodge(0.9),
            vjust = -0.5) +
  scale_fill_brewer(palette="Paired") +
  labs(fill="Method") +
  xlab("Degree") +
  ylab("Percent") +
  theme_minimal() 
dev.off()

# ----------------------------------------------------------------------------#
# Examining the Partnership Duration and Number of Partners for Outliers
# with Large Slope Absolute Values
#-----------------------------------------------------------------------------#

# Identify main slope outliers
ARTnet.wide.adjusted %>%
  filter(!(main.slope %in% c(-1:1))) %>%
  select(AMIS_ID, main.slope)

# Identify casual slope outliers
ARTnet.wide.adjusted %>%
  filter(!(casl.slope %in% c(-3:3))) %>%
  select(AMIS_ID, casl.slope) %>%
  print(n=22)

# ----------------------------------------------------------------------------#
# SUPPLEMENTAL TABLE 6
#-----------------------------------------------------------------------------#

# Proportion of ARTnet participants with more than 1 ongoing partnership
# by current method and retrospective method at 6 months

ARTnet.wide.adjusted2 <- ARTnet.wide.adjusted2 %>%
  mutate(deg.total.6.month_bi = ifelse(deg.total.6.month > 1, 1, 0),
         deg.total.dos_bi = ifelse(deg.total.dos > 1, 1, 0)) 

p_hat_6mo <- sum(ARTnet.wide.adjusted2$deg.total.6.month_bi)/4904

p_hat_dos <- sum(ARTnet.wide.adjusted2$deg.total.dos_bi)/4904

# Find 95% CI

alpha <- 0.05
z <- qnorm(1-alpha/2)

p_hat_6mo + c(-1,1)*z*sqrt(p_hat_6mo*(1-p_hat_6mo)/4904)
p_hat_dos + c(-1,1)*z*sqrt(p_hat_dos*(1-p_hat_dos)/4904)

ARTnet.wide.adjusted %>%
  group_by(partners_bi2) %>%
  summarize(main.duration = mean(main.avgduration.yr, na.rm = T),
            casl.duration = mean(casl.avgduration.yr, na.rm = T))

summary(ARTnet.wide.adjusted$main.avgduration.yr)

# ----------------------------------------------------------------------------#
# SUPPLEMENTAL TABLE 2
#-----------------------------------------------------------------------------#

addmargins(table(ARTnet.wide.adjusted$race.cat, ARTnet.wide.adjusted$partners_bi2))
addmargins(prop.table(table(ARTnet.wide.adjusted$race.cat, ARTnet.wide.adjusted$partners_bi2),2))

addmargins(table(ARTnet.wide.adjusted$age.cat, ARTnet.wide.adjusted$partners_bi2))
addmargins(prop.table(table(ARTnet.wide.adjusted$age.cat, ARTnet.wide.adjusted$partners_bi2),2))

addmargins(table(ARTnet.wide.adjusted$HLEDUCAT_2, ARTnet.wide.adjusted$partners_bi2))
addmargins(prop.table(table(ARTnet.wide.adjusted$HLEDUCAT_2, ARTnet.wide.adjusted$partners_bi2),2))
