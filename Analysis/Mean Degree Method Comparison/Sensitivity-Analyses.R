# Sensitivity analyses for Mean Degree Comparison analysis

## Load Data and Packages---------------------------------------------------------------------
library(ARTnetData, warn.conflicts=F, quietly=T)
library(tidyverse, warn.conflicts=F, quietly=T)
library(knitr, warn.conflicts=F, quietly=T)
options(kableExtra.latex.load_packages = FALSE)
library(kableExtra, warn.conflicts=F, quietly=T)

# Load long data
ARTnet.long$ONGOING <- as.numeric(ARTnet.long$ONGOING)
sum(is.na(ARTnet.long$ONGOING))

ARTnet.long$ongoing2 <- ifelse(is.na(ARTnet.long$ONGOING), 0, #set 3017 missing values to not ongoing
                               ARTnet.long$ONGOING)

table(ARTnet.long$ptype, ARTnet.long$ONGOING, useNA = "always") # 0 missing for main; 1235 missing for casual; 1782 missing for one-time
# shouldn't matter what happens to one-time partnership types, as they are all going to be set to 0 in the next steps

table(ARTnet.long$ptype, ARTnet.long$ongoing2, useNA = "always")

# Add main partnerships data to wide dataset
ARTnet.wide <- ARTnet.long %>%
  filter(RAI == 1 | IAI == 1 | ROI == 1 | IOI == 1) %>%
  filter(ptype == 1) %>%
  group_by(AMIS_ID) %>%
  summarise(deg.main.dos = sum(ongoing2)) %>%
  right_join(ARTnet.wide, by = "AMIS_ID")

# Add casual partnerships data to wide dataset
ARTnet.wide <- ARTnet.long %>%
  filter(RAI == 1 | IAI == 1 | ROI == 1 | IOI == 1) %>%
  filter(ptype == 2) %>%
  group_by(AMIS_ID) %>%
  summarise(deg.casl.dos = sum(ongoing2)) %>%
  right_join(ARTnet.wide, by = "AMIS_ID")

# Set 2557 missing values for degree of main relationships to 0
sum(is.na(ARTnet.wide$deg.main.dos))
table(ARTnet.wide$deg.main.dos, useNA = "always")
ARTnet.wide$deg.main.dos.0 <- ifelse(is.na(ARTnet.wide$deg.main.dos),
                                   0, ARTnet.wide$deg.main.dos)

# Set 2049 missing values for degree of casual relationships to 0
sum(is.na(ARTnet.wide$deg.casl.dos))
table(ARTnet.wide$deg.casl.dos, useNA = "always")
ARTnet.wide$deg.casl.dos.0 <- ifelse(is.na(ARTnet.wide$deg.casl.dos),
                                   0, ARTnet.wide$deg.casl.dos)

# Calculated results
mean.deg.main.dos.0 <- mean(ARTnet.wide$deg.main.dos.0)
mean.deg.casl.dos.0 <- mean(ARTnet.wide$deg.casl.dos.0)

# One-way sensitivity analysis of coding missing ongoing data-----------------------------

ARTnet.long$ongoing3 <- ifelse(is.na(ARTnet.long$ONGOING), 1, #set 3017 missing values to not ongoing
                               ARTnet.long$ONGOING)

table(ARTnet.long$ptype, ARTnet.long$ongoing3, useNA = "always")

table(ARTnet.long$ongoing2, ARTnet.long$ongoing3, useNA = "always")

# Add main partnerships data to wide dataset
ARTnet.wide <- ARTnet.long %>%
  filter(RAI == 1 | IAI == 1 | ROI == 1 | IOI == 1) %>%
  filter(ptype == 1) %>%
  group_by(AMIS_ID) %>%
  summarise(deg.main.dos.1 = sum(ongoing3)) %>%
  right_join(ARTnet.wide, by = "AMIS_ID")

# Add casual partnerships data to wide dataset
ARTnet.wide <- ARTnet.long %>%
  filter(RAI == 1 | IAI == 1 | ROI == 1 | IOI == 1) %>%
  filter(ptype == 2) %>%
  group_by(AMIS_ID) %>%
  summarise(deg.casl.dos.1 = sum(ongoing3)) %>%
  right_join(ARTnet.wide, by = "AMIS_ID")

# Set 2557 missing values for degree of main relationships to 0
sum(is.na(ARTnet.wide$deg.main.dos.1))
table(ARTnet.wide$deg.main.dos.1, useNA = "always")
ARTnet.wide$deg.main.dos.1 <- ifelse(is.na(ARTnet.wide$deg.main.dos.1),
                                     0, ARTnet.wide$deg.main.dos.1)

# Set 2049 missing values for degree of casual relationships to 0
sum(is.na(ARTnet.wide$deg.casl.dos.1))
table(ARTnet.wide$deg.casl.dos.1, useNA = "always")
ARTnet.wide$deg.casl.dos.1 <- ifelse(is.na(ARTnet.wide$deg.casl.dos.1),
                                     0, ARTnet.wide$deg.casl.dos.1)

# Calculated results
mean.deg.main.dos.1 <- mean(ARTnet.wide$deg.main.dos.1)
mean.deg.casl.dos.1 <- mean(ARTnet.wide$deg.casl.dos.1)

comparison <- ARTnet.wide %>%
                  select(AMIS_ID, deg.casl.dos.0, deg.casl.dos.1) %>%
                  mutate(diff = deg.casl.dos.1 - deg.casl.dos.0) %>%
                  filter(diff > 0)

table(comparison$diff)

comparison2 <- filter(comparison, diff>2)


# Sensitivity analysis of overlapping vs. not overlapping months--------------------------

# test with just 3-month offset

prior.month.of.evaluation <- 3
ARTnet.long$ongoing.evaluation.date <- ARTnet.long$SUB_DATE -
  round(prior.month.of.evaluation*30.44)
#sensitivity analysis code would be here?
ARTnet.long$ongoing3 <- ifelse(ARTnet.long$start.date <
                                 ARTnet.long$ongoing.evaluation.date &
                                 ARTnet.long$end.date >=
                                 ARTnet.long$ongoing.evaluation.date, 1, 0)

ARTnet.wide <- ARTnet.long %>%
  filter(RAI == 1 | IAI == 1 | ROI == 1 | IOI == 1) %>%
  filter(ptype == 1) %>%
  group_by(AMIS_ID) %>%
  summarise(deg.main.three.month = sum(ongoing3)) %>%
  right_join(ARTnet.wide, by = "AMIS_ID")

ARTnet.wide <- ARTnet.long %>%
  filter(RAI == 1 | IAI == 1 | ROI == 1 | IOI == 1) %>%
  filter(ptype == 2) %>%
  group_by(AMIS_ID) %>%
  summarise(deg.casl.three.month = sum(ongoing3)) %>%
  right_join(ARTnet.wide, by = "AMIS_ID")

# Set 2560 missing mean degree for main partnerships (3mo offset) to 0
ARTnet.wide$deg.main.three.month <- ifelse(is.na(ARTnet.wide$deg.main.three.month),
                                           0, ARTnet.wide$deg.main.three.month)

# Set 2064 missing mean degree for main partnerships (3mo offset) to 0
ARTnet.wide$deg.casl.three.month <- ifelse(is.na(ARTnet.wide$deg.casl.three.month),
                                           0, ARTnet.wide$deg.casl.three.month)

# Calculated results
mean.deg.main.three.month <- mean(ARTnet.wide$deg.main.three.month)
mean.deg.casl.three.month <- mean(ARTnet.wide$deg.casl.three.month)

cat("Using the three-month offset method, the mean degree of main partnerships
    among individuals in the ART-net dataset is: ",
    round(mean.deg.main.three.month,4))

cat("Using the three-month offset method, the mean degree of casual partnerships
    among individuals in the ART-net dataset is: ",
    round(mean.deg.casl.three.month,4))


# Here are the results comparing mean degree by partnership type (main or casual)
# and method of determining ongoing partnerships (day-of-survey or three-month offset)
# in table form:

Partnership.Type <- rbind(c("Main"),c("Casual"))

mean.degree.main <- ARTnet.wide %>%
  summarize(mean.deg.main.dos = round(mean(ARTnet.wide$deg.main.dos),4),
            mean.deg.main.three.month = round(mean(ARTnet.wide$deg.main.three.month),4))
colnames(mean.degree.main) <- c("Day-of-Survey", "Three-Month")

mean.degree.casl <- ARTnet.wide %>%
  summarize(mean.deg.casl.dos = round(mean(ARTnet.wide$deg.casl.dos),4),
            mean.deg.casl.three.month = round(mean(ARTnet.wide$deg.casl.three.month),4))
colnames(mean.degree.casl) <- c("Day-of-Survey", "Three-Month")

mean.degree.df <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(mean.degree.df) <- c("Day-of-Survey", "Three-Month")

mean.degree.df <- rbind(mean.degree.df, mean.degree.main, mean.degree.casl)

mean.degree.df

mean.degree.df.final <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(mean.degree.df.final) <- c("Type","Day-of-Survey","Three.Month")

mean.degree.df.final <- cbind(Partnership.Type, mean.degree.df)

kable(mean.degree.df.final) %>%
  kable_styling(bootstrap_options = "striped", font_size = 12)



# I copy over the function for n_month_offset written by Connor to see how we
# might vary degree in overlapping months

# removes loaded data since that is included in the function
rm(list = ls())

# pull out library loading and data cleaning steps from function

#Install Libraries
  library(ARTnetData, warn.conflicts=F, quietly=T)
  library(tidyverse, warn.conflicts=F, quietly=T)


  #Create Variable 'Age.Cat' to Create Age Categories for Ages
  ARTnet.wide$age.cat <-
    ifelse(ARTnet.wide$age >= 15 & ARTnet.wide$age <= 24, '15-24',
    ifelse(ARTnet.wide$age >= 25 & ARTnet.wide$age <= 34, '25-34',
    ifelse(ARTnet.wide$age >= 35 & ARTnet.wide$age <= 44, '35-44',
    ifelse(ARTnet.wide$age >= 45 & ARTnet.wide$age <= 54, '45-54',
    ifelse(ARTnet.wide$age >= 55 & ARTnet.wide$age <= 65, '55-65',
    ifelse(ARTnet.wide$age >= 66, '66+', 'unknown'))))))

  #Create variable 'HLEDUCAT_2' for new highest level fo education categories
  # 0: High school or below
  # 1: Some college
  # 2: College and above
  # NA: Missing ()

  ARTnet.wide$HLEDUCAT_2 <- ifelse(ARTnet.wide$HLEDUCAT <= 3, 0,
                            ifelse(ARTnet.wide$HLEDUCAT == 4, 1,
                            ifelse(ARTnet.wide$HLEDUCAT == 5, 2, NA)))


n_month_offset2 <- function(start_month, end_month, filter_var='all',
                           output_type='df') {

  #Create Dummy Variable 'All' to Filter for all records
  ARTnet.wide$all <- rep(1,nrow(ARTnet.wide))

  # pre-set start_month and end_month; filter_var and output_type
  #start_month <- 0
  #end_month <- 12
  #filter_var <- 'all' #can change variables here to race.cat, age.cat, etc.
  #output_type <- 'df'

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
#sensitivity analysis code would go somewhere here?

      ongoing.eval <- paste("ongoing.evaluation.date.m",i,sep="")
      names(ongoing.eval.df)[i - start_month_offset + 1] <- ongoing.eval
      assign(ongoing.eval, ARTnet.long$SUB_DATE - round(i*30.44))
      ongoing.eval.df[,i - start_month_offset + 1] <- get(ongoing.eval)

      # original code that assigns ongoing3 status
      #ongoing3 <- paste("ongoing3.m",i,sep="")
      #names(ongoing3.df)[i - start_month_offset + 1] <- ongoing3
      #assign(ongoing3, ifelse(is.na(ifelse(ARTnet.long.adjusted$start.date <
      #                                       get(ongoing.eval) &
      #                                       ARTnet.long.adjusted$end.date >= get(ongoing.eval),
      # if start date is before month offset but not on and if end date is on or after month offset then 1, else 0
      # need to change < to <= for start.date < get(ongoing.eval)
      #                                     1, 0)), 0, ifelse(ARTnet.long.adjusted$start.date <
      #                                                         get(ongoing.eval) &
      #                                                         ARTnet.long.adjusted$end.date >= get(ongoing.eval),
      #                                                       1, 0)))
      #ongoing3.df[,i - start_month_offset + 1] <- get(ongoing3)

      # sensitivity analysis code that assigns ongoing4 status
      ongoing4 <- paste("ongoing4.m",i,sep="")
      names(ongoing4.df)[i - start_month_offset + 1] <- ongoing4
      assign(ongoing4, ifelse(is.na(ifelse(ARTnet.long.adjusted2$start.date <=
                                             get(ongoing.eval) &
                                             ARTnet.long.adjusted2$end.date >= get(ongoing.eval),
                                           1, 0)), 0, ifelse(ARTnet.long.adjusted2$start.date <=
                                                               get(ongoing.eval) &
                                                               ARTnet.long.adjusted2$end.date >= get(ongoing.eval),
                                                             1, 0)))
      ongoing4.df[,i - start_month_offset + 1] <- get(ongoing4)

      ARTnet.long.adjusted2 <- cbind(ARTnet.long.adjusted2,
                                    ongoing.eval.df[i - start_month_offset + 1],
                                    ongoing4.df[i - start_month_offset + 1])

#head(ARTnet.long.adjusted2$start.date, n=500)
#head(ARTnet.long.adjusted2$end.date, n=500)

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
        ARTnet.wide.adjusted2$deg.total.dos),
        0,
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

      ARTnet.wide.adjusted2$deg.main.dos <- ifelse(is.na(
        ARTnet.wide.adjusted2$deg.main.dos),
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

  #double-check the 95% CI calculation
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

# Examples

all2 <- n_month_offset2(0,12)
race.cat2 <- n_month_offset2(0,12,'race.cat')
age.cat2 <- n_month_offset2(0,12,'age.cat')
division2 <- n_month_offset2(0,12,'DIVCODE')
region2 <- n_month_offset2(0,12,'REGCODE')
income2 <- n_month_offset2(0,12,'HHINCOME')
income2 <- income2 %>% filter(!is.na(var_val) &
                      var_val < 77)
education2 <- n_month_offset2(0,12,'HLEDUCAT_2')
education2 <- education2 %>% filter(!is.na(var_val))

# Plot (first run function for plotting)

plot_md_comparisons(all2)
plot_md_comparisons(race.cat2, "Race/Ethnicity",
                    c("Black","Hispanic", "Other", "White"))
plot_md_comparisons(age.cat2, "Age Category",
                    c("15-24 years",
                      "25-34 years",
                      "35-44 years",
                      "45-54 years",
                      "55-65 years"))
plot_md_comparisons(region2, "Census Region",
                    c("Northeast",
                      "Midwest",
                      "South",
                      "West"))
plot_md_comparisons(division2, "Census Division",
                    c("New England",
                      "Middle Atlantic",
                      "East North Central",
                      "West North Central",
                      "South Atlantic",
                      "East South Central",
                      "West South Central",
                      "Mountain",
                      "Pacific"))
plot_md_comparisons(education2, "Highest Level of Education",
                    c("High school or below",
                      "Some college",
                      "College and above"))
plot_md_comparisons(income2, "Annual Household Income",
                    c("$0 to $19,999",
                      "$20,000 to $39,999",
                      "$40,000 to $74,999",
                      "$75,000 or more"))

# Code to quantify mean number of overlapping partnerships per participant

library(lubridate)

# For all partnerships

wider <- ARTnet.long %>%
            mutate(start.date_mo = floor_date(ARTnet.long$start.date),
                   end.date_mo = floor_date(ARTnet.long$end.date)) %>%
            filter(ptype !=3) %>%
            select(AMIS_ID, PARTNER_ID, start.date_mo, end.date_mo) %>%
            pivot_wider(
              id_cols = AMIS_ID,
              names_from = (PARTNER_ID),
              values_from = c(start.date_mo, end.date_mo),
              names_sep = "_",
              names_prefix = "PART"
            ) %>%
            mutate(part12.yes = start.date_mo_PART1 == end.date_mo_PART2,
                   part13.yes = start.date_mo_PART1 == end.date_mo_PART3,
                   part14.yes = start.date_mo_PART1 == end.date_mo_PART4,
                   part15.yes = start.date_mo_PART1 == end.date_mo_PART5,

                   part23.yes = start.date_mo_PART2 == end.date_mo_PART3,
                   part24.yes = start.date_mo_PART2 == end.date_mo_PART4,
                   part25.yes = start.date_mo_PART2 == end.date_mo_PART5,

                   part34.yes = start.date_mo_PART3 == end.date_mo_PART4,
                   part35.yes = start.date_mo_PART3 == end.date_mo_PART5,

                   part45.yes = start.date_mo_PART4 == end.date_mo_PART5)

wider$sum.yes <- wider %>% select(part12.yes:part45.yes) %>%
                 rowSums(na.rm = T)

table(wider$sum.yes)
x <- wider %>% filter(sum.yes == 3)


# For main partnerships

wider.m <- ARTnet.long %>%
            mutate(start.date_mo = floor_date(ARTnet.long$start.date),
                   end.date_mo = floor_date(ARTnet.long$end.date)) %>%
            filter(ptype == 1) %>%
            select(AMIS_ID, PARTNER_ID, start.date_mo, end.date_mo) %>%
            pivot_wider(
              id_cols = AMIS_ID,
              names_from = (PARTNER_ID),
              values_from = c(start.date_mo, end.date_mo),
              names_sep = "_",
              names_prefix = "PART"
            ) %>%
            mutate(part12.yes = start.date_mo_PART1 == end.date_mo_PART2,
                   part13.yes = start.date_mo_PART1 == end.date_mo_PART3,
                   part14.yes = start.date_mo_PART1 == end.date_mo_PART4,
                   part15.yes = start.date_mo_PART1 == end.date_mo_PART5,

                   part23.yes = start.date_mo_PART2 == end.date_mo_PART3,
                   part24.yes = start.date_mo_PART2 == end.date_mo_PART4,
                   part25.yes = start.date_mo_PART2 == end.date_mo_PART5,

                   part34.yes = start.date_mo_PART3 == end.date_mo_PART4,
                   part35.yes = start.date_mo_PART3 == end.date_mo_PART5,

                   part45.yes = start.date_mo_PART4 == end.date_mo_PART5)

wider.m$sum.yes <- wider.m %>% select(part12.yes:part45.yes) %>%
                 rowSums(na.rm = T)

table(wider.m$sum.yes)

# For casual partnerships

wider.c <- ARTnet.long %>%
            mutate(start.date_mo = floor_date(ARTnet.long$start.date),
                   end.date_mo = floor_date(ARTnet.long$end.date)) %>%
            filter(ptype == 2) %>%
            select(AMIS_ID, PARTNER_ID, start.date_mo, end.date_mo) %>%
            pivot_wider(
              id_cols = AMIS_ID,
              names_from = (PARTNER_ID),
              values_from = c(start.date_mo, end.date_mo),
              names_sep = "_",
              names_prefix = "PART"
            ) %>%
            mutate(part12.yes = start.date_mo_PART1 == end.date_mo_PART2,
                   part13.yes = start.date_mo_PART1 == end.date_mo_PART3,
                   part14.yes = start.date_mo_PART1 == end.date_mo_PART4,
                   part15.yes = start.date_mo_PART1 == end.date_mo_PART5,

                   part23.yes = start.date_mo_PART2 == end.date_mo_PART3,
                   part24.yes = start.date_mo_PART2 == end.date_mo_PART4,
                   part25.yes = start.date_mo_PART2 == end.date_mo_PART5,

                   part34.yes = start.date_mo_PART3 == end.date_mo_PART4,
                   part35.yes = start.date_mo_PART3 == end.date_mo_PART5,

                   part45.yes = start.date_mo_PART4 == end.date_mo_PART5)

wider.c$sum.yes <- wider.c %>% select(part12.yes:part45.yes) %>%
                 rowSums(na.rm = T)

table(wider.c$sum.yes)





