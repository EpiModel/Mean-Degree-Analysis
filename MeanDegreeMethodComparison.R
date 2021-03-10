# ------------------------------------------------------------------------------------------#
# Mean Degree of Ongoing Partnerships-Day-of-Survey versus Three-Month Offset Method
# Purpose: To compare two methods of determining the mean degree of ongoing casual and main
# partnerships using response data from the ARTnet study.
#-------------------------------------------------------------------------------------------#

## Load Data and Packages---------------------------------------------------------------------
library(ARTnetData, warn.conflicts=F, quietly=T)
library(dplyr, warn.conflicts=F, quietly=T)
library(knitr, warn.conflicts=F, quietly=T)
options(kableExtra.latex.load_packages = FALSE)
library(kableExtra, warn.conflicts=F, quietly=T)
library(lubridate)
library(gridExtra)
library(cowplot)
library(ggplot2)


# Load long data
ARTnet.long$ONGOING <- as.numeric(ARTnet.long$ONGOING)
ARTnet.long$ongoing2 <- ifelse(is.na(ARTnet.long$ONGOING), 0, #set 3017 missing values to not ongoing
                               ARTnet.long$ONGOING)

# Randomly impute days (1-30) for start dates and end dates that are not equal to SUB_DATE
set.seed(12345)

ARTnet.long$start.date.2 <- ARTnet.long$start.date
ARTnet.long$end.date.2 <- ARTnet.long$end.date

for (i in 1:nrow(ARTnet.long)) {

  day(ARTnet.long[i,"start.date.2"]) <- sample(1:30, size=1, replace = T)

  day(ARTnet.long[i,"end.date.2"]) <- ifelse(day(ARTnet.long[i,"end.date"]) != day(ARTnet.long[i,"SUB_DATE"]),
                                             sample(1:30, size=1, replace = T), day(ARTnet.long[i,"end.date.2"]))

  }

summary(day(ARTnet.long$start.date.2))
summary(day(ARTnet.long$end.date.2))

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

## Partnership-level characteristics

# Partnership type
addmargins(table(ARTnet.long$ptype, useNA = 'always'))

# Self-reported ongoing status
temp_long <- ARTnet.long %>%
  filter(ptype %in% c(1,2)) %>%
  select(AMIS_ID, PARTNER_ID, ongoing2, ptype)

addmargins(table(temp_long$ongoing2, temp_long$ptype))

# Mean (SD) duration of partnership
mean(ARTnet.long$duration, na.rm = T)
sd(ARTnet.long$duration, na.rm = T)

# ----------------------------------------------------------------------------#
# Generalized Function for Day-of-Survey and Months-Offset Method
#-----------------------------------------------------------------------------#

# We calculate mean degree using the ARTnet long and wide datasets.
# The granularity of the ART-net long dataset is the individual partnerships whereas
# the granularity of the ART-net wide dataset is the surveyed individual.
# First, the number of ongoing partnerships per individual is assessed using the
# long dataset. Then, mean degree is calculated by summing ongoing partnerships
# by surveyed individual and dividing by total surveyed individuals using the
# wide dataset.

# Below is a helper function to determine mean degree by month by some categorical
# variable.

# To view overall mean degree using day-of-survey compared with n-month offset for
# offset months 0-12, use:
#  `n_month_offset(0, 12, filter_var='all', output_type='df')`

# To view mean degree by n-month offset by race, use:
#  `n_month_offset(0, 12, filter_var='race.cat', output_type='df')`

# Below, we evaluate mean degree by n-month offset overall (filter_var='all'),
# by race (race.cat), by age (age.cat), and by geography (city) classifications
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
      # keep in mind that start dates are based on reported Month/Year and days are imputed to 15th of the month
      # end dates are also imputed to 15th day of each month unless partnership is ongoing and end date is set to day of survey
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

# ----------------------------------------------------------------------------#
# Plot function for creating figures
#-----------------------------------------------------------------------------#

# The plot_md_comparisons function then uses the data frames generated from the 
# n_month_offset function to plot comparisons of mean degree by day of survey and 
# n-month offset for specified month offset ranges.

plot_md_comparisons <- function(md_df, cat, labels) {

    if (length(unique(md_df$var_val)) == 1) {

      total_plot_title <- paste("Total Partnerships")
      main_plot_title <- paste("Main Partnerships")
      casl_plot_title <- paste("Casual Partnerships")

       total_plot <- ggplot(md_df,
                            aes(x=factor(month_num), y=md_total)) +
                     geom_line() +
                     geom_point() +
                     geom_errorbar(
                       aes(ymin=md_total_ll, ymax=md_total_ul),
                       width=.2,
                       position=position_dodge(0.05)) +
                     labs(fill= "Category", title=total_plot_title,
                          x = "Month Offset", y = "Mean Degree") +
                     ylim(c(0.9, 1.3)) +
                     theme_minimal(base_size = 10) 

       total_plot <- total_plot +
                       geom_hline(aes(yintercept = unique(md_total_dos)),
                                  linetype = "dotted",
                                  data = md_df) 
       total_plot <- total_plot +
                       geom_hline(aes(yintercept = unique(md_total_dos_ll)),
                                  linetype = "dashed",
                                  data = md_df) 
       total_plot <- total_plot +
                       geom_hline(aes(yintercept = unique(md_total_dos_ul)),
                                  linetype = "dashed",
                                  data = md_df)
       total_plot <- total_plot + scale_x_discrete(breaks = factor(
         unique(md_df$month_num)),
                                                   labels = unique(
                                                     md_df$month))

       main_plot <- ggplot(md_df,
                            aes(x=factor(month_num), y=md_main),
                            theme(axis.text.x = month)) +
                     geom_line() +
                     geom_point()+
                     geom_errorbar(
                       aes(ymin=md_main_ll, ymax=md_main_ul),
                       width=.2,
                       position=position_dodge(0.05)) +
                    labs(title=main_plot_title,
                          x = "Month Offset", y = "Mean Degree") +
                    theme(legend.position = "none") +
                    ylim(c(0.35, 0.5)) +
                    theme_minimal(base_size = 10)

       main_plot <- main_plot +
                       geom_hline(aes(yintercept = unique(md_main_dos)),
                                  linetype = "dotted",
                                  data = md_df)
       main_plot <- main_plot +
                       geom_hline(aes(yintercept = unique(md_main_dos_ll)),
                                  linetype = "dashed",
                                  data = md_df)
       main_plot <- main_plot +
                       geom_hline(aes(yintercept = unique(md_main_dos_ul)),
                                  linetype = "dashed",
                                  data = md_df)
       main_plot <- main_plot + scale_x_discrete(breaks = factor(
         unique(md_df$month_num)),
                                                   labels = unique(md_df$month))

       casl_plot <- ggplot(md_df,
                            aes(x=factor(month_num), y=md_casl),
                            theme(axis.text.x = month)) +
                     geom_line() +
                     geom_point()+
                     geom_errorbar(
                       aes(ymin=md_casl_ll, ymax=md_casl_ul),
                       width=.2,
                       position=position_dodge(0.05)) +
                     labs(title=casl_plot_title,
                          x = "Month Offset", y = "Mean Degree") +
                     theme(legend.position = "none") +
                     ylim(c(0.6, 0.85)) +
                     theme_minimal(base_size = 10)

       casl_plot <- casl_plot +
                       geom_hline(aes(yintercept = unique(md_casl_dos)),
                                  linetype = "dotted",
                                  data = md_df)
       casl_plot <- casl_plot +
                       geom_hline(aes(yintercept = unique(md_casl_dos_ll)),
                                  linetype = "dashed",
                                  data = md_df)
       casl_plot <- casl_plot +
                       geom_hline(aes(yintercept = unique(md_casl_dos_ul)),
                                  linetype = "dashed",
                                  data = md_df)
       casl_plot <- casl_plot + scale_x_discrete(breaks = factor(
         unique(md_df$month_num)),
                                                   labels = unique(md_df$month))

       #require(gridExtra)
       suppressMessages(grid.arrange(total_plot, main_plot, casl_plot, nrow=3))

      } else {

            total_plot_title <- paste("Total Partnerships")
            main_plot_title <- paste("Main Partnerships")
            casl_plot_title <- paste("Casual Partnerships")

             total_plot <- ggplot(md_df,
                            aes(x=factor(month_num), y=md_total,
                                group=var_val, color=var_val),
                            theme(axis.text.x = month)) +
                     geom_line() +
                     geom_point()+
                     geom_errorbar(
                       aes(ymin=md_total_ll, ymax=md_total_ul),
                       width=.2,
                       position=position_dodge(0.05)) +
                     labs(title=total_plot_title,
                          x = "Month Offset", y = "Mean Degree") +
                     scale_color_discrete(name=cat,
                                          labels=labels) +
                     ylim(c(0.5, 1.6)) 

      total_plot <- total_plot +
  geom_hline(data = md_df,
             aes(yintercept = md_total_dos, col = var_val),
             linetype = "dashed") +
             theme_minimal(base_size = 10)

       main_plot <- ggplot(md_df,
                            aes(x=factor(month_num), y=md_main,
                                group=var_val, color=var_val),
                            theme(axis.text.x = month)) +
                     geom_line() +
                     geom_point() +
                     geom_errorbar(
                       aes(ymin=md_main_ll, ymax=md_main_ul),
                       width=.2,
                       position=position_dodge(0.05)) +
                     labs(title=main_plot_title,
                          x = "Month Offset", y = "Mean Degree") +
                     theme_minimal(base_size = 10) +
                     theme(legend.position = "none") +
                     ylim(c(0.2, 0.6)) 

      main_plot <- main_plot +
  geom_hline(data = md_df,
             aes(yintercept = md_main_dos, col = var_val),
             linetype = "dashed") 

       casl_plot <- ggplot(md_df,
                            aes(x=factor(month_num), y=md_casl,
                                group=var_val, color=var_val),
                            theme(axis.text.x = month)) +
                     geom_line() +
                     geom_point()+
                     geom_errorbar(
                       aes(ymin=md_casl_ll, ymax=md_casl_ul),
                       width=.2,
                       position=position_dodge(0.05)) +
                     labs(title=casl_plot_title,
                          x = "Month Offset", y = "Mean Degree") +
                     theme_minimal(base_size = 10) +
                     theme(legend.position = "none") +
                     ylim(c(0.3, 1.25)) 

       casl_plot <- casl_plot +
  geom_hline(data = md_df,
             aes(yintercept = md_casl_dos, col = var_val),
             linetype = "dashed") 

       #require(gridExtra)
       #require(cowplot)
       legend <- get_legend(total_plot)
       total_plot <- total_plot + theme(legend.position = "none")
       grid.arrange(arrangeGrob(total_plot, main_plot, casl_plot), legend, ncol=2,
                    widths=c(5,1))

      }

}

# ----------------------------------------------------------------------------#
# FIGURE 1
#-----------------------------------------------------------------------------#

png('Figure1.png', width=2048, height=1536, res=300)
plot_md_comparisons(all)
dev.off()

# ----------------------------------------------------------------------------#
# FIGURE 2
#-----------------------------------------------------------------------------#

png('Figure2.png', width=2048, height=1536, res=300)
plot_md_comparisons(race.cat, "Race/Ethnicity",
                    c("Black","Hispanic", "Other", "White"))
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
                      "55-65 years"))
dev.off()

# ----------------------------------------------------------------------------#
# SUPPLEMENTAL FIGURE 3
#-----------------------------------------------------------------------------#

png('SF3.png', width=2048, height=1536, res=300)
plot_md_comparisons(region, "Census Region",
                    c("Northeast",
                      "Midwest",
                      "South",
                      "West"))
dev.off()

# ----------------------------------------------------------------------------#
# SUPPLEMENTAL FIGURE 4
#-----------------------------------------------------------------------------#

png('SF4.png', width=2900, height=1600, res=350)
plot_md_comparisons(education, "Education Level",
                    c("High school or below",
                      "Some college",
                      "College and above"))
dev.off()

# ----------------------------------------------------------------------------#
# SUPPLEMENTAL FIGURE 5
#-----------------------------------------------------------------------------#

png('SF5.png', width=3000, height=1600, res=300)
plot_md_comparisons(income, "Annual Household Income",
                    c("$0 to $19,999",
                      "$20,000 to $39,999",
                      "$40,000 to $74,999",
                      "$75,000 or more"))
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

# Add mean duration of relationships to ARTnet.wide.adjusted
ARTnet.wide.adjusted <- ARTnet.long.adjusted %>%
  filter(RAI == 1 | IAI == 1 | ROI == 1 | IOI == 1) %>%
  filter(ptype == 1) %>%
  group_by(AMIS_ID) %>%
  summarise(main.avgduration = mean(duration)) %>%
  right_join(ARTnet.wide.adjusted, by = "AMIS_ID")

ARTnet.wide.adjusted <- ARTnet.long.adjusted %>%
  filter(RAI == 1 | IAI == 1 | ROI == 1 | IOI == 1) %>%
  filter(ptype == 2) %>%
  group_by(AMIS_ID) %>%
  summarise(casl.avgduration = mean(duration)) %>%
  right_join(ARTnet.wide.adjusted, by = "AMIS_ID")

# Scale up average main and causal duration to months and years
ARTnet.wide.adjusted <- ARTnet.wide.adjusted %>%
  mutate(main.avgduration.mo = main.avgduration/4,
         casl.avgduration.mo = casl.avgduration/4,
         main.avgduration.yr = main.avgduration/52,
         casl.avgduration.yr = casl.avgduration/52)

summary(ARTnet.wide.adjusted$main.avgduration)
summary(ARTnet.wide.adjusted$casl.avgduration)
summary(ARTnet.wide.adjusted$main.avgduration.mo)
summary(ARTnet.wide.adjusted$casl.avgduration.mo)
summary(ARTnet.wide.adjusted$main.avgduration.yr)
summary(ARTnet.wide.adjusted$casl.avgduration.yr)

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

# Check data
table(ARTnet.wide.adjusted$n.main, useNA = "always")
table(ARTnet.wide.adjusted$n.casl, useNA = "always")
table(ARTnet.wide.adjusted$n.one, useNA = "always")
table(ARTnet.wide.adjusted$n.all, useNA = "always")

# Create dichotomous variable for participants reporting 5 or fewer partners
# overall compared with participants reporting more than 5 partners in the
# past year overall

ARTnet.wide.adjusted$partners_bi <- ifelse(ARTnet.wide.adjusted$M_MP12OANUM2 > 5, 1, 0)

# Crosstab of the dichotomous 
addmargins(table(ARTnet.wide.adjusted$partners_bi, ARTnet.wide.adjusted$n.all, useNA = "always"))

# Create a new dichotomous variable for participants reporting 5 or fewer partners
# regardless of they have 5 or more total partners 
# 0 = 5 or fewer partners
# 1 = 6+ partners
ARTnet.wide.adjusted$partners_bi2 <- 
  ifelse(ARTnet.wide.adjusted$n.all < 5 & ARTnet.wide.adjusted$partners_bi == 1, 0, 
         ifelse(ARTnet.wide.adjusted$partners_bi == 1, 1, 0))

# Check variable
table(ARTnet.wide.adjusted$partners_bi2, ARTnet.wide.adjusted$partners_bi)

# Slope by new variable
ARTnet.wide.adjusted %>%
  group_by(partners_bi2) %>%
  summarize(mean.slope.m = mean(main.slope),
            mean.slope.c = mean(casl.slope))


# Create a new variable for levels of reported male partners
ARTnet.wide.adjusted$total_part <- 
  ifelse(ARTnet.wide.adjusted$n.all < 5, ARTnet.wide.adjusted$n.all, 
         ifelse(ARTnet.wide.adjusted$M_MP12OANUM2 == 5, ARTnet.wide.adjusted$n.all,
                "6+"))

# ----------------------------------------------------------------------------#
# SUPPLEMENTAL TABLE 2
#-----------------------------------------------------------------------------#

# Check variable
table(ARTnet.wide.adjusted$partners_bi, ARTnet.wide.adjusted$total_part)

#-----------------------------------------------------------------------------#

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
linear_reg(ARTnet.wide.adjusted$main.slope, ARTnet.wide.adjusted$main.avgduration.yr)
linear_reg(ARTnet.wide.adjusted$main.slope, ARTnet.wide.adjusted$partners_bi2)

linear_reg(ARTnet.wide.adjusted$casl.slope, ARTnet.wide.adjusted$race.cat)
linear_reg(ARTnet.wide.adjusted$casl.slope, ARTnet.wide.adjusted$age.cat)
linear_reg(ARTnet.wide.adjusted$casl.slope, ARTnet.wide.adjusted$age)
linear_reg(ARTnet.wide.adjusted$casl.slope, factor(ARTnet.wide.adjusted$HHINCOME_2))
linear_reg(ARTnet.wide.adjusted$casl.slope, factor(ARTnet.wide.adjusted$HLEDUCAT_2))
linear_reg(ARTnet.wide.adjusted$casl.slope, ARTnet.wide.adjusted$REGCODE)
linear_reg(ARTnet.wide.adjusted$casl.slope, ARTnet.wide.adjusted$main.avgduration.yr)
linear_reg(ARTnet.wide.adjusted$casl.slope, ARTnet.wide.adjusted$partners_bi2)

# ----------------------------------------------------------------------------#
# SUPPLEMENTAL TABLE 3
#-----------------------------------------------------------------------------#

linear_reg(ARTnet.wide.adjusted$main.slope, ARTnet.wide.adjusted$total_part)
linear_reg(ARTnet.wide.adjusted$casl.slope, ARTnet.wide.adjusted$total_part)

# ----------------------------------------------------------------------------#
# TABLE 3
#-----------------------------------------------------------------------------#

# Multiple regression model

MLR_1 <- lm(main.slope ~ race.cat + age + main.avgduration.yr + partners_bi2, data = ARTnet.wide.adjusted)
summary(MLR_1)
round(confint(MLR_1), 2)

MLR_2 <- lm(casl.slope ~ race.cat + age + casl.avgduration.yr + partners_bi2, data = ARTnet.wide.adjusted)
summary(MLR_2)
round(confint(MLR_2), 2)

# ----------------------------------------------------------------------------#
# Exploring differences in stability of N-month offset by 
# number of partnerships reported and partnership duration
#-----------------------------------------------------------------------------#

summary(ARTnet.wide.adjusted$main.slope)
summary(ARTnet.wide.adjusted$casl.slope)

table(ARTnet.wide.adjusted$main.slope, ARTnet.wide.adjusted$n.all)
table(ARTnet.wide.adjusted$casl.slope, ARTnet.wide.adjusted$n.all)

# ----------------------------------------------------------------------------#
# SUPPLEMENTAL FIGURE 6
#-----------------------------------------------------------------------------#

png('SF6.png', width=2048, height=1536, res=300)
ggplot(ARTnet.wide.adjusted, aes(x = main.avgduration.yr, y = main.slope)) +
  theme_classic() +
  geom_point(alpha = 0.1, size = 3) +
  xlab("Average Duration of Main Relationships (Years)") +
  ylab("Change in Degree of Main Relationships at 12- and 0-Month Offsets")
dev.off()

# ----------------------------------------------------------------------------#
# SUPPLEMENTAL FIGURE 7
#-----------------------------------------------------------------------------#

png('SF7.png', width=2048, height=1550, res=300)
ggplot(ARTnet.wide.adjusted, aes(x = casl.avgduration.yr, y = casl.slope)) +
  theme_classic() +
  geom_point(alpha = 0.1, size = 3) +
  xlab("Average Duration of Casual Relationships (Years)") +
  ylab("Change in Degree of Casual Relationships at 12- and 0-Month Offsets")
dev.off()


# ----------------------------------------------------------------------------#
# SUPPLEMENTAL FIGURE 8
#-----------------------------------------------------------------------------#

png('SF8.png', width=2048, height=1536, res=300)
ggplot(ARTnet.wide.adjusted, aes(x = n.all, y = main.slope)) +
  theme_classic() +
  geom_jitter(width = 0.1, height= 0.1, alpha=0.1, size = 3) +
  xlab("Total Number of Male Partners Reported") +
  ylab("Change in Degree of Main Relationships at 12- and 0-Month Offsets")
dev.off()

# ----------------------------------------------------------------------------#
# SUPPLEMENTAL FIGURE 9
#-----------------------------------------------------------------------------#

png('SF9.png', width=2048, height=1536, res=300)
ggplot(ARTnet.wide.adjusted, aes(x = total_part, y = main.slope)) +
  theme_classic() +
  geom_jitter(width = 0.1, height= 0.1, alpha=0.1, size = 3) +
  xlab("Total Number of Male Partners in the Past 12 Months") +
  ylab("Change in Degree of Main Relationships at 12- and 0-Month Offsets")
dev.off()

# ----------------------------------------------------------------------------#
# SUPPLEMENTAL FIGURE 10
#-----------------------------------------------------------------------------#

png('SF10.png', width=2048, height=1550, res=300)
ggplot(ARTnet.wide.adjusted, aes(x = n.all, y = casl.slope)) +
  theme_classic() +
  geom_jitter(width = 0.1, height= 0.1, alpha=0.1, size = 3) +
  xlab("Total Number of Male Partners Reported") +
  ylab("Change in Degree of Casual Relationships at 12- and 0-Month Offsets")
dev.off()

# ----------------------------------------------------------------------------#
# SUPPLEMENTAL FIGURE 11
#-----------------------------------------------------------------------------#

png('SF11.png', width=2048, height=1550, res=300)
ggplot(ARTnet.wide.adjusted, aes(x = total_part, y = casl.slope)) +
  theme_classic() +
  geom_jitter(width = 0.1, height= 0.1, alpha=0.1, size = 3) +
  xlab("Total Number of Male Partners in the Past 12 Months") +
  ylab("Change in Degree of Casual Relationships at 12- and 0-Month Offsets")
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
# SUPPLEMENTAL FIGURE 12
#-----------------------------------------------------------------------------#

main.outliers <- ARTnet.long %>%
  filter(AMIS_ID %in% c(2515935,3017617,27010355,2976652,32510928)) %>%
  select(AMIS_ID, ptype, SUB_DATE, start.date.2, end.date.2, ONGOING) %>%
  mutate(months.sub.start = (year(SUB_DATE) - year(start.date.2)) * 12 + month(SUB_DATE) - month(start.date.2),
         months.sub.end = (year(SUB_DATE) - year(end.date.2)) * 12 + month(SUB_DATE) - month(end.date.2))

# ----------------------------------------------------------------------------#
# SUPPLEMENTAL FIGURE 13
#-----------------------------------------------------------------------------#

casl.outliers <- ARTnet.long %>%
  filter(AMIS_ID %in% c(2568904,26610093,2853178,263264,2997173)) %>%
  select(AMIS_ID, ptype, SUB_DATE, start.date.2, end.date.2, ONGOING) %>%
  mutate(months.sub.start = (year(SUB_DATE) - year(start.date.2)) * 12 + month(SUB_DATE) - month(start.date.2),
         months.sub.end = (year(SUB_DATE) - year(end.date.2)) * 12 + month(SUB_DATE) - month(end.date.2))


# ----------------------------------------------------------------------------#
# ADDITIONAL SUPPLEMENTAL ANALYSES
#-----------------------------------------------------------------------------#

# Check proportion of participants reporting 4-5 ongoing partners
# on day of survey (included in discussion/limitations)

temp_wide <- reshape(temp_long, idvar = "AMIS_ID", timevar = "PARTNER_ID", direction = "wide")

temp_wide$ongoing2.1 <- ifelse(is.na(temp_wide$ongoing2.1), 0, temp_wide$ongoing2.1)
temp_wide$ongoing2.2 <- ifelse(is.na(temp_wide$ongoing2.2), 0, temp_wide$ongoing2.1)
temp_wide$ongoing2.3 <- ifelse(is.na(temp_wide$ongoing2.3), 0, temp_wide$ongoing2.1)
temp_wide$ongoing2.4 <- ifelse(is.na(temp_wide$ongoing2.4), 0, temp_wide$ongoing2.1)
temp_wide$ongoing2.5 <- ifelse(is.na(temp_wide$ongoing2.5), 0, temp_wide$ongoing2.1)

temp_wide$ongoing_all <- temp_wide$ongoing2.1 + temp_wide$ongoing2.2 + temp_wide$ongoing2.3 +
  temp_wide$ongoing2.4 + temp_wide$ongoing2.5

temp_wide2 <- left_join(ARTnet.wide.adjusted, temp_wide, by = "AMIS_ID")

addmargins(table(temp_wide2$ongoing_all, useNA = "always"))


