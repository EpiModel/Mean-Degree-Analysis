n_month_offset <- function(start_month, end_month, filter_var='all', output_type='df') {

  library(ARTnetData)
  library(tidyverse)

  ARTnet.wide$all <- rep(1,nrow(ARTnet.wide))
  ARTnet.wide$age.cat <- ifelse(ARTnet.wide$age >= 15 & ARTnet.wide$age <= 24,'15-24',
                                ifelse(ARTnet.wide$age >= 25 & ARTnet.wide$age <= 34, '25-34',
                                       ifelse(ARTnet.wide$age >= 35 & ARTnet.wide$age <= 44, '35-44',
                                              ifelse(ARTnet.wide$age >= 45 & ARTnet.wide$age <= 54, '45-54',
                                                     ifelse(ARTnet.wide$age >= 55 & ARTnet.wide$age <= 65, '55-65',
                                                            ifelse(ARTnet.wide$age >= 66, '66+', 'unknown'))))))

  mean.degree.main <- matrix(nrow = nrow(unique(ARTnet.wide[filter_var]))
                             , ncol = end_month - start_month + 1)
  mean.degree.casual <- matrix(nrow = nrow(unique(ARTnet.wide[filter_var]))
                               , ncol = end_month - start_month + 1)

  mean.degree.main.dos <- matrix(nrow = nrow(unique(ARTnet.wide[filter_var]))
                                 , ncol = end_month - start_month + 1)
  mean.degree.casual.dos <- matrix(nrow = nrow(unique(ARTnet.wide[filter_var]))
                                   , ncol = end_month - start_month + 1)

  n.main <- matrix(nrow = nrow(unique(ARTnet.wide[filter_var]))
                   , ncol = end_month - start_month + 1)
  n.casual <- matrix(nrow = nrow(unique(ARTnet.wide[filter_var]))
                     , ncol = end_month - start_month + 1)

  start_month_offset <- start_month
  end_month_offset <- end_month

  month <- seq(start_month_offset, end_month_offset)
  n_month <- paste(month, "M", sep="")

  unique_filter_var <- pull(unique(ARTnet.wide[filter_var]))

  for (j in 1:nrow(unique(ARTnet.wide[filter_var]))) {

    # Initalize data frames with appropriate row and column dimensions
    ongoing.eval.m <- matrix(nrow = length(ARTnet.long$SUB_DATE), ncol = end_month_offset - start_month_offset + 1)
    ongoing.eval.df <- data.frame(ongoing.eval.m)

    ongoing3.m <- matrix(nrow = length(ARTnet.long$SUB_DATE), ncol = end_month_offset - start_month_offset + 1)
    ongoing3.df <- data.frame(ongoing3.m)

    ARTnet.long$ONGOING <- as.numeric(ARTnet.long$ONGOING)
    ARTnet.long$ongoing2 <- ifelse(is.na(ARTnet.long$ONGOING), 0, ARTnet.long$ONGOING)

    ARTnet.long.adjusted <- ARTnet.long
    ARTnet.wide.adjusted <- ARTnet.wide

    for (i in start_month_offset:end_month_offset) {

      ongoing.eval <- paste("ongoing.evaluation.date.m",i,sep="")
      names(ongoing.eval.df)[i - start_month_offset + 1] <- ongoing.eval
      assign(ongoing.eval, ARTnet.long$SUB_DATE - round(i*30.44))
      ongoing.eval.df[,i - start_month_offset + 1] <- get(ongoing.eval)

      ongoing3 <- paste("ongoing3.m",i,sep="")
      names(ongoing3.df)[i - start_month_offset + 1] <- ongoing3
      assign(ongoing3, ifelse(ARTnet.long.adjusted$start.date < get(ongoing.eval) &
                                ARTnet.long.adjusted$end.date >= get(ongoing.eval),
                              1, 0))
      ongoing3.df[,i - start_month_offset + 1] <- get(ongoing3)

      ARTnet.long.adjusted <- cbind(ARTnet.long.adjusted,
                                    ongoing.eval.df[i - start_month_offset + 1],
                                    ongoing3.df[i - start_month_offset + 1])

      ARTnet.wide.adjusted <- ARTnet.long.adjusted %>%
        filter(RAI == 1 | IAI == 1 | ROI == 1 | IOI == 1) %>%
        filter(ptype == 1) %>%
        group_by(AMIS_ID) %>%
        summarise(deg.main.dos = sum(ongoing2),
                  deg.main.n.month = sum(get(paste("ongoing3.m",i,sep="")))) %>%
        right_join(ARTnet.wide.adjusted, by = "AMIS_ID")

      #Additional filtering
      ARTnet.wide.adjusted <- ARTnet.wide.adjusted %>%
        filter(UQ(as.symbol(filter_var)) == unique_filter_var[j])

      ARTnet.wide.adjusted$deg.main.dos <- ifelse(is.na(ARTnet.wide.adjusted$deg.main.dos),
                                                  0, ARTnet.wide.adjusted$deg.main.dos)
      ARTnet.wide.adjusted$deg.main.n.month <- ifelse(is.na(ARTnet.wide.adjusted$deg.main.n.month), 0,
                                                      ARTnet.wide.adjusted$deg.main.n.month)
      mean.degree.main[j, i - start_month_offset + 1] <- mean(ARTnet.wide.adjusted$deg.main.n.month)
      mean.degree.main.dos[j, i - start_month_offset + 1] <- mean(ARTnet.wide.adjusted$deg.main.dos)
      n.main[j, i - start_month_offset + 1] <- nrow(ARTnet.wide.adjusted)
      deg.main.n.month.name <- paste("deg.main.n.month.m",i,sep="")
      names(ARTnet.wide.adjusted)[names(ARTnet.wide.adjusted) == "deg.main.n.month"] <-
        deg.main.n.month.name

      ARTnet.wide.adjusted <- ARTnet.long.adjusted %>%
        filter(RAI == 1 | IAI == 1 | ROI == 1 | IOI == 1) %>%
        filter(ptype == 2) %>%
        group_by(AMIS_ID) %>%
        summarise(deg.casl.dos = sum(ongoing2),
                  deg.casl.n.month = sum(get(paste("ongoing3.m",i,sep="")))) %>%
        right_join(ARTnet.wide.adjusted, by = "AMIS_ID")

      #Additional filtering
      ARTnet.wide.adjusted <- ARTnet.wide.adjusted %>%
        filter(UQ(as.symbol(filter_var)) == unique_filter_var[j])

      ARTnet.wide.adjusted$deg.casl.dos <- ifelse(is.na(ARTnet.wide.adjusted$deg.casl.dos),
                                                  0, ARTnet.wide.adjusted$deg.casl.dos)
      ARTnet.wide.adjusted$deg.casl.n.month <- ifelse(is.na(ARTnet.wide.adjusted$deg.casl.n.month), 0,
                                                      ARTnet.wide.adjusted$deg.casl.n.month)
      mean.degree.casual[j, i - start_month_offset + 1] <- mean(ARTnet.wide.adjusted$deg.casl.n.month)
      mean.degree.casual.dos[j, i - start_month_offset + 1] <- mean(ARTnet.wide.adjusted$deg.casl.dos)
      n.casual[j, i - start_month_offset + 1] <- nrow(ARTnet.wide.adjusted)
      deg.casl.n.month.name <- paste("deg.casl.n.month.m",i,sep="")
      names(ARTnet.wide.adjusted)[names(ARTnet.wide.adjusted) == "deg.casl.n.month"] <- deg.casl.n.month.name

      ARTnet.wide.adjusted <- select(ARTnet.wide.adjusted, -c(deg.main.dos, deg.casl.dos))
    }
  }

  colnames(mean.degree.main) <- n_month
  rownames(mean.degree.main) <- unique_filter_var

  colnames(mean.degree.casual) <- n_month
  rownames(mean.degree.casual) <- unique_filter_var

  colnames(mean.degree.main.dos) <- n_month
  rownames(mean.degree.main.dos) <- unique_filter_var

  colnames(mean.degree.casual.dos) <- n_month
  rownames(mean.degree.casual.dos) <- unique_filter_var

  colnames(n.main) <- n_month
  rownames(n.main) <- unique_filter_var

  colnames(n.casual) <- n_month
  rownames(n.casual) <- unique_filter_var

  out <- list("main" = list("md" = mean.degree.main,
                            "md.dos" = mean.degree.main.dos,
                            "n" = n.main),
              "casual" = list("md" = mean.degree.casual,
                              "md.dos" = mean.degree.casual.dos,
                              "n" = n.casual))

  if(length(unique_filter_var) > 1) {

    var_val <- vector()
    md_main <- vector()
    md_main_dos <- vector()
    md_casl <- vector()
    md_casl_dos <- vector()
    num <- vector()

    for (i in 1:length(names(out$main$md[,1]))) {

      var_val <- c(var_val,rep(names(out$main$md[,1][i]),
                               end_month_offset - start_month_offset + 1))
      md_main <- c(md_main, out$main$md[i,])
      md_main_dos <- c(md_main_dos, out$main$md.dos[i,])
      md_casl <- c(md_casl, out$casual$md[i,])
      md_casl_dos <- c(md_casl_dos, out$casual$md.dos[i,])
      num <- c(num, out$main$n[i,])
    }

    var_name <- rep(filter_var,length(names(out$main$md[,1]))*length(n_month))
    month <- rep(n_month,length(names(out$main$md[,1])))

  } else {

    var_name <- rep(filter_var,length(n_month))
    var_val <- rep(filter_var,length(n_month))
    month <- n_month
    num <- out$main$n[1,]
    md_main <- out$main$md[1,]
    md_main_dos <- out$main$md.dos[1,]
    md_casl <- out$casual$md[1,]
    md_casl_dos <- out$casual$md.dos[1,]

  }

  out_df <- data.frame(var_name, var_val, month, num,
                       md_main, md_main_dos,
                       md_casl, md_casl_dos)

  ifelse(output_type == 'list', return(out), return(out_df))

}
