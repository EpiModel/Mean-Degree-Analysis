n_month_offset_ptype <- function(start_month, end_month, filter_var='all',
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
    ongoing.eval.m <- matrix(nrow = length(l$SUB_DATE),
                             ncol = end_month_offset - start_month_offset + 1)
    ongoing.eval.df <- data.frame(ongoing.eval.m)
    
    #creates a dataframe with 16198 (# of partnerships) rows and columns for each month offset
    ongoing4.m <- matrix(nrow = length(l$SUB_DATE),
                         ncol = end_month_offset - start_month_offset + 1)
    ongoing4.df <- data.frame(ongoing4.m)
    
    #sets all missing values for ongoing status to 0
    l$ONGOING <- as.numeric(l$ONGOING)
    l$ongoing2 <- ifelse(is.na(l$ONGOING), 0,
                                   l$ONGOING)
    
    set.seed(100) # reproducibility
    l <- ARTnet.long %>%  # ptype 3 must be inactive
      mutate(active = ifelse(is.na(ONGOING), 2, ONGOING),
             ptype.alt = case_when(
               ptype==3 & active==1 ~ 2,
               TRUE ~ ptype),
      ) %>% # impute DK responses for active based on ptype
      rowwise() %>%
      mutate(active.imp = case_when(
        active==2 & ptype.alt==2 ~ as.numeric(rbinom(1,1,0.5)),
        active==2 & ptype.alt==3 ~ as.numeric(rbinom(1,1,0.1)),
        TRUE ~ active)
      ) %>%
      ungroup() %>% # reclassify ptype3 based on imputation
      mutate(ptype.imp = case_when(
        ptype.alt==3 & active.imp==1 ~ 2,
        TRUE ~ ptype.alt)
      )
    
    #creates new objects for the adjusted ARTnet long and wide datasets
    l.adjusted2 <- l
    ARTnet.wide.adjusted2 <- ARTnet.wide
    
    for (i in start_month_offset:end_month_offset) {
      
      ongoing.eval <- paste("ongoing.evaluation.date.m",i,sep="")
      names(ongoing.eval.df)[i - start_month_offset + 1] <- ongoing.eval
      assign(ongoing.eval, l$SUB_DATE - round(i*30.44))
      ongoing.eval.df[,i - start_month_offset + 1] <- get(ongoing.eval)
      
      # this code calculates whether degree is 1 or 0 at specific N-month offset for each partnership
      # if N-month offset date falls within partnership duration then 1
      # if partnerships start OR end on N-month offset date then 1
      # keep in mind that start dates and end dates have been randomly imputed
      # end dates are also imputed unless partnership is ongoing and end date is set to day of survey
      
      ongoing4 <- paste("ongoing4.m",i,sep="")
      names(ongoing4.df)[i - start_month_offset + 1] <- ongoing4
      assign(ongoing4, ifelse(is.na(ifelse(l.adjusted2$start.date.2 <=
                                             get(ongoing.eval) &
                                             l.adjusted2$end.date.2 >= get(ongoing.eval),
                                           1, 0)), 0, ifelse(l.adjusted2$start.date.2 <=
                                                               get(ongoing.eval) &
                                                               l.adjusted2$end.date.2 >= get(ongoing.eval),
                                                             1, 0)))
      ongoing4.df[,i - start_month_offset + 1] <- get(ongoing4)
      
      l.adjusted2 <- cbind(l.adjusted2,
                                     ongoing.eval.df[i - start_month_offset + 1],
                                     ongoing4.df[i - start_month_offset + 1])
      
      # Total Partnerships
      ARTnet.wide.adjusted2 <- l.adjusted2 %>%
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
      ARTnet.wide.adjusted2 <- l.adjusted2 %>%
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
      ARTnet.wide.adjusted2 <- l.adjusted2 %>%
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

########################################
# SUPPLEMENTAL TABLE 7                 #
########################################

all.ptype <- n_month_offset_ptype(0,12)



