library(data.table)

ARTnet.long$start.date_mo <- floor_date(ARTnet.long$start.date)
ARTnet.long$end.date_mo <- floor_date(ARTnet.long$end.date)

x <- matrix(numeric(0), ncol=4)

for (i in ARTnet.wide$AMIS_ID) {

  # for total partnerhips
  ID.start <- ARTnet.long[ARTnet.long$AMIS_ID == i & ARTnet.long$ptype != 3,]$start.date_mo
  ID.end <- ARTnet.long[ARTnet.long$AMIS_ID == i & ARTnet.long$ptype != 3,]$end.date_mo

  test <- ID.start %in% ID.end

  overlap <- length(test[test == TRUE])

  # for main partnerships
  ID.start.m <- ARTnet.long[ARTnet.long$AMIS_ID == i & ARTnet.long$ptype == 1,]$start.date_mo
  ID.end.m <- ARTnet.long[ARTnet.long$AMIS_ID == i & ARTnet.long$ptype == 1,]$end.date_mo

  test.m <- ID.start.m %in% ID.end.m

  overlap.main <- length(test.m[test.m == TRUE])

  # for casual partnerships
  ID.start.c <- ARTnet.long[ARTnet.long$AMIS_ID == i & ARTnet.long$ptype == 2,]$start.date_mo
  ID.end.c <- ARTnet.long[ARTnet.long$AMIS_ID == i & ARTnet.long$ptype == 2,]$end.date_mo

  test.c <- ID.start.c %in% ID.end.c

  overlap.casl <- length(test.c[test.c == TRUE])

  #build dataframe
  x_i <- data.frame(AMIS_ID = i, overlap, overlap.main, overlap.casl)
  x <- rbind(x, x_i)

}

mean(x$overlap)
table(x$overlap)

mean(x$overlap.main)
table(x$overlap.main)

mean(x$overlap.casl)
table(x$overlap.casl)

cleaning <- ARTnet.long %>%
  select(AMIS_ID, SUB_DATE, start.date_mo, end.date_mo, ptype) %>%
  filter(start.date_mo == end.date_mo) %>%
  group_by(AMIS_ID) %>%
  summarise(subtract = n())


x <- x %>% left_join()

c <- matrix(numeric(0), ncol=4)

for (i in ARTnet.wide$AMIS_ID) {

  # across main and casual partnerships
  ID.start <- ARTnet.long[ARTnet.long$AMIS_ID == i & ARTnet.long$ptype != 3,]$start.date_mo
  ID.end <- ARTnet.long[ARTnet.long$AMIS_ID == i & ARTnet.long$ptype != 3,]$end.date_mo

  # for total partnerhips
  for (j in ID.start) {

    overlap <- sum(ID.end == j)

  }

  overlap.total <- sum(overlap)

  # for main partnerships
  ID.start.m <- ARTnet.long[ARTnet.long$AMIS_ID == i & ARTnet.long$ptype == 1,]$start.date_mo
  ID.end.m <- ARTnet.long[ARTnet.long$AMIS_ID == i & ARTnet.long$ptype == 1,]$end.date_mo

  for (h in ID.start.m) {

    overlap.m <- sum(ID.end.m == h)

  }

  overlap.main <- sum(overlap.m)

  # for casual partnerships
  ID.start.c <- ARTnet.long[ARTnet.long$AMIS_ID == i & ARTnet.long$ptype == 2,]$start.date_mo
  ID.end.c <- ARTnet.long[ARTnet.long$AMIS_ID == i & ARTnet.long$ptype == 2,]$end.date_mo

  for (k in ID.start.c) {

    overlap.c <- sum(ID.end.c == k)

  }

  overlap.casl <- sum(overlap.c)

  #build dataframe
  c_i <- data.frame(AMIS_ID = i, overlap.total, overlap.main, overlap.casl)
  c <- rbind(c, c_i)

}

table(c$overlap.total)
table(c$overlap.main)
table(c$overlap.casl)

md_df <- race.cat2
cat <- "Race/Ethnicity"
labels <- c("Black","Hispanic", "Other", "White")

total_plot_title <- paste("Mean Degree Comparison by ",
                          cat," - ",
                          "Total Partnerships")
#main_plot_title <- paste("Mean Degree Comparison by ",
#                         cat," - ",
#                         "Main Partnerships")
#casl_plot_title <- paste("Mean Degree Comparison by ",
#                         cat," - ",
#                         "Casual Partnerships")

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
             linetype = "dashed")
