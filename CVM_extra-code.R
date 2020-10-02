## N-Month Offset Method-----------------------------------------------------------------------

#Next, instead of assessing ongoing partnerships using a three-month offset, we
#can generalize our approach to look at n-month offsets for user specified values
#of n.

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
  assign(ongoing3, ifelse(is.na(ifelse(ARTnet.long.adjusted$start.date <
                                         get(ongoing.eval) &
                                         ARTnet.long.adjusted$end.date >= get(ongoing.eval),
                                       1, 0)), 0, ifelse(ARTnet.long.adjusted$start.date <
                                                           get(ongoing.eval) &
                                                           ARTnet.long.adjusted$end.date >= get(ongoing.eval),
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


# RESULTS ------------------------------------------------------------------------------------

par(mfrow=c(2,2))

plot(month, mean.degree.main,
     main = "\n\n\nMain Partnerships",
     xlab = "Number of Offset Months",
     ylab = "Mean Degree",
     ylim = c(0,1),
     cex.main = 0.8)
abline(h=mean.deg.main.dos, col="blue", xlim = c())
text(0.45*(end_month_offset - start_month_offset),
     1.12*mean.deg.main.dos,
     paste("Mean Degree:\nDay-of-Survey Method = ",round(mean.deg.main.dos,4),
           sep=""),
     cex = 0.8,
     adj = 0)

plot(month, mean.degree.casual,
     main = "\n\n\nCasual Partnerships",
     xlab = "Number of Offset Months",
     ylab = "Mean Degree",
     ylim = c(0,1),
     cex.main = 0.8)
lines(mean.deg.casl.dos)
abline(h=mean.deg.casl.dos, col="purple")
text(0.45*(end_month_offset - start_month_offset),
     1.1*mean.deg.casl.dos,
     paste("Mean Degree:\nDay-of-Survey Method = ",round(mean.deg.casl.dos,4),
           sep=""),
     cex = 0.8,
     adj = 0)
mtext("Partnership Mean Degree", side = 3, line = -1, outer = TRUE, font = 2)
mtext("N-Month Offset Method - Points, Day-of-Survey Method - Line", side = 3,
      line = -2, outer = TRUE, font = 2, cex = 0.75)

plot(month, mean.degree.main,
     main = "Main Partnerships - Scaled",
     xlab = "Number of Offset Months",
     ylab = "Mean Degree",
     cex.main = 0.8)
abline(h=mean.deg.main.dos, col="blue")
text(0.3*(end_month_offset - start_month_offset),
     0.99*mean.deg.main.dos,
     paste("Mean Degree:\nDay-of-Survey Method = ",round(mean.deg.main.dos,4),sep=""),
     cex = .8,
     adj = 0)

plot(month, mean.degree.casual,
     main = "Casual Partnerships - Scaled",
     xlab = "Number of Offset Months",
     ylab = "Mean Degree",
     cex.main = 0.8)
lines(mean.deg.casl.dos)
abline(h=mean.deg.casl.dos, col="purple")
text(0.45*(end_month_offset - start_month_offset),
     1.01*mean.deg.casl.dos,
     paste("Mean Degree:\nDay-of-Survey Method = ",round(mean.deg.casl.dos,4),
           sep=""),
     cex = .8,
     adj = 0)

#par(xpd = TRUE)
#text(par("usr")[2] - 18,y=mean(par("usr")[3:4]) + 0,"test taxis label",srt=-270)

#Mean degree - main
mean.degree.main.df <- data.frame(n_month, round(mean.degree.main,4))

#Mean degree - casual
mean.degree.casual.df <- data.frame(n_month, round(mean.degree.casual,4))

mean.degree.table <- mean.degree.main.df %>%
  left_join(mean.degree.casual.df, by = "n_month")
colnames(mean.degree.table) <- c("Month", "Main", "Casual")

mean.degree.table <- setNames(data.frame(t(mean.degree.table[,-1])),
                              mean.degree.table[,1])

dos_ref_col <- rbind(c(round(mean.deg.main.dos,4)),c(round(
  mean.deg.casl.dos,4)))
colnames(dos_ref_col) <- c("Day-of-Survey")

mean.degree.table <- cbind(dos_ref_col, mean.degree.table)

kable(mean.degree.table) %>%
  kable_styling(bootstrap_options = "striped", font_size = 6)

## investigation_zero_month------------------------------------------------------------------

# The code below investigates why the 0-month offset yields a higher mean degree compared to the DoS method.

ARTnet.long$ONGOING <- as.numeric(ARTnet.long$ONGOING)
ARTnet.long$ongoing2 <- ifelse(is.na(ARTnet.long$ONGOING), 0,
                               ARTnet.long$ONGOING)

prior.month.of.evaluation <- 0
ARTnet.long$ongoing.evaluation.date <- ARTnet.long$SUB_DATE -
  round(prior.month.of.evaluation*30.44)
ARTnet.long$ongoing3 <- ifelse(ARTnet.long$start.date <
                                 ARTnet.long$ongoing.evaluation.date &
                                 ARTnet.long$end.date >=
                                 ARTnet.long$ongoing.evaluation.date, 1, 0)
ARTnet.long$ongoing3 <- ifelse(is.na(ARTnet.long$ongoing3), 0,
                               ARTnet.long$ongoing3)

ARTnet.wide <- ARTnet.long %>%
  filter(RAI == 1 | IAI == 1 | ROI == 1 | IOI == 1) %>%
  filter(ptype == 1) %>%
  group_by(AMIS_ID) %>%
  summarise(deg.main.dos = sum(ongoing2),
            deg.main.zero.month.offset = sum(ongoing3)) %>%
  right_join(ARTnet.wide, by = "AMIS_ID")

ARTnet.wide <- ARTnet.long %>%
  filter(RAI == 1 | IAI == 1 | ROI == 1 | IOI == 1) %>%
  filter(ptype == 2) %>%
  group_by(AMIS_ID) %>%
  summarise(deg.casl.dos = sum(ongoing2),
            deg.casl.zero.month.offset = sum(ongoing3)) %>%
  right_join(ARTnet.wide, by = "AMIS_ID")

ARTnet.wide$deg.main.dos <- ifelse(is.na(ARTnet.wide$deg.main.dos),
                                   0, ARTnet.wide$deg.main.dos)
ARTnet.wide$deg.casl.dos <- ifelse(is.na(ARTnet.wide$deg.casl.dos),
                                   0, ARTnet.wide$deg.casl.dos)
ARTnet.wide$deg.main.zero.month.offset <- ifelse(is.na(
  ARTnet.wide$deg.main.zero.month.offset),
  0, ARTnet.wide$deg.main.zero.month.offset)
ARTnet.wide$deg.casl.zero.month.offset <- ifelse(is.na(
  ARTnet.wide$deg.casl.zero.month.offset),
  0, ARTnet.wide$deg.casl.zero.month.offset)


## ----dos_zmo_comparison1, echo = FALSE----------------------------------------------------------------------------------------------------------

ARTnet.wide %>%
  select(AMIS_ID, deg.casl.dos, deg.casl.zero.month.offset, deg.main.dos, deg.main.zero.month.offset)%>%
  filter(deg.casl.dos > deg.casl.zero.month.offset | deg.main.dos >
           deg.main.zero.month.offset)



## ----dos_zmo_comparison2, echo=FALSE------------------------------------------------------------------------------------------------------------

ARTnet.wide %>%
  select(AMIS_ID, deg.casl.dos, deg.casl.zero.month.offset, deg.main.dos, deg.main.zero.month.offset)%>%
  filter(deg.casl.dos < deg.casl.zero.month.offset | deg.main.dos <
           deg.main.zero.month.offset)



## ----dos_zmo_comparison3, echo=FALSE------------------------------------------------------------------------------------------------------------

ARTnet.wide %>%
  select(AMIS_ID, deg.casl.dos, deg.casl.zero.month.offset, deg.main.dos, deg.main.zero.month.offset)%>%
  filter(0 < deg.casl.zero.month.offset - deg.casl.dos | 0 < deg.main.zero.month.offset - deg.main.dos) %>%
  inner_join(ARTnet.long, ARTnet.wide, by = "AMIS_ID")%>%
  select(AMIS_ID, ptype, start.date, end.date, SUB_DATE, ongoing2, ongoing3)%>%
  filter(ongoing2 != ongoing3 & ptype %in% c(1,2) & SUB_DATE == end.date)



## ----dos_zmo_comparison4, echo=FALSE------------------------------------------------------------------------------------------------------------

ARTnet.wide %>%
  select(AMIS_ID, deg.casl.dos, deg.casl.zero.month.offset, deg.main.dos, deg.main.zero.month.offset)%>%
  filter(AMIS_ID %in% c(27110546, 2231741, 2589295, 2251818, 214311))

# Explore some of these cases
ARTnet.long %>%
  select(AMIS_ID, ptype, start.date, end.date, SUB_DATE, ongoing2, ongoing3) %>%
  filter(AMIS_ID %in% c(27110546, 2231741, 2589295, 2251818, 214311) &
           ptype %in% c(1,2))

mean.deg.main.dos <- mean(ARTnet.wide$deg.main.dos)
mean.deg.casl.dos <- mean(ARTnet.wide$deg.casl.dos)


## ----lineplot, echo=FALSE-----------------------------------------------------------------------------------------------------------------------

df <- ARTnet.long %>%
  filter(RAI == 1 | IAI == 1 | ROI == 1 | IOI == 1) %>%
  filter(ptype %in% c(1,2))

df$dos.ref <- as.numeric(df$SUB_DATE - df$SUB_DATE) #X-axis 0 point
df$start.date.dos.ref <- as.numeric(df$SUB_DATE - df$start.date)
df$end.date.dos.ref <- as.numeric(df$SUB_DATE - df$end.date)
df$ptype_char <- ifelse(df$ptype == 1, "Main", "Casual")
df$ptype_char <- as.factor(df$ptype_char)

df <- df %>% arrange(desc(ptype), end.date.dos.ref)

df$UID <- seq(1,nrow(df),1)
df$UID_INV <- (nrow(df) + 1) - df$UID #Y-axis value

plot <- ggplot(data=df, aes(dos.ref,UID_INV)) + coord_cartesian(xlim =
                                                                  c(0,365)) +
  ylab("Partnerships") + xlab("Days Offset from Day of Survey")

plot + geom_segment(aes(x = end.date.dos.ref, y = UID_INV, xend =
                          start.date.dos.ref, yend =
                          UID_INV, color = ptype_char), data = df)
