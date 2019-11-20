# install.packages("survival")
# install.packages("survminer")

### Packages and data import from SAS ###
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)
library(haven)
hurricane <- read_sas("Homework1_SA/hurricane.sas7bdat", NULL)

### Percentage of pumps that survived the hurricane ###
summary(hurricane$survive) # = 41.04%

### Percentage of pumps in each type of failure and average failure time for each failure type ###
by_failure <- hurricane %>%
  group_by(reason) %>%
  summarise(avg_fail_hour = mean(hour), pct = length(reason) / 770)

### Survival probability for all pumps ###
hurricane_surv <-
  Surv(time = hurricane$hour, event = hurricane$survive == 0)

hurricane_km <- survfit(hurricane_surv ~ 0, data = hurricane)
# summary(hurricane_km)

### Visual of survival probability curve ###
ggsurvplot(
  hurricane_km,
  data = hurricane,
  conf.int = TRUE,
  palette = "blue",
  xlab = "Hour",
  ylab = "Survival Probability",
  legend = "none",
  title = "Failure of pumps during Hurricane Katrina",
  break.y.by = 0.1
)

### Survival probability stratified by failure type ###
hurricane_strat <-
  survfit(hurricane_surv ~ reason, data = hurricane)
# summary(hurricane_strat)

### Survival probability curve visual color-coded by reason for failure ###
ggsurvplot(
  hurricane_strat,
  data = hurricane,
  pval = TRUE,
  xlab = "Hour",
  ylab = "Survival Probability",
  break.y.by = 0.1,
  legend.title = "Failure reason"
)

### Conditional failure probability for all pumps ###
## Calculating hazard probabilities ##
hurricane_km$hp <- hurricane_km$n.event / hurricane_km$n.risk
print(hurricane_km$hp)

## Mergeing calculated hp with time sequence to a df ##
hurricane_haz <- merge(
  data.frame(time = seq(1, 48, 1)),
  data.frame(time = hurricane_km$time,
             hp = hurricane_km$hp),
  by = "time",
  all = TRUE
)

## If null than non-event ##
hurricane_haz[is.na(hurricane_haz) == TRUE] <- 0
# print(hurricane_haz)

### Visualizaing hazard probability across time period ###
plot(
  y = hurricane_haz$hp,
  x = hurricane_haz$time,
  main = "Hazard Probability Function",
  xlab = "Tenure",
  ylab = "Hazard Probability",
  type = 'l'
)

### Visualizaing CUMULATIVE hazard probability across time period ###
ggsurvplot(
  hurricane_km,
  data = hurricane,
  fun = "cumhaz",
  conf.int = TRUE,
  palette = "blue",
  title = "Cumulative Hazard Probability",
  xlab = "Hour",
  ylab = "Cumulative Hazard",
  legend = "none"
)

### Conditional failure (hazard) probability by failure type ###
## Took a roundabout approach and split the data for km, hp calculations ##
## Then added var of reason type back to df                              ##
## rbind dfs and plotted the result                                      ##
## (I didn't include reason 0 because those were non-events)             ##
### Reason 1 ----

hurricane1 <- hurricane %>%
  filter(reason == 1)

hurricane_surv1 <- Surv(time = hurricane1$hour, event = hurricane1$survive == 0)
hurricane_km1 <- survfit(hurricane_surv1 ~ 0, data = hurricane1)

hurricane_km1$hp <- hurricane_km1$n.event / hurricane_km1$n.risk
print(hurricane_km1$hp)

hurricane_haz1 <- merge(
  data.frame(time = seq(1, 48, 1)),
  data.frame(time = hurricane_km1$time,
             hp = hurricane_km1$hp),
  by = "time",
  all = TRUE
)
hurricane_haz1[is.na(hurricane_haz1) == TRUE] <- 0
hurricane_haz1$reason = 1
print(hurricane_haz1)

### Reason 2 ----

hurricane2 <- hurricane %>%
  filter(reason == 2)

hurricane_surv2 <- Surv(time = hurricane2$hour, event = hurricane2$survive == 0)
hurricane_km2 <- survfit(hurricane_surv2 ~ 0, data = hurricane2)

hurricane_km2$hp <- hurricane_km2$n.event / hurricane_km2$n.risk
print(hurricane_km2$hp)

hurricane_haz2 <- merge(
  data.frame(time = seq(1, 48, 1)),
  data.frame(time = hurricane_km2$time,
             hp = hurricane_km2$hp),
  by = "time",
  all = TRUE
)
hurricane_haz2[is.na(hurricane_haz2) == TRUE] <- 0
hurricane_haz2$reason = 2
print(hurricane_haz2)

### Reason 3 ----
hurricane3 <- hurricane %>%
  filter(reason == 3)

hurricane_surv3 <- Surv(time = hurricane3$hour, event = hurricane3$survive == 0)
hurricane_km3 <- survfit(hurricane_surv3 ~ 0, data = hurricane3)

hurricane_km3$hp <- hurricane_km3$n.event / hurricane_km3$n.risk
print(hurricane_km3$hp)

hurricane_haz3 <- merge(
  data.frame(time = seq(1, 48, 1)),
  data.frame(time = hurricane_km3$time,
             hp = hurricane_km3$hp),
  by = "time",
  all = TRUE
)
hurricane_haz3[is.na(hurricane_haz3) == TRUE] <- 0
hurricane_haz3$reason = 3
print(hurricane_haz3)

### Reason 4 ----
hurricane4 <- hurricane %>%
  filter(reason == 4)

hurricane_surv4 <- Surv(time = hurricane4$hour, event = hurricane4$survive == 0)
hurricane_km4 <- survfit(hurricane_surv4 ~ 0, data = hurricane4)

hurricane_km4$hp <- hurricane_km4$n.event / hurricane_km4$n.risk
print(hurricane_km4$hp)

hurricane_haz4 <- merge(
  data.frame(time = seq(1, 48, 1)),
  data.frame(time = hurricane_km4$time,
             hp = hurricane_km4$hp),
  by = "time",
  all = TRUE
)
hurricane_haz4[is.na(hurricane_haz4) == TRUE] <- 0
hurricane_haz4$reason = 4
print(hurricane_haz4)


all_haz <- rbind(hurricane_haz1, hurricane_haz2, hurricane_haz4, hurricane_haz3)

### The aforementioned visual ###
all_haz$reason <- as.factor(all_haz$reason) # Have to include this or reason is identified as a continuous variable
ggplot(all_haz,
       aes(x = time,
           y = hp,
           group = reason)) + 
  geom_line(aes(color = reason)) + 
  theme_minimal() +
  labs(title = "Hazard Probablity by Failure Reasons",
       y = "Hazard Probability",
       x = "Time")

### Testing if types of failure have similar survival probabilities ###

## Wilcoxon
survdiff(hurricane_surv ~ reason, rho = 1, data = hurricane)
# p = <2e-16 

## logrank
survdiff(hurricane_surv ~ reason, rho = 0, data = hurricane)
# p = <2e-16 (the same ^)

## Reasons 1 & 3 are currently paired, as are 2 & 4 
survdiff(hurricane_surv ~ reason, rho = 0, data = hurricane, subset = (reason==1|reason==3)) # p = 2e-10 
survdiff(hurricane_surv ~ reason, rho = 0, data = hurricane, subset = (reason==2|reason==4)) # p = <2e-16 
## Repeating with other pairings
survdiff(hurricane_surv ~ reason, rho = 0, data = hurricane, subset = (reason==1|reason==4)) # p = 2e-07 
survdiff(hurricane_surv ~ reason, rho = 0, data = hurricane, subset = (reason==1|reason==2)) # p = <2e-16 
survdiff(hurricane_surv ~ reason, rho = 0, data = hurricane, subset = (reason==2|reason==3)) # p = 5e-04 
