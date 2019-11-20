##############################################################################
########################### Data Import & Packages ###########################
##############################################################################

# install.packages("survival")
# install.packages("survminer")
# install.packages("flexsurv")

library(survival)
library(survminer)
library(flexsurv)
library(haven)
library(dplyr)

hurricane <- read_sas("Homework1_SA/hurricane.sas7bdat", NULL)

# # Accelerated Failure Time Model #
# aft.ln <- survreg(Surv(hour, reason == 1) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation, data = hurricane, dist = 'lognormal')
# summary(aft.ln)
#
# # Interpretation of Parameter Estimates #
# recid.aft.ln <- survreg(Surv(week, arrest == 1) ~ fin + age +mar + prio, data = recid, dist = 'lognormal')
# summary(recid.aft.ln)
#
# (exp(coef(recid.aft.ln))-1)*100
#
# # Exponential vs. Weibull #
# recid.aft.w <- survreg(Surv(week, arrest == 1) ~ fin + age + race + wexp + mar + paro + prio, data = recid, dist = 'weibull')
# summary(recid.aft.w)

##############################################################################
########################### Checking Distributions ###########################
##############################################################################

hurr.aft.w <-
  flexsurvreg(
    Surv(hour, reason == 1) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation,
    data = hurricane,
    dist =  "weibull"
  )

plot(
  hurr.aft.w,
  type = "cumhaz",
  ci = TRUE,
  conf.int = FALSE,
  las = 1,
  bty = "n",
  xlab = "week",
  ylab = "Cumulative Hazard",
  main = "Weibull Distribution"
)

hurr.aft.e <-
  flexsurvreg(
    Surv(hour, reason == 1) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation,
    data = hurricane,
    dist = "exp"
  )
plot(
  hurr.aft.e,
  type = "cumhaz",
  ci = TRUE,
  conf.int = FALSE,
  las = 1,
  bty = "n",
  xlab = "week",
  ylab = "Cumulative Hazard",
  main = "Exponential Distribution"
)

hurr.aft.g <-
  flexsurvreg(
    Surv(hour, reason == 1) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation,
    data = hurricane,
    dist =  "gamma"
  )
plot(
  hurr.aft.g,
  type = "cumhaz",
  ci = TRUE,
  conf.int = FALSE,
  las = 1,
  bty = "n",
  xlab = "week",
  ylab = "Cumulative Hazard",
  main = "Gamma Distribution"
)

hurr.aft.ll <-
  flexsurvreg(
    Surv(hour, reason == 1) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation,
    data = hurricane,
    dist =  "llogis"
  )
plot(
  hurr.aft.ll,
  type = "cumhaz",
  ci = TRUE,
  conf.int = FALSE,
  las = 1,
  bty = "n",
  xlab = "week",
  ylab = "Cumulative Hazard",
  main = "Log-Logistic Distribution"
)

hurr.aft.ln <-
  flexsurvreg(
    Surv(hour, reason == 1) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation,
    data = hurricane,
    dist =  "lognormal"
  )
plot(
  hurr.aft.ln,
  type = "cumhaz",
  ci = TRUE,
  conf.int = FALSE,
  las = 1,
  bty = "n",
  xlab = "week",
  ylab = "Cumulative Hazard",
  main = "Log-Normal Distribution"
)

hurr.aft.f <-
  flexsurvreg(
    Surv(hour, reason == 1) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation,
    data = hurricane,
    dist =  "genf"
  )
plot(
  hurr.aft.f,
  type = "cumhaz",
  ci = TRUE,
  conf.int = FALSE,
  las = 1,
  bty = "n",
  xlab = "week",
  ylab = "Cumulative Hazard",
  main = "F Distribution"
)

###########################################################################
########################## Goodness-of-Fit Tests ##########################
###########################################################################

# Not considering an exponential anymore because of obvious flaws in above plot.

like.w <-
  flexsurvreg(
    Surv(hour, reason == 1) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation,
    data = hurricane,
    dist = "weibull"
  )$loglik

like.ln <-
  flexsurvreg(
    Surv(hour, reason == 1) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation,
    data = hurricane,
    dist = "lnorm"
  )$loglik

like.g <-
  flexsurvreg(
    Surv(hour, reason == 1) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation,
    data = hurricane,
    dist = "gamma"
  )$loglik

like.ll <-
  flexsurvreg(
    Surv(hour, reason == 1) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation,
    data = hurricane,
    dist =  "llogis"
  )$loglik

like.f <-
  flexsurvreg(
    Surv(hour, reason == 1) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation,
    data = hurricane,
    dist =  "genf"
  )$loglik

pval.w.g <- 1 - pchisq((-2 * (like.w - like.g)), 1)
pval.ln.g <- 1 - pchisq((-2 * (like.ln - like.g)), 1)
pval.g.f <- 1 - pchisq((-2 * (like.g - like.f)), 1)
pval.ll.f <- 1 - pchisq((-2 * (like.ll - like.f)), 1)

Tests <- c('Wei vs. Gam', 'LogN vs. Gam', 'Gam vs. F', 'LogL vs. F')
P_values <-
  c(pval.w.g, pval.ln.g, pval.g.f, pval.ll.f, pvan.ln.f, pval.ln.ll)
cbind(Tests, P_values) # Weibull wins! Because Gamma beats LogNormal and Weibull v. Gamma p-value is 1.

#################################################################################################################
####################### Accelerated Failure Time (AFT) Model with Weibull distribution ##########################
#################################################################################################################

hurr.aft.w <-
  survreg(
    Surv(hour, reason == 1) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation,
    data = hurricane,
    dist = 'weibull'
  ) # Model including all variables
summary(hurr.aft.w)

# Backward selection
hurr.aft.w.bw = step(hurr.aft.w,
                trace = 0,
                direction = "backward",
                k = log(nrow(hurricane)))
summary(hurr.aft.w.bw) 

hurr.aft.w <-
  survreg(
    Surv(hour, reason == 1) ~ backup + servo + slope,
    data = hurricane,
    dist = 'weibull'
  ) # Model including selected variables
summary(hurr.aft.w)

# Value               Std. Error     z       p
# (Intercept)  4.7711     0.1524 31.31 < 2e-16
# backup       0.2710     0.1236  2.19 0.02831
# servo        0.3859     0.1306  2.95 0.00313
# slope       -0.0606     0.0174 -3.47 0.00051
# Log(scale)  -0.4381     0.0860 -5.10 3.5e-07

# Backup hour increase: 
100*(exp(0.2710) - 1) # Having a backup increases predicted hours until flooding by 31.1% 

# Servo hour increase:
100*(exp(0.3859) - 1) # Having a servo increases predicted hours until flooding by 47.1%

# Slope hour decrease:
100*(exp(-0.0606) - 1) # One unit increase to slopes decreases predicted hours until flooding by 5.9%

#########################################################
#################### Recommendations ####################
#########################################################

# I'm going to begin by assigning all of the pump stations IDs — this will make it easier to specify the pump stations I want to upgrade.
id <- c(1:770)
hurricane <- cbind(id, hurricane)

# I'm considering focusing improvements on pump stations that failed before the 24 hour mark
# with the assumption that the more pumps that are functioning, the less strain there is on the pump
# stations overall. Therefore, increasing some pumps lifespans (but not necessarily all the way to the 48hr mark)
# has the potential to lead to an increased overall mean lifespan. 

# With that in mind, I'm first going to filter the dataset by the pumps which flooded before the 24hr mark.

flood_b24hr <- hurricane[,c(1:8, 57:61)] %>%      # Ignoring all of the h# variables
  filter(reason == 1) %>%                         # Filtering by flood failures
  filter(hour < 24)                               # Filtering by failure time less than 24hr

# The result of this filteer is a subset of 115 pumps.

wo_servo <- flood_b24hr %>%         
  filter(servo == 0) %>%                          # Filtering for pumps without servos
  arrange(hour) %>%                               # Arranging by hour
  mutate(servo_new_hour = hour+(hour*0.471)) %>%  # Calculating potential result of added servo, $150k each! Could fix up to 16.
  filter(backup == 1) %>%                         # Let's only look at those that already have the backup upgrade.
  mutate(hour_diff = servo_new_hour - hour)       # Calculating the predicted hour difference!

# In this subset there now are 12 pumps; for 5 of those pumps (336, 353, 384, 406, 368), adding a servo is predicted to extended their lifespans by at least
# 8 hours and past the 24 hour mark. This upgrade would cost $750k. One of these stations (368), however, does have a slope of 10 ft.
# We might consider not upgrading that one with a servo because what we know about slope suggests that could be the bigger problem.
# Excluding the latter pump, this would cost $600k.

# What if we also did something about the slope for those pump stations sill not at the 24hr mark?

wo_servo_slope <- wo_servo %>%         
  filter(servo_new_hour < 24) %>%                                               # Filtering for those pumps still not at 24hrs
  mutate(slope_servo_new_hour = servo_new_hour + 3*(servo_new_hour*0.059)) %>%  # Adding predicted lifespan for change in slope-3
  mutate(hour_diff_both = slope_servo_new_hour - hour)                          # Calculating benefit of adding servo and slope-3

# There are 3 additional pump stations (330, 385, 405) whose lifespans could be prolonged to and past the 24hr mark with the addtion of a servo
# and work to decrease their slopes by 3ft. This would cost $180k each, a total of $540k. Note: I did calculate the new hours after the
# slope change based on the predicted failure hour with the addition of the servo rather than on the hour before the servo's installation.

# These upgrades and the aforementioned servo upgrades would total $1,140,000.

wo_backup <- flood_b24hr %>%
  filter(backup == 0) %>%
  arrange(hour) %>%
  mutate(backup_new_hour = hour+(hour*0.311)) %>% # $100k each! Could fix up to 25.
  filter(servo == 1) %>%                          # Let's consider pumps which do not already have the servo.
  mutate(hour_diff = backup_new_hour - hour)      # Calculating the predicted hour difference!

# In this subset there now are 9 pumps; for 5 of those pumps (409, 411, 407, 430, 323), adding a backup is predicted to extended their lifespans by at least
# 5 hours and past the 24 hour mark. This upgrade would cost $500k. One of these stations (323), however, does shave a slope of 13 ft.
# We might consider not upgrading that one with a servo because what we know about slope suggests that could be the bigger problem.
# Excluding the latter pump, these upgrades would cost $400k.

# What if we also did something about the slope for those pump stations sill not at the 24hr mark?

wo_backup_slope <- wo_backup %>%         
  filter(backup_new_hour < 24) %>%                                                 # Filtering for those pumps still not at 24hrs
  mutate(slope_backup_new_hour = backup_new_hour + 2*(backup_new_hour*0.059)) %>%  # Adding predicted lifespan for change in slope-2
  mutate(hour_diff_both = slope_backup_new_hour - hour)                            # Calculating benefit of adding servo and slope-2

# An additional pump station (386) hits the 24hr mark with the reduction of slope by just two units! Note: Once again, I did calculate the new
# hours after the slope change based on the predicted failure hour with the addition of the backup rather than on the hour before the 
# backup's installation. This upgrade would cost #120k.

# The total cost of these upgrades including backups would be $520k. The total cost of these recommended upgrades for 12 pump stations
# including backups, servos, and slope maintenace is $1,660,000 — $840k below budget. 

'%!in%' <- function(x,y)!('%in%'(x,y)) # This creates a "not in" function! Reference: https://stackoverflow.com/questions/5831794/opposite-of-in

upgrade_pumps <- c(336, 353, 384, 406, 330, 385, 405, 409, 411, 407, 430, 386)
flood_b24hr_lo <- flood_b24hr %>%
  filter(id %!in% upgrade_pumps)                   # Creates a subset of the flood failures I haven't yet recommended upgrades for

# For these remaining pump stations, let's see what we could do by just changing their slopes.

slope1 <- flood_b24hr_lo %>%
  mutate(slope_min1 = hour + (hour*0.059)) %>%
  filter(slope_min1 > 24)                        

# Three pumps (323, 377, 403) @ slope-1 exceed 24hr mark; this would cost $30k. One of these pumps is the one with a servo and no backup that is on
# 13 unit slope. Looks like we could make it last at least 24hrs by just changing the slope rather than paying +$90k to add a backup.

# Total cost: $1,690,000
# Remaining budget: $810k

upgrade_pumps <- c(336, 353, 384, 406, 330, 385, 405, 409, 411, 407, 430, 386, 323, 377, 403)
flood_b24hr_lo <- flood_b24hr %>%
  filter(id %!in% upgrade_pumps)                    # Creates a subset of the flood failures I haven't yet recommended upgrades for
                                            
slope2 <- flood_b24hr_lo %>%
  filter(slope >= 2) %>%                            # Obviously we only want to consider pumps with at least a 2 unit slope now
  mutate(slope_min2 = hour + 2*(hour*0.059)) %>%   
  filter(slope_min2 > 24)

# Two more pumps (344, 422) exceed 24hrs with the change of slope-2. This would cost $20k/ea, totaling $40k for this repair.

upgrade_pumps <- c(336, 353, 384, 406, 330, 385, 405, 409, 411, 407, 430, 386, 323, 377, 403, 344, 422)
flood_b24hr_lo <- flood_b24hr %>%
  filter(id %!in% upgrade_pumps)                    # Creates a subset of the flood failures I haven't yet recommended upgrades for

slope3 <- flood_b24hr_lo %>%
  filter(slope >= 3) %>%                            # Obviously we only want to consider pumps with at least a 3 unit slope now
  mutate(slope_min3 = hour + 3*(hour*0.059)) %>%   
  filter(slope_min3 > 24)

# No pumps make it to 24hrs with with improvement. Let's try a slope-4 change.

slope4 <- flood_b24hr_lo %>%
  filter(slope >= 4) %>%                          # Looking only at pumps with at least a 4 unit slope
  mutate(slope_min4 = hour + 4*(hour*0.059)) %>% 
  filter(slope_min4 > 24)

# Pump 368 is predicted to survive to the 24hr mark with a slope-4 change which would cost $40k

# Total cost: $1,730,000
# Remaining budget: $770k

upgrade_pumps <- c(336, 353, 384, 406, 330, 385, 405, 409, 411, 407, 430, 386, 323, 377, 403, 344, 422, 368)
flood_b24hr_lo <- flood_b24hr %>%
  filter(id %!in% upgrade_pumps)                    # Creates a subset of the flood failures I haven't yet recommended upgrades for

slope5 <- flood_b24hr_lo %>%
  filter(slope >= 5) %>%
  mutate(slope_min5 = hour + 5*(hour*0.059)) %>% 
  filter(slope_min5 > 24)

slope6 <- flood_b24hr_lo %>%
  filter(slope >= 6) %>%
  mutate(slope_min5 = hour + 6*(hour*0.059)) %>% 
  filter(slope_min5 > 24)

slope7 <- flood_b24hr_lo %>%
  filter(slope >= 7) %>%
  mutate(slope_min5 = hour + 7*(hour*0.059)) %>% 
  filter(slope_min5 > 24)

slope8 <- flood_b24hr_lo %>%
  filter(slope >= 8) %>%
  mutate(slope_min5 = hour + 8*(hour*0.059)) %>% 
  filter(slope_min5 > 24)

slope9 <- flood_b24hr_lo %>%
  filter(slope >= 9) %>%
  mutate(slope_min5 = hour + 9*(hour*0.059)) %>% 
  filter(slope_min5 > 24)

slope10 <- flood_b24hr_lo %>%
  filter(slope >= 10) %>%
  mutate(slope_min5 = hour + 10*(hour*0.059)) %>% 
  filter(slope_min5 > 24)

# I'm not going to test past this point because now we're talking about $100k worth of slope repair, which might better be spent on a 
# servo or backup upgrade. So far, I've extended the predicted lifespan of 18 pumps to at least the 24hr mark. According to our model,
# with these upgrades, only 33 of the 770 pumps would fail because of flooding during the first 24 hours. Not bad!
# Now, what to do with the remaining $770k and 97 pumps that flooded?

upgrade_pumps <- c(336, 353, 384, 406, 330, 385, 405, 409, 411, 407, 430, 386, 323, 377, 403, 344, 422, 368)
flooded <- hurricane[,c(1:8, 58:61)] %>%
  filter(reason == 1) %>%
  filter(id %!in% upgrade_pumps)            # This is a subset of the remaining flooded, not-upgraded pump stations.



flooded <- hurricane[,c(1,2,5, 59:60)] %>%
  filter(reason == 1)

backups <- flooded %>%
  filter(hour < 48) %>%
  filter(backup == 0) %>%
  mutate(pred_hour = hour+(hour*0.311)) %>%
  filter(pred_hour > 48) %>%
  mutate(cph = 100/(48-hour))
  
servo <- flooded %>%
  filter(hour < 48) %>%
  filter(servo == 0) %>%
  mutate(pred_hour = hour+(hour*0.471)) %>%
  filter(pred_hour > 48) %>%
  mutate(cph = 150/(48-hour)) 

# B to 317, 324, 333      for $9k/hr:    $300k
# S to 318, 343, 397      for $10k/hr:   $450k      $750k
# S to 417                for $10.7k/hr: $150k
# S to 364                for $11.5k/hr: $150k      $1,050,000
# B to 337, 342, 372, 380 for $12.5k/hr: $400k
# S to 419                for $12.5k/hr: $150k      $1,600,000
# S to 358                for $13.6k/hr: $150k
# B to 345, 449           for $14.3k/hr: $200k
# B to 366, 370           for $16.7k/hr: $200k      $2,150,000
# B to 357                for $20k/hr:   $100k
# S to 331                for $25k/hr:   $150k      $2,400,000      # Instead of the backup for $25k/hr because of the 2hr difference!
                                                                    # Oops. Over by $50k now! Can't do both, remove 394.
# B to 387                for $33.3k/hr: $100k      $2.5 million!   # 423 also an option but can't do both.












