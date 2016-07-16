library(ggplot2)
library(knitr) 
library(lubridate)
library(pscl)
library(texreg)
library(lme4)
library(lmerTest)
library(stargazer)

opts_chunk$set(fig.path = 'figures/')

###################################
### HIGH LEVEL QUESTIONS I NEED TO ANSWER
# 1. What should the intervention be?
#   1a. How many people receive the treatment?
#   1b. How does randomization work?
# 2. What covariates should be used for regression adjustment
## 2a. Post level newcomer removed analysis
## 2b. Post level newcomer comment analysis
## 2c. Comment level newcomer analysis
## 2d. Comment level newcomer analysis

## HIGH LEVEL QUESTIONS FOR LATER:
# #1. How long should the experiment period be?


###################################
#### LOAD POSTS 
full.posts <- read.csv("outputs/r_science_posts_2016.04.04_22.09.42-2016.06.30_23.59.34.csv")
full.posts$post.ama <- full.posts$post.ama=="True"
full.posts$datetime <- as.POSIXct(full.posts$created)
full.posts$post.hour <- hour(full.posts$datetime)

short.posts <- read.csv("outputs/r_science_posts_2016.07.04_04.46.26-2016.07.14_21.36.36.csv")
short.posts$post.ama <- short.posts$post.ama=="True"
short.posts$datetime <- as.POSIXct(short.posts$created_utc, origin="1970-01-01")
short.posts$post.hour <- hour(short.posts$datetime)

full.posts$newcomer.comments.experiment.day.pct <- (full.posts$newcomer.comments.experiment.day + 1) / (full.posts$newcomer.comments + 1)
full.posts$newcomer.comments.removed.experiment.day.pct <- (full.posts$newcomer.comments.removed.experiment.day + 1) / (full.posts$newcomer.comments.removed + 1)

short.posts$newcomer.comments.removed.experiment.day.zero <- short.posts$newcomer.comments.removed.experiment.day == 0
short.posts$newcomer.comments.experiment.day.zero <- short.posts$newcomer.comments.experiment.day == 0
full.posts$newcomer.comments.removed.experiment.day.zero <- full.posts$newcomer.comments.removed.experiment.day == 0
full.posts$newcomer.comments.experiment.day.zero <- full.posts$newcomer.comments.experiment.day == 0

full.posts$newcomer.comments.zero <- full.posts$newcomer.comments == 0
full.posts$newcomer.comments.removed.zero <- full.posts$newcomer.comments == 0

short.posts$newcomer.comments.zero <- short.posts$newcomer.comments == 0
short.posts$newcomer.comments.removed.zero <- short.posts$newcomer.comments == 0

###################################
#### LOAD COMMENTS
full.comments <- read.csv("outputs/r_science_comments_2016.04.04_22.09.42-2016.06.30_23.59.34.csv")
short.comments <- read.csv("outputs/r_science_comments_2016.07.04_04.46.26-2016.07.14_21.36.36.csv")
full.comments$datetime <-  as.POSIXct(full.comments$created)
short.comments$datetime <-  as.POSIXct(short.comments$created_utc, origin="1970-01-01")
full.comments$hour <- hour(full.comments$datetime)
short.comments$hour <- hour(short.comments$datetime)
short.comments$post.ama <- short.comments$post.ama=="True"
full.comments$post.ama <- full.comments$post.ama=="True"

full.comments$post.datetime <- as.POSIXct(full.comments$post.created)
full.comments$post.hour <- hour(full.comments$post.datetime)

short.comments$post.datetime <- as.POSIXct(short.comments$post.created)
short.comments$post.hour <- hour(short.comments$post.datetime)


###################################
### BEGIN REPORT
# 1. How many newcomers are there
# 2. How many newcomers don't delete their accounts later?
library(editR)
editR("/Users/nathan/Documents/github/CivilServant-Analysis/reports/experiment.planning.07.16.2016.Rmd")

min(full.comments$datetime)
max(full.comments$datetime)

### PLOT COMMENTS PER HOUR
ggplot(full.comments, aes(hour)) +
  ggtitle("Comments Per Hour, April 4 2016 - May 30 2016") +
  geom_histogram(bins=24)

### SUMMARIZE HOW MANY NEWCOMER COMMENTS and REMOVED COMMENTS FALL WITHIN THE EXPERIMENT DAY
ggplot(subset(full.posts, newcomer.comments > 0), aes(newcomer.comments.experiment.day.pct)) +
  ggtitle(expression(atop("Percent Comments Within Day-Randomized Experiment Period April 4 2016 - May 30 2016", atop(italic("Within the subset of posts with at least 1 newcomer comment"))))) +
  theme(axis.text.x = element_text(angle=-45, hjust=0, vjust=1), 
        #plot.margin = unit(c(1.5, 1, 1, 1), "cm"), 
        plot.title = element_text(size = 25, face = "bold", colour = "black", vjust = -1)) +
  geom_histogram(bins=100) 
  
ggplot(subset(full.posts, newcomer.comments > 0), aes(newcomer.comments.removed.experiment.day.pct)) +
  ggtitle(expression(atop("Percent Removed Comments Within Day-Randomized Experiment Period April 4 2016 - May 30 2016", atop(italic("Within the subset of posts with at least 1 newcomer comment"))))) +
  theme(axis.text.x = element_text(angle=-45, hjust=0, vjust=1), 
        #plot.margin = unit(c(1.5, 1, 1, 1), "cm"), 
        plot.title = element_text(size = 15, face = "bold", colour = "black", vjust = -1)) +
  geom_histogram(bins=100)

##################################################
### HOW SHOULD THE DEPENDENT VARIABLES BE MODELED? (COMMENT BOX RULES EXPERIMENT)

################################
##### Newcomer Comments Removed

ggplot(short.posts, aes(newcomer.comments.removed.experiment.day)) +
  ggtitle("Experiment Priod Newcomer Comments Removed Per Post, July 4 - 14 2016") +
  geom_histogram(bins=100)

ggplot(subset(short.posts, visible=="True"), aes(newcomer.comments.removed.experiment.day)) +
  ggtitle("Experiment Priod Newcomer Comments Removed Per Visible Post, July 4 - 14 2016") +
  geom_histogram(bins=100)

summary(short.posts$newcomer.comments.removed.experiment.day.zero)
summary(subset(short.posts, visible=="True")$newcomer.comments.removed.experiment.day.zero)

summary(ncrmexz1 <- glm(newcomer.comments.removed.experiment.day.zero ~ 1, data=short.posts, family=binomial))
summary(ncrmexz2 <- glm(newcomer.comments.removed.experiment.day.zero ~ visible, data=short.posts, family=binomial))
summary(ncrmexz3 <- glm(newcomer.comments.removed.experiment.day.zero ~ visible + post.sub.top.minutes, data=short.posts, family=binomial))
summary(ncrmexz4 <- glm(newcomer.comments.removed.experiment.day.zero ~ visible + post.sub.top.minutes + post.ama, data=short.posts, family=binomial))
summary(ncrmexz5 <- glm(newcomer.comments.removed.experiment.day.zero ~ visible + post.sub.top.minutes + experiment.day.minutes, data=short.posts, family=binomial))
summary(ncrmexz6 <- glm(newcomer.comments.removed.experiment.day.zero ~ visible + post.sub.top.minutes + post.hour + I(post.hour^2), data=short.posts, family=binomial))
summary(ncrmexz7 <- glm(newcomer.comments.removed.experiment.day.zero ~ visible + post.sub.top.minutes + weekend, data=short.posts, family=binomial))
summary(ncrmexz8 <- glm(newcomer.comments.removed.experiment.day.zero ~ visible + post.sub.top.minutes + post.flair, data=short.posts, family=binomial))

stargazer(ncrmexz1, ncrmexz2, ncrmexz3, ncrmexz4, ncrmexz5, ncrmexz6, ncrmexz7, ncrmexz8, type="text")
#htmlreg(list(ncrmexz1, ncrmexz2, ncrmexz3, ncrmexz4, ncrmexz5, ncrmexz6, ncrmexz7, ncrmexz8))


summary(model.newcomer.comments.rm.exp.zi.1 <- zeroinfl(newcomer.comments.removed.experiment.day ~ 1 | 
                                                          visible + post.sub.top.minutes, 
                                                        data=short.posts))
summary(model.newcomer.comments.rm.exp.zi.2 <- zeroinfl(newcomer.comments.removed.experiment.day ~ visible | 
                                                          visible + post.sub.top.minutes, 
                                                        data=short.posts))
summary(model.newcomer.comments.rm.exp.zi.3 <- zeroinfl(newcomer.comments.removed.experiment.day ~ visible +  post.ama | 
                                                          visible + post.sub.top.minutes, 
                                                        data=short.posts))
summary(model.newcomer.comments.rm.exp.zi.4 <- zeroinfl(newcomer.comments.removed.experiment.day ~ visible + post.ama +  post.sub.top.minutes | 
                                                          visible + post.sub.top.minutes, 
                                                        data=short.posts))

summary(model.newcomer.comments.rm.exp.zi.5 <- zeroinfl(newcomer.comments.removed.experiment.day ~ visible + post.ama +  post.sub.top.minutes + experiment.day.minutes | 
                                                          visible + post.sub.top.minutes, 
                                                        data=short.posts))

summary(model.newcomer.comments.rm.exp.zi.6 <- zeroinfl(newcomer.comments.removed.experiment.day ~ visible + post.ama +  post.sub.top.minutes + experiment.day.minutes + weekend | 
                                                          visible + post.sub.top.minutes, 
                                                        data=short.posts))

summary(model.newcomer.comments.rm.exp.zi.7 <- zeroinfl(newcomer.comments.removed.experiment.day ~ visible + post.ama +  post.sub.top.minutes + experiment.day.minutes + weekend + post.flair | 
                                                          visible + post.sub.top.minutes, 
                                                        data=short.posts))
summary(model.newcomer.comments.rm.exp.zi.8 <- zeroinfl(newcomer.comments.removed.experiment.day ~ visible + post.ama +  post.sub.top.minutes + experiment.day.minutes + weekend + post.flair + post.hour + I(post.hour^2) | 
                                                  visible + post.sub.top.minutes, 
                                                data=short.posts))
stargazer(model.newcomer.comments.rm.exp.zi.1,
          model.newcomer.comments.rm.exp.zi.2,
          model.newcomer.comments.rm.exp.zi.3,          
          model.newcomer.comments.rm.exp.zi.4,          
          model.newcomer.comments.rm.exp.zi.5,
          model.newcomer.comments.rm.exp.zi.6,
          model.newcomer.comments.rm.exp.zi.7, 
          model.newcomer.comments.rm.exp.zi.8, type="text")

htmlreg(list(model.newcomer.comments.rm.exp.zi.1,
          model.newcomer.comments.rm.exp.zi.2,
          model.newcomer.comments.rm.exp.zi.3,          
          model.newcomer.comments.rm.exp.zi.4,          
          model.newcomer.comments.rm.exp.zi.5,
          model.newcomer.comments.rm.exp.zi.6,
          model.newcomer.comments.rm.exp.zi.7), type="html")

################################
##### Newcomer Comments

ggplot(short.posts, aes(newcomer.comments.removed.experiment.day)) +
  ggtitle("Experiment Priod Newcomer Comments Removed Per Post, July 4 - 14 2016") +
  geom_histogram(bins=100)

ggplot(subset(short.posts, visible=="True"), aes(newcomer.comments.removed.experiment.day)) +
  ggtitle("Experiment Priod Newcomer Comments Removed Per Visible Post, July 4 - 14 2016") +
  geom_histogram(bins=100)

summary(short.posts$newcomer.comments.experiment.day.zero)

summary(subset(short.posts, visible=="True")$newcomer.comments.experiment.day.zero)

## Fit the logistic regression
ncexz1 <- glm(newcomer.comments.experiment.day.zero ~ 1, data=short.posts, family=binomial)
ncexz2 <- glm(newcomer.comments.experiment.day.zero ~ visible, data=short.posts, family=binomial)
ncexz3 <- glm(newcomer.comments.experiment.day.zero ~ visible + post.sub.top.minutes, data=short.posts, family=binomial)
ncexz4 <- glm(newcomer.comments.experiment.day.zero ~ visible + post.sub.top.minutes + post.ama, data=short.posts, family=binomial)
ncexz5 <- glm(newcomer.comments.experiment.day.zero ~ visible + post.sub.top.minutes + experiment.day.minutes, data=short.posts, family=binomial)
ncexz6 <- glm(newcomer.comments.experiment.day.zero ~ visible + post.sub.top.minutes + post.hour + I(post.hour^2), data=short.posts, family=binomial)
ncexz7 <- glm(newcomer.comments.experiment.day.zero ~ visible + post.sub.top.minutes + weekend, data=short.posts, family=binomial)
ncexz8 <- glm(newcomer.comments.experiment.day.zero ~ visible + post.sub.top.minutes + post.flair, data=short.posts, family=binomial)

#htmlreg(list(ncexz1, ncexz2, ncexz3, ncexz4, ncexz5, ncexz6, ncexz7, ncexz8))
stargazer(ncexz1, ncexz2, ncexz3, ncexz4, ncexz5, ncexz6, ncexz7, ncexz8, type="text")

### Now fit the zero inflated model
model.newcomer.comments.exp.zi.1 <- zeroinfl(newcomer.comments.experiment.day ~ 1 | 
                                               visible + post.sub.top.minutes,  data=short.posts)
model.newcomer.comments.exp.zi.2 <- zeroinfl(newcomer.comments.experiment.day ~ visible | 
                                               visible + post.sub.top.minutes,  data=short.posts)
model.newcomer.comments.exp.zi.3 <- zeroinfl(newcomer.comments.experiment.day ~ visible + post.ama| 
                                               visible + post.sub.top.minutes,  data=short.posts)
model.newcomer.comments.exp.zi.4 <- zeroinfl(newcomer.comments.experiment.day ~ visible + post.ama + post.sub.top.minutes | 
                                               visible + post.sub.top.minutes,  data=short.posts)
model.newcomer.comments.exp.zi.5 <- zeroinfl(newcomer.comments.experiment.day ~ visible + post.ama + post.sub.top.minutes + experiment.day.minutes | 
                                               visible + post.sub.top.minutes,  data=short.posts)
model.newcomer.comments.exp.zi.6 <- zeroinfl(newcomer.comments.experiment.day ~ visible + post.ama + post.sub.top.minutes + experiment.day.minutes + post.hour + I(post.hour^2)  | 
                                               visible + post.sub.top.minutes,  data=short.posts)
model.newcomer.comments.exp.zi.7 <- zeroinfl(newcomer.comments.experiment.day ~ visible + post.ama + post.sub.top.minutes + experiment.day.minutes + post.hour + I(post.hour^2) + weekend | 
                                               visible + post.sub.top.minutes,  data=short.posts)
model.newcomer.comments.exp.zi.8 <- zeroinfl(newcomer.comments.experiment.day ~ visible + post.ama + post.sub.top.minutes + experiment.day.minutes + post.hour + I(post.hour^2) + weekend + post.flair| 
                                               visible + post.sub.top.minutes,  data=short.posts)

stargazer(model.newcomer.comments.exp.zi.1,
          model.newcomer.comments.exp.zi.2,
          model.newcomer.comments.exp.zi.3,
          model.newcomer.comments.exp.zi.4,
          model.newcomer.comments.exp.zi.5,
          model.newcomer.comments.exp.zi.6,
          model.newcomer.comments.exp.zi.7,
          model.newcomer.comments.exp.zi.8, type="text")
# htmlreg(list(model.newcomer.comments.exp.zi.1,
#           model.newcomer.comments.exp.zi.2,
#           model.newcomer.comments.exp.zi.3,
#           model.newcomer.comments.exp.zi.4,
#           model.newcomer.comments.exp.zi.5,
#           model.newcomer.comments.exp.zi.6,
#           model.newcomer.comments.exp.zi.7,
#           model.newcomer.comments.exp.zi.8))

##################################################
### HOW SHOULD THE DEPENDENT VARIABLES BE MODELED? (STICKY COMMENT EXPERIMENT)

###########################
######## Newcomer Comments Removed

# predict zeroes
ncrmz1 <- glm(newcomer.comments.removed.zero ~ 1, data=short.posts, family=binomial)
ncrmz2 <- glm(newcomer.comments.removed.zero ~ visible, data=short.posts, family=binomial)
ncrmz3 <- glm(newcomer.comments.removed.zero ~ visible + post.sub.top.minutes, data=short.posts, family=binomial)
ncrmz4 <- glm(newcomer.comments.removed.zero ~ visible + post.sub.top.minutes + post.ama, data=short.posts, family=binomial)
ncrmz5 <- glm(newcomer.comments.removed.zero ~ visible + post.sub.top.minutes + post.hour + I(post.hour^2), data=short.posts, family=binomial)
ncrmz6 <- glm(newcomer.comments.removed.zero ~ visible + post.sub.top.minutes + weekend, data=short.posts, family=binomial)
ncrmz7 <- glm(newcomer.comments.removed.zero ~ visible + post.sub.top.minutes + post.flair, data=short.posts, family=binomial)

htmlreg(list(ncrmexz1, ncrmexz2, ncrmexz3, ncrmexz4, ncrmexz5, ncrmexz6, ncrmexz7))
stargazer(ncrmz1, ncrmz2, ncrmz3, ncrmz4, ncrmz5, ncrmz6, ncrmz7, type="text")


# full models
model.newcomer.comments.rm.zi.1 <- zeroinfl(newcomer.comments.removed ~ 1 | 
                                                          visible + post.sub.top.minutes, 
                                                        data=short.posts)
model.newcomer.comments.rm.zi.2 <- zeroinfl(newcomer.comments.removed ~ visible | 
                                                          visible + post.sub.top.minutes, 
                                                        data=short.posts)
model.newcomer.comments.rm.zi.3 <- zeroinfl(newcomer.comments.removed ~ visible +  post.ama | 
                                                          visible + post.sub.top.minutes, 
                                                        data=short.posts)
model.newcomer.comments.rm.zi.4 <- zeroinfl(newcomer.comments.removed ~ visible + post.ama +  post.sub.top.minutes | 
                                                          visible + post.sub.top.minutes, 
                                                        data=short.posts)
model.newcomer.comments.rm.zi.5 <- zeroinfl(newcomer.comments.removed ~ visible + post.ama +  post.sub.top.minutes + weekend | 
                                                          visible + post.sub.top.minutes, 
                                                        data=short.posts)
model.newcomer.comments.rm.zi.6 <- zeroinfl(newcomer.comments.removed ~ visible + post.ama +  post.sub.top.minutes + weekend + post.flair | 
                                                          visible + post.sub.top.minutes, 
                                                        data=short.posts)
model.newcomer.comments.rm.zi.7 <- zeroinfl(newcomer.comments.removed ~ visible + post.ama +  post.sub.top.minutes + weekend + post.flair + post.hour + I(post.hour^2) | 
                                                          visible + post.sub.top.minutes, 
                                                        data=short.posts)

# htmlreg(list(model.newcomer.comments.rm.zi.1,
#              model.newcomer.comments.rm.zi.2,
#              model.newcomer.comments.rm.zi.3,          
#              model.newcomer.comments.rm.zi.4,          
#              model.newcomer.comments.rm.zi.5,
#              model.newcomer.comments.rm.zi.6,
#              model.newcomer.comments.rm.zi.7), type="html")

stargazer(model.newcomer.comments.rm.zi.1,
          model.newcomer.comments.rm.zi.2,
          model.newcomer.comments.rm.zi.3,          
          model.newcomer.comments.rm.zi.4,          
          model.newcomer.comments.rm.zi.5,
          model.newcomer.comments.rm.zi.6,
          model.newcomer.comments.rm.zi.7, type="text")

###########################
######## Newcomer Comments
ncz1 <- glm(newcomer.comments.zero ~ 1, data=short.posts, family=binomial)
ncz2 <- glm(newcomer.comments.zero ~ visible, data=short.posts, family=binomial)
ncz3 <- glm(newcomer.comments.zero ~ visible + post.sub.top.minutes, data=short.posts, family=binomial)
ncz4 <- glm(newcomer.comments.zero ~ visible + post.sub.top.minutes + post.ama, data=short.posts, family=binomial)
ncz5 <- glm(newcomer.comments.zero ~ visible + post.sub.top.minutes + post.hour + I(post.hour^2), data=short.posts, family=binomial)
ncz6 <- glm(newcomer.comments.zero ~ visible + post.sub.top.minutes + weekend, data=short.posts, family=binomial)
ncz7 <- glm(newcomer.comments.zero ~ visible + post.sub.top.minutes + post.flair, data=short.posts, family=binomial)

#htmlreg(list(ncz1, ncz2, ncz3, ncz4, ncz5, ncz6, ncz7))
stargazer(ncz1, ncz2, ncz3, ncz4, ncz5, ncz6, ncz7, type="text")


### Now fit the zero inflated model
model.newcomer.comments.zi.1 <- zeroinfl(newcomer.comments ~ 1 | 
                                               visible + post.sub.top.minutes,  data=short.posts)
model.newcomer.comments.zi.2 <- zeroinfl(newcomer.comments ~ visible | 
                                               visible + post.sub.top.minutes,  data=short.posts)
model.newcomer.comments.zi.3 <- zeroinfl(newcomer.comments ~ visible + post.ama| 
                                               visible + post.sub.top.minutes,  data=short.posts)
model.newcomer.comments.zi.4 <- zeroinfl(newcomer.comments ~ visible + post.ama + post.sub.top.minutes | 
                                               visible + post.sub.top.minutes,  data=short.posts)
model.newcomer.comments.zi.5 <- zeroinfl(newcomer.comments ~ visible + post.ama + post.sub.top.minutes + post.hour + I(post.hour^2)  | 
                                               visible + post.sub.top.minutes,  data=short.posts)
model.newcomer.comments.zi.6 <- zeroinfl(newcomer.comments ~ visible + post.ama + post.sub.top.minutes + post.hour + I(post.hour^2) + weekend | 
                                               visible + post.sub.top.minutes,  data=short.posts)
model.newcomer.comments.zi.7 <- zeroinfl(newcomer.comments ~ visible + post.ama + post.sub.top.minutes + post.hour + I(post.hour^2) + weekend + post.flair| 
                                               visible + post.sub.top.minutes,  data=short.posts)

htmlreg(list(model.newcomer.comments.zi.1,
          model.newcomer.comments.zi.2,
          model.newcomer.comments.zi.3,
          model.newcomer.comments.zi.4,
          model.newcomer.comments.zi.5,
          model.newcomer.comments.zi.6,
          model.newcomer.comments.zi.7), type="text")


stargazer(model.newcomer.comments.zi.1,
          model.newcomer.comments.zi.2,
          model.newcomer.comments.zi.3,
          model.newcomer.comments.zi.4,
          model.newcomer.comments.zi.5,
          model.newcomer.comments.zi.6,
          model.newcomer.comments.zi.7, type="text")


##################################################
### HOW SHOULD THE DEPENDENT VARIABLES BE MODELED? 
### (COMMENT LEVEL ANALYSIS OF STICKY COMMENT EXPERIMENT)

summary(short.comments$visible)

ccv1 <- glmer(visible ~ 1 + (1 | link_id), data = short.comments, family = binomial, nAGQ = 0)
ccv2 <- glmer(visible ~ post.visible + (1 | link_id), data = short.comments, family = binomial)
ccv3 <- glmer(visible ~ post.visible + post.sub.top.minutes + (1 | link_id), data = short.comments, family = binomial, nAGQ = 0)
ccv4 <- glmer(visible ~ post.visible + post.sub.top.minutes + post.ama + (1 | link_id), data = short.comments, family = binomial, nAGQ = 0)
ccv5 <- glmer(visible ~ post.visible + post.sub.top.minutes + post.ama + post.flair + (1 | link_id), data = short.comments, family = binomial, nAGQ = 0)
ccv6 <- glmer(visible ~ post.visible + post.sub.top.minutes + post.ama + post.flair + toplevel + (1 | link_id), data = short.comments, family = binomial, nAGQ = 0)
ccv7 <- glmer(visible ~ post.visible + post.sub.top.minutes + post.ama + post.flair + toplevel + weekend + (1 | link_id), data = short.comments, family = binomial, nAGQ = 0)
ccv8 <- glmer(visible ~ post.visible + post.sub.top.minutes + post.ama + post.flair + toplevel + post.hour + I(post.hour^2) + (1 | link_id), data = short.comments, family = binomial, nAGQ = 0)

htmlreg(list(ccv1,ccv2,ccv3,ccv4,ccv5,ccv6,ccv7, ccv8))
#stargazer(ccv1,ccv2,ccv3,ccv4,ccv5,ccv6,ccv7, ccv8, type="text")

##### NOW TRY IT WITH a 3-level model
ccvri1 <- glmer(visible ~ 1 + (1 | post.flair/link_id), data = short.comments, family = binomial, nAGQ = 0)
ccvri2 <- glmer(visible ~ post.visible + (1 | post.flair/link_id), data = short.comments, family = binomial)
ccvri3 <- glmer(visible ~ post.visible + post.sub.top.minutes + (1 | post.flair/link_id), data = short.comments, family = binomial, nAGQ = 0)
ccvri4 <- glmer(visible ~ post.visible + post.sub.top.minutes + post.ama + (1 | post.flair/link_id), data = short.comments, family = binomial, nAGQ = 0)
ccvri5 <- glmer(visible ~ post.visible + post.sub.top.minutes + post.ama + toplevel + (1 | post.flair/link_id), data = short.comments, family = binomial, nAGQ = 0)
ccvri6 <- glmer(visible ~ post.visible + post.sub.top.minutes + post.ama + toplevel + weekend + (1 | post.flair/link_id), data = short.comments, family = binomial, nAGQ = 0)
ccvri7 <- glmer(visible ~ post.visible + post.sub.top.minutes + post.ama + toplevel + post.hour + I(post.hour^2) + (1 | post.flair/link_id), data = short.comments, family = binomial, nAGQ = 0)

htmlreg(list(ccvri1,ccvri2,ccvri3,ccvri4,ccvri5,ccvri6,ccvri7))
#stargazer(ccvri1,ccvri2,ccvri3,ccvri4,ccvri5,ccvri6,ccvri7, type="text")


