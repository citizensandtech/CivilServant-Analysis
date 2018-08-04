library(ggplot2)
library(lubridate)
library(stargazer)
library(texreg)
library(lme4)
library(lmerTest)
library(gmodels) # Contains CrossTable
library(pscl)
library(data.table)
library(boot)
library(MASS)

### ANALYSIS FOR THE PAPER PRE-REGISTERED AT https://osf.io/jhkcf/
### Preventing Harassment and Increasing Group Participation 
### Through Social Norms: A Field Experiment in Online Science Discussions
### J. Nathan Matias (August 2018)

rm(list=ls())
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#critical value for 3 comparisons is 2.398
critval.3 <- 2.398

#################################################
### LOAD DATASETS

### LOAD COMMENTS DATASET
exs.comments <- read.csv("experiments/r_science_comments_science_sticky_09.24.2016.toptime.csv")
exs.comments= subset(exs.comments, post.block.id!="nonama.block058")
exs.comments= subset(exs.comments, post.block.id!="nonama.block222")
exs.comments = subset(exs.comments, post.block.id!="nonama.block037")

exs.comments$datetime <-  as.POSIXct(exs.comments$created)
exs.comments$hour <- hour(exs.comments$datetime)
exs.comments$post.ama <- exs.comments$post.ama=="True"

exs.comments$post.datetime <- as.POSIXct(exs.comments$post.created)
exs.comments$post.hour <- hour(exs.comments$post.datetime)

exs.comments$post.sub.top.minutes.ln <- log1p(exs.comments$post.sub.top.minutes)
exs.comments$post.sub.top.gap.sum.ln <- log1p(exs.comments$post.sub.top.gap.sum)

## CREATE DATASET OF NEWCOMER COMMENTS
newcomer.comments <- subset(exs.comments, author.prev.comments ==0)

### LOAD POSTS DATASET
all.posts <- read.csv("experiments/r_science_experiment_1_posts.09.26.2016.csv")
exs.posts <- all.posts
exs.posts$post.sub.top.minutes.ln <- log1p(exs.posts$sub.top.minutes)
exs.posts$datetime <-  as.POSIXct(exs.posts$created)
exs.posts$post.hour <- hour(exs.posts$datetime)

exs.posts$nonreply.newcomer.comments <- exs.posts$newcomer.comments - exs.posts$newcomer.comments.experiment.replies
exs.posts$TREAT <- exs.posts$treatment
exs.posts$post.ama <- exs.posts$post.ama=="True"

exs.posts <- subset(exs.posts, block.id!="nonama.block222")
exs.posts <- subset(exs.posts, block.id!="nonama.block058")
exs.posts <- subset(exs.posts, block.id!="nonama.block037")
exs.posts$visible.bool <- exs.posts$visible=="True"
exs.posts$weekend.bool <- exs.posts$weekend =="True"

######### H1: AVERAGE TREATMENT EFFECT ON CHANCE OF COMPLYING WITH THE RULES
ccv5 <- glmer(visible ~ post.visible + post.ama + post.sub.top.minutes.ln + post.treatment + (1 | link_id), data = newcomer.comments, family = binomial, nAGQ=20)
summary(ccv5)
stargazer(ccv5, type="text", star.cutoffs = c(0.05, 0.01, 0.001), ci=c(TRUE))
texreg(list(ccv5), include.deviance = TRUE)

## FOLLOWUP: NOW WITHOUT ADJUSTING FOR THE ALGORITHM
summary(ccv6 <- glmer(visible ~ post.visible + post.ama + post.treatment + (1 | link_id), data = newcomer.comments, family = binomial, nAGQ=20))
summary(ccv6)
texreg(list(ccv6), include.deviance = TRUE,
       custom.coef.names = c("(Intercept)", "Post Visible", "Live Q\\&A (AMA)", "Treatment"),
       custom.model.names = c("Final Model"),
       label="h1:results:noalgo")

##################################
#### ILLUSTRATE EFFECT WITH ERROR BARS
#### FOR NEWCOMER COMMENT HYPOTHES (H1)

h1.summary <- summary(ccv5)

h1.a <- h1.summary$coefficients['post.treatment',][['Estimate']]  
h1.a.upr <- h1.summary$coefficients['post.treatment',][['Estimate']] + critval.3 * h1.summary$coefficients['post.treatment',][['Std. Error']]
h1.a.lwr <- h1.summary$coefficients['post.treatment',][['Estimate']] - critval.3 * h1.summary$coefficients['post.treatment',][['Std. Error']]

h1.effect <- data.frame(fit <- h1.a, upr <- h1.a.upr, lwr <- h1.a.lwr)

h1.p <- p.adjust(h1.summary$coefficients['post.treatment',][['Pr(>|z|)']], n=3, method="bonferroni")

h1.p.visible <- p.adjust(h1.summary$coefficients['post.visibleTrue',][['Pr(>|z|)']], n=3, method="bonferroni")
h1.p.ama <- p.adjust(h1.summary$coefficients['post.amaTRUE',][['Pr(>|z|)']], n=3, method="bonferroni")
h1.p.top.minutes.ln <- p.adjust(h1.summary$coefficients['post.sub.top.minutes.ln',][['Pr(>|z|)']], n=3, method="bonferroni")


ggplot(h1.effect, aes("H1", fit)) +
  geom_point(size=2, colour=cbbPalette[2]) +
  geom_errorbar(aes(ymax=upr, ymin=lwr), width=0.2, size=1, colour=cbbPalette[2]) +
  ylim(0,1) + 
  coord_flip() +
  geom_text(aes(label=sprintf("%1.2f%%", 100*fit)), vjust=1.5, #hjust=0.7, 
            size=7, color="#ffffff", fontface="bold") +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        title = element_text(hjust=0.2),
        axis.text = element_text(hjust=0, vjust = 0, angle=0),
        axis.title =  element_text(hjust=0, vjust = 0, angle=0)) +
  ylab(paste("Effect of posting rules on the log-odds of a newcomer comment to be removed\n",
             "by moderators in r/science community from 08/25/2016 - 09/23/2016.\n", 
             "n = ", nrow(newcomer.comments), " newcomers, ", nrow(exs.posts), " posts, in a random intercepts binomial model (p=", 
             sprintf("%0.03f", h1.p), ").", sep="")) +
  annotate("text", label = "A. Effect on Log-Odds of Newcomer Rule Compliance",
           x = 1.4, y = 0, hjust = 0, size=6) +
  #  ggtitle("Posting Rules to Discussions Increases Newcomer Rule Compliance") +
  ggsave("illustrations/r_science_h1_newcomer_compliance.pdf", width = 8.5, height = 3, units = "in")


##################################
#### PREDICT FITTED PROBABILITIES
#### FOR NEWCOMER COMMENT HYPOTHES (H1)

h1.summary$coefficients['post.treatment',]['Estimate']

### BASELINE MODEL
baseline_prob = 1/(1+exp(-1 *(h1.summary$coefficients['(Intercept)',]['Estimate'] + 
                                h1.summary$coefficients['post.visibleTrue',]['Estimate'])))
baseline_prob
treat_prob = 1/(1+exp(-1 *(h1.summary$coefficients['(Intercept)',]['Estimate'] + 
                             h1.summary$coefficients['post.visibleTrue',]['Estimate'] + 
                             h1.summary$coefficients['post.treatment',]['Estimate'])))
treat_prob

h1.prob.effect <- treat_prob - baseline_prob
h1.prob.effect

##################################
#### OUTPUT LaTeX TABLE
#### FOR NEWCOMER COMMENT HYPOTHES (H1)

texreg(list(ccv5), include.deviance = TRUE,
       custom.coef.names = c("(Intercept)", "Post Visible", "Live Q\\&A (AMA)", "Minutes in Top 5", "Treatment"),
       custom.model.names = c("Final Model"),
       caption=paste("Posting rules caused the chance that newcomer comments would comply with the rules to increase by ",
                     toString(signif(h1.effect*100, 3)),
                     " percentage points. Results of a random-intercepts logistic regression on data from an experiment in r/science with ",
                     toString(length(unique(newcomer.comments$link_id))),
                     " discussions and ",toString(prettyNum(nrow(newcomer.comments),big.mark=",")), " comments.", 
                     sep=""), label="h1:results")


###########################################
## POSTS ANALYSIS
###########################################
## SUMMARY STATISTICS

hist(exs.posts$post.sub.top.minutes.ln)
summary(exs.posts$post.ama)
summary(exs.posts$weekend)
summary(exs.posts$post.flair)
hist(exs.posts$post.hour)


#### MODEL H2: The Number of Newcomer Comments removed Per Post
summary(zcm2 <- zeroinfl(newcomer.comments.removed ~ visible.bool + post.ama +  post.sub.top.minutes.ln + post.flair + weekend.bool + post.hour + I(post.hour^2) + TREAT | visible.bool + post.sub.top.minutes.ln, data=exs.posts))

#### MODEL H3: The Number of Newcomers Per Post
summary(zcm3 <- zeroinfl(newcomer.comments ~ visible.bool + post.ama +  post.sub.top.minutes.ln + post.flair + weekend.bool + post.hour + I(post.hour^2) + TREAT | visible.bool + post.sub.top.minutes.ln, data=exs.posts))

stargazer(zcm2, zcm3, type="text", star.cutoffs = c(0.05, 0.01, 0.001), ci=c(TRUE, TRUE))
screenreg(list(zcm2, zcm3))
screenreg(list(zcm2, zcm3),       
          custom.coef.names = c("(Intercept)", "Post Visible", "Live Q\\&A (AMA)", "Minutes in Top 5", 
                                "Topic: animalsci", "Topic: anthropology", "Topic: Astronomy", 
                                "Topic: Biology", "Topic: Cancer", "Topic: Chemistry", "Topic: Compsci", 
                                "Topic: Earth Science", "Topic: Engineering", "Topic: Environment",
                                "Topic: Epidemiology", "Topic: Geology", "Topic: Health", "Topic: Math",
                                "Topic: Medicine", "Topic: Meta", "Topic: Nanotech", "Topic: Neuroscience",
                                "Topic: Paleontology", "Topic: Physics", "Topic: Psychology", "Topic: Sociology",
                                "Weekend", "Post Hour", "Post Hour ^2", "Treatment", 
                                "Zero: (Intercept)", "Zero: Post Visible", "Zero: Minutes in Top 5"),
          custom.model.names = c("Newcomer Comments Removed", "Newcomer Comment Count")
)

texreg(list(zcm2, zcm3),       
          custom.coef.names = c("(Intercept)", "Post Visible", "Live Q\\&A (AMA)", "Minutes in Top 5", 
                                "Topic: animalsci", "Topic: anthropology", "Topic: Astronomy", 
                                "Topic: Biology", "Topic: Cancer", "Topic: Chemistry", "Topic: Compsci", 
                                "Topic: Earth Science", "Topic: Engineering", "Topic: Environment",
                                "Topic: Epidemiology", "Topic: Geology", "Topic: Health", "Topic: Math",
                                "Topic: Medicine", "Topic: Meta", "Topic: Nanotech", "Topic: Neuroscience",
                                "Topic: Paleontology", "Topic: Physics", "Topic: Psychology", "Topic: Sociology",
                                "Weekend", "Post Hour", "Post Hour ^2", "Treatment", 
                                "Zero: (Intercept)", "Zero: Post Visible", "Zero: Minutes in Top 5"),
          custom.model.names = c("Newcomer Comments Removed", "Newcomer Comment Count")
)

### NOW WITHOUT ADJUSTING FOR THE ALGORITHM
summary(zcm2.a <- zeroinfl(newcomer.comments.removed ~ visible.bool + post.ama + post.flair + weekend.bool + post.hour + I(post.hour^2) + TREAT | visible.bool, data=exs.posts))
summary(zcm3.a <- zeroinfl(newcomer.comments ~ visible.bool + post.ama + post.flair + weekend.bool + post.hour + I(post.hour^2) + TREAT | visible.bool, data=exs.posts))
texreg(list(zcm2.a, zcm3.a),       
       custom.coef.names = c("(Intercept)", "Post Visible", "Live Q\\&A (AMA)",
                             "Topic: animalsci", "Topic: anthropology", "Topic: Astronomy", 
                             "Topic: Biology", "Topic: Cancer", "Topic: Chemistry", "Topic: Compsci", 
                             "Topic: Earth Science", "Topic: Engineering", "Topic: Environment",
                             "Topic: Epidemiology", "Topic: Geology", "Topic: Health", "Topic: Math",
                             "Topic: Medicine", "Topic: Meta", "Topic: Nanotech", "Topic: Neuroscience",
                             "Topic: Paleontology", "Topic: Physics", "Topic: Psychology", "Topic: Sociology",
                             "Weekend", "Post Hour", "Post Hour ^2", "Treatment", 
                             "Zero: (Intercept)", "Zero: Post Visible"),
       custom.model.names = c("Newcomer Comments Removed", "Newcomer Comment Count")
)


#### GENERATE CONFIDENCE INTERVALS AND PLOTS
## USE IRR, which is exp(coefficient +- confint)
## Hypothesis 2
h2.summary <- summary(zcm2)
h2.a <- exp(h2.summary$coefficients$count['TREAT',][['Estimate']])
h2.a.upr <- exp(h2.summary$coefficients$count['TREAT',][['Estimate']] + critval.3 * h2.summary$coefficients$count['TREAT',][['Std. Error']])
h2.a.lwr <- exp(h2.summary$coefficients$count['TREAT',][['Estimate']] - critval.3 * h2.summary$coefficients$count['TREAT',][['Std. Error']])
h2.p <- p.adjust(h2.summary$coefficients$count['TREAT',][['Pr(>|z|)']], n=3, method="bonferroni")
h2.effect <- data.frame(fit <- h2.a, upr <- h2.a.upr, lwr <- h2.a.lwr)

ggplot(h2.effect, aes("H2", fit)) +
  geom_point(size=2, colour=cbbPalette[3]) +
  geom_errorbar(aes(ymax=upr, ymin=lwr), width=0.2, size=1, colour=cbbPalette[3]) +
  ylim(1,1.5) + 
  coord_flip() +
  geom_text(aes(label=sprintf("%1.2f%%", 100*fit)), vjust=1.5, #hjust=0.7, 
            size=7, color="#ffffff", fontface="bold") +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        title = element_text(hjust=0.2),
        axis.text = element_text(hjust=0, vjust = 0, angle=0),
        axis.title =  element_text(hjust=0, vjust = 0, angle=0)) +
  ylab(paste("Effect of posting rules on incidence rate of number of newcomer comments removed\n",
             "by moderators in r/science community from 08/25/2016 - 09/23/2016.\n", 
             "n = ", nrow(newcomer.comments), " newcomers, ", nrow(exs.posts), " posts, in a zero-inflated poisson model (p=", 
             sprintf("%0.03f", h2.p), ").", sep="")) +
  annotate("text", label = "C. Effect on Incidence Rate of Newcomer Comment Removals",
           x = 1.4, y = 1, hjust = 0, size=6) +
  #ggtitle("Posting Rules Increases the Rate of Newcomer Comment Removals") +
  ggsave("illustrations/r_science_h2_newcomers_removed.pdf", width = 8.5, height = 3, units = "in")

## Hypothesis 3
h3.summary <- summary(zcm3)
h3.a <- exp(h3.summary$coefficients$count['TREAT',][['Estimate']])
h3.a.upr <- exp(h3.summary$coefficients$count['TREAT',][['Estimate']] + critval.3 * h3.summary$coefficients$count['TREAT',][['Std. Error']])
h3.a.lwr <- exp(h3.summary$coefficients$count['TREAT',][['Estimate']] - critval.3 * h3.summary$coefficients$count['TREAT',][['Std. Error']])
h3.p <- p.adjust(h3.summary$coefficients$count['TREAT',][['Pr(>|z|)']], n=3, method="bonferroni")
h3.effect <- data.frame(fit <- h3.a, upr <- h3.a.upr, lwr <- h3.a.lwr)


ggplot(h3.effect, aes("H3", fit)) +
  geom_point(size=2, colour=cbbPalette[4]) +
  geom_errorbar(aes(ymax=upr, ymin=lwr), width=0.2, size=1, colour=cbbPalette[4]) +
  ylim(1,1.5) + 
  coord_flip() +
  geom_text(aes(label=sprintf("%1.2f%%", 100*fit)), vjust=1.5, #hjust=0.7, 
            size=7, color="#ffffff", fontface="bold") +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(hjust=0.2, vjust = -5),
        axis.text = element_text(hjust=0, margin=margin(b=10, t=-10)),
        axis.title =  element_text(hjust=0, vjust = 0, angle=0)) +
  ylab(paste("Effect of posting rules on incidence rate of number of newcomer comments\n",
             "by moderators in r/science community from 08/25/2016 - 09/23/2016.\n", 
             "n = ", nrow(newcomer.comments), " newcomers, ", nrow(exs.posts), " posts, in a zero-inflated poisson model (p < ", 
             sprintf("%0.03f", h3.p), ").", sep="")) +
  annotate("text", label = "B. Effect on Incidence Rate of Newcomer Comments",
           x = 1.4, y = 1, hjust = 0, size=6) +
  #ggtitle("Posting Rules Increases the Incidence Rate of Newcomer Comments") +
  ggsave("illustrations/r_science_h3_newcomer_count.pdf", width = 8.5, height = 3, units = "in")

########################################
###### CHECK THE BALANCE OF THE SAMPLE
summary(glm(treatment ~ post.flair + post.sub.top.minutes.ln, family="binomial", data=exs.posts))

summary(glm(treatment ~ factor(weekday), family="binomial", data=subset(exs.posts, post.ama==TRUE)))


########################################
## OUTPUT RESULTS TO RData File For Paper
########################################
rm(ccv1,ccv2,ccv3, ccv4, ccv5)
rm(m1,m2,mmmm1, nb1,nb2,nb3, occv1, occv2, occv3, occv4, occv5, occv6)
rm(ozcm1, ozcm2, ozcm3, ozcm4, ozcrm1, ozcrm2, ozcrm3, ozcrm4)
rm(tcnb, tcnb1, tcnb2, tcnb3, tcnb4, tcnb5, tcnb6)
rm(zcm1, zcm2, zcm3, zcm4)
rm(nc)

all.posts$media <- NULL
exs.posts$media <- NULL
all.posts$media_embed <- NULL
exs.posts$media_embed <- NULL
all.posts$selftext <- NULL 
exs.posts$selftext <- NULL 
all.posts$selftext_html <- NULL
exs.posts$selftext_html <- NULL
all.posts$permalink <- NULL
exs.posts$permalink <- NULL
all.posts$user_repots <- NULL
exs.posts$user_reports <- NULL
all.posts$mod_reports <- NULL
exs.posts$mod_reports <- NULL
all.posts$url <- NULL
exs.posts$url <- NULL
all.posts$clicked <- NULL
exs.posts$clicked <- NULL
all.posts$suggested_sort <- NULL
exs.posts$suggested_sort <- NULL
all.posts$archived <- NULL
exs.posts$archived <- NULL
newcomer.comments$post.badpost <- NULL
exs.comments$post.badpost <- NULL

##################################################################
### LOAD PRE-EXPERIMENT OBSERVATIONAL DATA FOR EXPLAINING CONTEXT
##################################################################
pre.posts <- read.csv("~/Documents/github/CivilServant-Analysis/outputs/r_science_posts_2016.07.04_04.45.23-2016.08.01_22.14.00.csv")
pre.posts$title <- NULL
pre.posts$approved_by <- NULL
pre.posts$banned_by <- NULL
pre.posts$archived <- NULL
pre.posts$clicked <- NULL
pre.posts$distinguished <- NULL
pre.posts$domain <- NULL
pre.posts$downs <- NULL
pre.posts$experiment.day <- NULL
pre.posts$experiment.day.minutes <- NULL
pre.posts$experiment.day.next <- NULL
pre.posts$hidden <- NULL
pre.posts$media <- NULL
pre.posts$media_embed <- NULL
pre.posts$mod_reports <- NULL
pre.posts$locked <- NULL
pre.posts$over_18 <- NULL
pre.posts$permalink <- NULL
pre.posts$preview <- NULL
pre.posts$quarantine <- NULL
pre.posts$report_reasons <- NULL
pre.posts$saved <- NULL
pre.posts$secure_media <- NULL
pre.posts$secure_media_embed <- NULL
pre.posts$stickied <- NULL
pre.posts$thumbnail <- NULL
pre.posts$user_reports <- NULL
pre.posts$visited <- NULL
pre.posts$created_utc <- as.POSIXct(pre.posts$created_utc)
pre.posts$created <- NULL
pre.posts$author <- NULL
pre.posts$author_flair_css_class <- NULL
pre.posts$from <- NULL
pre.posts$from_id <- NULL
pre.posts$from_kind <- NULL
pre.posts$author_flair_text <- NULL
pre.posts$X <- NULL
pre.posts$edited <- NULL
pre.posts$gilded <- NULL
pre.posts$hide_score <- NULL
pre.posts$is_self <- NULL
pre.posts$id <- NULL
pre.posts$url <- NULL
pre.posts$score <- NULL
pre.posts$subreddit <- NULL
pre.posts$saved <- NULL
pre.posts$post.badpost <- NULL
pre.posts$link_flair_css_class <- NULL
pre.posts$link_flair_text <- NULL
pre.posts$likes <- NULL
pre.posts$name <- NULL
pre.posts$post_hint <- NULL
pre.posts$removal_reason <- NULL
pre.posts$selftext <- NULL
pre.posts$selftext_html <- NULL
pre.posts$suggested_sort <- NULL
pre.posts$ups <- NULL
pre.posts$url <- NULL


## percentage of newcomer comments
max(pre.posts$created_utc) 

min(pre.posts$created_utc)

signif(sum(pre.posts$newcomer.comments.removed) / as.numeric(max(pre.posts$created_utc)- min(pre.posts$created_utc), units = "days"), 3)

signif(sum(pre.posts$newcomer.comments) / as.numeric(max(pre.posts$created_utc)- min(pre.posts$created_utc), units = "days"), 3)


sum(pre.posts$num.comments.removed) / as.numeric(max(pre.posts$created_utc)- min(pre.posts$created_utc), units = "days")

nrow(pre.posts) / as.numeric(max(pre.posts$created_utc)- min(pre.posts$created_utc), units = "days")


signif(sum(pre.posts$newcomer.comments) / sum(pre.posts$num_comments)*100, 3)

## percentage of removed newcomer comments of a ll removed
signif(sum(pre.posts$newcomer.comments.removed) / sum(pre.posts$num.comments.removed)*100, 3)


### OUTPUT  TO FILE
rdata_filename <- paste("r_science_experiment_results_", format(min(exs.posts$datetime),"%m.%d.%Y"), "-",
                        format(max(exs.posts$datetime),"%m.%d.%Y"), ".RData", sep="")
save.image(rdata_filename)
