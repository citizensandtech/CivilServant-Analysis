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
library(rms)
library(ri2)
library(beepr)
library(xtable)

### ANALYSIS FOR THE PAPER PRE-REGISTERED AT https://osf.io/jhkcf/
### Preventing Harassment and Increasing Group Participation 
### Through Social Norms: A Field Experiment in Online Science Discussions
### J. Nathan Matias (August 2018)

rm(list=ls())
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#critical value for 3 comparisons is 2.398
critval.3 <- 2.398
critval = 1.96

#################################################
### LOAD DATASETS

### LOAD COMMENTS DATASET
all.comments <- read.csv("r_science_comments_science_sticky_09.24.2016.csv")
exs.comments= subset(all.comments, post.block.id!="nonama.block058")
exs.comments= subset(exs.comments, post.block.id!="nonama.block222")
exs.comments = subset(exs.comments, post.block.id!="nonama.block037")

exs.comments <- exs.comments[,c("visible", "author.prev.comments", "link_id", "post.ama", "post.treatment", "post.visible", "post.sub.top.minutes.ln", "post.block.id")]
exs.comments$visible.int <- as.numeric(exs.comments$visible == "True")



## CREATE DATASET OF NEWCOMER COMMENTS
newcomer.comments <- subset(exs.comments, author.prev.comments ==0)

newcomer.comments.nonama <- subset(newcomer.comments, post.ama==FALSE)
newcomer.comments.ama <- subset(newcomer.comments, post.ama==TRUE)


### LOAD POSTS DATASET
all.posts <- read.csv("r_science_experiment_1_posts.09.26.2016.csv")

all.posts <- all.posts[,c("newcomer.comments", "newcomer.comments.removed", "visible", "post.ama", "sub.top.minutes", "block.id", "created", "treatment", "weekend", "post.flair")]

#newcomer.comments ~ visible.bool + post.ama +  post.sub.top.minutes.ln + post.flair + weekend.bool + post.hour + I(post.hour^2) + TREAT

exs.posts <- all.posts
exs.posts$post.sub.top.minutes.ln <- log1p(exs.posts$sub.top.minutes)
exs.posts$datetime <-  as.POSIXct(exs.posts$created)
exs.posts$post.hour <- hour(exs.posts$datetime)

exs.posts$TREAT <- exs.posts$treatment

exs.posts <- subset(exs.posts, block.id!="nonama.block222")
exs.posts <- subset(exs.posts, block.id!="nonama.block058")
exs.posts <- subset(exs.posts, block.id!="nonama.block037")
exs.posts$block.id <- factor(exs.posts$block.id)
exs.posts$visible.bool <- exs.posts$visible=="True"
exs.posts$visible.int <- as.integer(exs.posts$visible.bool)
exs.posts$weekend.bool <- exs.posts$weekend =="True"

exs.posts.ama <- subset(exs.posts, post.ama==TRUE)
exs.posts.nonama <- subset(exs.posts, post.ama!=TRUE)


######### H1: AVERAGE TREATMENT EFFECT ON CHANCE OF COMPLYING WITH THE RULES
######### This is the analysis that was pre-registered
summary(h1.preregistered <- glmer(visible ~ post.visible + post.ama + post.sub.top.minutes.ln + post.treatment + (1 | link_id), data = newcomer.comments, family = binomial, nAGQ=40))

# ######## WITHOUT ANY ADJUSTMENT
# summary(h1.unadjusted <- glmer(visible ~ post.treatment + post.ama + (1 | link_id), data = newcomer.comments, family = binomial))
# summary(h1.adjusted <- glmer(visible ~ post.treatment + post.ama + post.visible + (1 | link_id), data = newcomer.comments, family = binomial))

# H1 Adjusted Models
summary(h1.unadjusted.nonama <- glmer(visible ~ post.treatment + (1 | link_id), data = subset(newcomer.comments, post.ama!=TRUE), family = binomial))
summary(h1.unadjusted.ama <- glmer(visible ~ post.treatment + (1 | link_id), data = subset(newcomer.comments, post.ama==TRUE), family = binomial))

###########################################
## POSTS ANALYSIS
###########################################

#### MODEL H2: The Number of Newcomer Comments removed Per Post
summary(h2.preregistered <- zeroinfl(newcomer.comments.removed ~ visible.bool + post.ama +  post.sub.top.minutes.ln + post.flair + weekend.bool + post.hour + I(post.hour^2) + TREAT | visible.bool + post.sub.top.minutes.ln, data=exs.posts))

#### H2 Unadjusted subgroup analyses

summary(h2.unadjusted.ama <- glm.nb(newcomer.comments.removed ~ TREAT, data=exs.posts.ama))
summary(h2.unadjusted.nonama <- glm.nb(newcomer.comments.removed ~ TREAT, data=exs.posts.nonama))

#### H2 randomization inference (nonama)
h2.nonama.dat <- exs.posts.nonama
h2.nonama.dat$block.id <- factor(h2.nonama.dat$block.id)
h2.nonama.dat$Z <- h2.nonama.dat$TREAT 
h2.nonama.dat$Y <- h2.nonama.dat$newcomer.comments.removed
h2.nonama.dat <- h2.nonama.dat[c("block.id", 'Y', 'Z')]

h2.nonama.declaration <-
  with(h2.nonama.dat,{
    declare_ra(
      blocks = h2.nonama.dat$block.id,
      N = nrow(h2.nonama.dat),
      block_m = with(h2.nonama.dat, tapply(Z, block.id, sum))
    )
  })

h2.nonama.ri2_out <- conduct_ri(
  Y ~ Z,
  sharp_hypothesis = 0,
  declaration = h2.nonama.declaration,
  data = h2.nonama.dat,
  sims=5000,
  progress_bar=TRUE,
  p = "upper"
)
h2.nonama.ri2_out

#plot(h2.nonama.ri2_out)
h2.nonama.ri2.pvalue <- summary(h2.nonama.ri2_out)[,'upper_p_value']
h2.nonama.ri2.estimate <- summary(h2.nonama.ri2_out)[,'estimate']
beep(sound = "treasure", expr = NULL)

#### H2 randomization inference (AMA)
h2.ama.dat <- exs.posts.ama
h2.ama.dat$block.id <- factor(h2.ama.dat$block.id)
h2.ama.dat$Z <- h2.ama.dat$TREAT 
h2.ama.dat$Y <- h2.ama.dat$newcomer.comments.removed
h2.ama.dat <- h2.ama.dat[c("block.id", 'Y', 'Z')]

h2.ama.declaration <-
  with(h2.ama.dat,{
    declare_ra(
      blocks = h2.ama.dat$block.id,
      N = nrow(h2.ama.dat),
      block_m = with(h2.ama.dat, tapply(Z, block.id, sum))
    )
  })

h2.ama.ri2_out <- conduct_ri(
  Y ~ Z,
  sharp_hypothesis = 0,
  declaration = h2.ama.declaration,
  data = h2.ama.dat,
  sims=5000,
  progress_bar=TRUE,
  p = "lower"
)
h2.ama.ri2_out

#plot(h2.nonama.ri2_out)
h2.ama.ri2.pvalue <- summary(h2.ama.ri2_out)[,'lower_p_value']
h2.ama.ri2.estimate <- summary(h2.ama.ri2_out)[,'estimate']
beep(sound = "treasure", expr = NULL)

#### MODEL H3: The Number of Newcomers Per Post
summary(h3.preregistered <- zeroinfl(newcomer.comments ~ visible.bool + post.ama +  post.sub.top.minutes.ln + post.flair + weekend.bool + post.hour + I(post.hour^2) + TREAT | visible.bool + post.sub.top.minutes.ln, data=exs.posts))

## H3 unadjusted subgroup analyses
summary(h3.unadjusted.ama <- glm.nb(newcomer.comments ~ TREAT, data=exs.posts.ama))
summary(h3.unadjusted.nonama <- glm.nb(newcomer.comments ~ TREAT, data=exs.posts.nonama))

#### H3 randomization inference (NONAMA)
h3.dat <- exs.posts.nonama
h3.dat$block.id <- factor(h3.dat$block.id)
h3.dat$Z <- exs.posts.nonama$TREAT 
h3.dat$Y <- exs.posts.nonama$newcomer.comments

h3.declaration <-
  with(h3.dat,{
    declare_ra(
      blocks = h3.dat$block.id,
      N = nrow(h3.dat),
      block_m = with(h3.dat, tapply(Z, block.id, sum))
    )
  })

h3.ri2_out <- conduct_ri(
  Y ~ Z,
  sharp_hypothesis = 0,
  declaration = h3.declaration,
  data = h3.dat,
  sims=5000,
  progress_bar=TRUE,
  p = "upper"
)
h3.ri2_out

#plot(h3.ri2_out)
h3.nonama.ri2.pvalue <- summary(h3.ri2_out)[,'upper_p_value']
h3.nonama.ri2.estimate <- summary(h3.ri2_out)[,'estimate']

beep(sound = "treasure", expr = NULL)

#### H3 randomization inference (AMA)
h3.ama.dat <- exs.posts.ama
h3.ama.dat$block.id <- factor(h3.ama.dat$block.id)
h3.ama.dat$Z <- h3.ama.dat$TREAT 
h3.ama.dat$Y <- h3.ama.dat$newcomer.comments
h3.ama.dat <- h3.ama.dat[c("block.id", 'Y', 'Z')]

h3.ama.declaration <-
  with(h3.ama.dat,{
    declare_ra(
      blocks = h3.ama.dat$block.id,
      N = nrow(h3.ama.dat),
      block_m = with(h3.ama.dat, tapply(Z, block.id, sum))
    )
  })

h3.ama.ri2_out <- conduct_ri(
  Y ~ Z,
  sharp_hypothesis = 0,
  declaration = h3.ama.declaration,
  data = h3.ama.dat,
  sims=10000,
  progress_bar=TRUE,
  p = "lower"
)
h3.ama.ri2_out

#plot(h3.ama.ri2_out)
h3.ama.ri2.pvalue <- summary(h3.ama.ri2_out)[,'lower_p_value']
h3.ama.ri2.estimate <- summary(h3.ama.ri2_out)[,'estimate']

beep(sound = "treasure", expr = NULL)


###########################################################
#################### ESTIMATE P VALUES ####################

h1.preregistered.p <- p.adjust(summary(h1.preregistered)$coefficients['post.treatment',][['Pr(>|z|)']], n=3, method="bonferroni")
h2.preregistered.p <- p.adjust(summary(h2.preregistered)$coefficients$count['TREAT',][['Pr(>|z|)']], n=3, method="bonferroni")
h3.preregistered.p <- p.adjust(summary(h3.preregistered)$coefficients$count['TREAT',][['Pr(>|z|)']], n=3, method="bonferroni")

adjusted.p.values <- data.frame(estimator=c("h1.preregistered",
                                            "h2.preregistered",
                                            "h3.preregistered",
                                            "h1.unadjusted.nonama",
                                            "h1.unadjusted.ama",
                                            "h2.unadjusted.nonama",
                                            "h2.unadjusted.ama",
                                            "h3.unadjusted.nonama",
                                            "h3.unadjusted.ama",
                                            "h2.nonama.ri",
                                            "h2.ama.ri",
                                            "h3.nonama.ri",
                                            "h3.ama.ri"), 
                                pvalue = c(
                                  summary(h1.preregistered)$coefficients['post.treatment',][['Pr(>|z|)']],
                                  summary(h2.preregistered)$coefficients$count['TREAT',][['Pr(>|z|)']], 
                                  summary(h3.preregistered)$coefficients$count['TREAT',][['Pr(>|z|)']],
                                  
                                  summary(h1.unadjusted.nonama)$coefficients['post.treatment',][['Pr(>|z|)']],
                                  summary(h1.unadjusted.ama)$coefficients['post.treatment',][['Pr(>|z|)']], # FAILS TEST
                                  
                                  summary(h2.unadjusted.nonama)$coefficients['TREAT',][['Pr(>|z|)']],
                                  summary(h2.unadjusted.ama)$coefficients['TREAT',][['Pr(>|z|)']],
                                  
                                  summary(h3.unadjusted.nonama)$coefficients['TREAT',][['Pr(>|z|)']],
                                  summary(h3.unadjusted.ama)$coefficients['TREAT',][['Pr(>|z|)']],
                                  h2.nonama.ri2.pvalue,
                                  h2.ama.ri2.pvalue,
                                  h3.nonama.ri2.pvalue,
                                  h3.ama.ri2.pvalue
                                )
)
adjusted.p.values$pvalue.bh <- p.adjust(adjusted.p.values$pvalue, method="BH")
adjusted.p.values


##########################################
### RESULTS GRAPHS AND TABLES
##########################################
### Pre-registered result
h1.preregistered.summary <- summary(h1.preregistered)
h1.preregistered.p


### MAIN RESULT
h1.summary <- summary(h1.unadjusted.nonama)

h1.a <- h1.summary$coefficients['post.treatment',][['Estimate']]  
h1.a.upr <- h1.summary$coefficients['post.treatment',][['Estimate']] + critval * h1.summary$coefficients['post.treatment',][['Std. Error']]
h1.a.lwr <- h1.summary$coefficients['post.treatment',][['Estimate']] - critval * h1.summary$coefficients['post.treatment',][['Std. Error']]

h1.effect <- data.frame(fit = h1.a, upr = h1.a.upr, lwr = h1.a.lwr)

h1.p <- adjusted.p.values[adjusted.p.values$estimator=="h1.unadjusted.nonama",]$pvalue

ggplot(h1.effect, aes("H1", fit)) +
  geom_hline(yintercept=0, color="#999999") +
  geom_point(size=2, colour=cbbPalette[2]) +
  geom_errorbar(aes(ymax=upr, ymin=lwr), width=0.2, size=1, colour=cbbPalette[2]) +
  ylim(-0.05,1) + 
  coord_flip() +
  #  geom_text(aes(label=sprintf("%1.2f%%", 100*fit)), vjust=1.5, #hjust=0.7, 
  #            size=7, color="#ffffff", fontface="bold") +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        title = element_text(hjust=0.2),
        axis.text = element_text(hjust=0, vjust = 0, angle=0),
        axis.title =  element_text(hjust=0, vjust = 0, angle=0)) +
  ylab(paste("Effect of posting rules on the log-odds of a newcomer comment to be removed\n",
             "by moderators in r/science community from 08/25/2016 - 09/23/2016.\n", 
             "n = ", nrow(newcomer.comments.nonama), " newcomers, ", length(unique(newcomer.comments.nonama$link_id)), " posts, in a random intercepts logistic regression (p=", 
             sprintf("%0.04f", h1.p), ").", sep="")) +
  annotate("text", label = "A. Effect on Log-Odds of Newcomer Rule Compliance",
           x = 1.4, y = 0.01, hjust = 0, size=6) +
  #  ggtitle("Posting Rules to Discussions Increases Newcomer Rule Compliance") +
  ggsave("illustrations/r_science_h1_newcomer_compliance.pdf", width = 8.5, height = 3, units = "in")

## Hypothesis 2
h2.summary <- summary(h2.unadjusted.nonama)
h2.a <- exp(h2.summary$coefficients['TREAT',][['Estimate']])

h2.a.upr <- exp(h2.summary$coefficients['TREAT',][['Estimate']] + critval * h2.summary$coefficients['TREAT',][['Std. Error']])
h2.a.lwr <- exp(h2.summary$coefficients['TREAT',][['Estimate']] - critval * h2.summary$coefficients['TREAT',][['Std. Error']])

h2.p <- adjusted.p.values[adjusted.p.values$estimator=="h2.unadjusted.nonama",]$pvalue
h2.effect <- data.frame(fit = h2.a, upr = h2.a.upr, lwr = h2.a.lwr)


ggplot(h2.effect, aes("H2", fit)) +
  geom_hline(yintercept=1, color="#999999") +
  geom_point(size=2, colour=cbbPalette[3]) +
  geom_errorbar(aes(ymax=upr, ymin=lwr), width=0.2, size=1, colour=cbbPalette[3]) +
  ylim(0.92,2.5) + 
  coord_flip() +
  #  geom_text(aes(label=sprintf("%1.2f%%", 100*fit)), vjust=1.5, #hjust=0.7, 
  #            size=7, color="#ffffff", fontface="bold") +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        title = element_text(hjust=0.2),
        axis.text = element_text(hjust=0, vjust = 0, angle=0),
        axis.title =  element_text(hjust=0, vjust = 0, angle=0)) +
  ylab(paste("Effect of posting rules on incidence rate of number of newcomer comments removed\n",
             "by moderators in r/science community from 08/25/2016 - 09/23/2016.\n", 
             "n = ", nrow(newcomer.comments.nonama), " newcomers, ", nrow(exs.posts.nonama), 
             " posts, in a negative binomial model (p = ",sprintf("%1.4f", h2.p),").", sep="")) +
  annotate("text", label = "C. Effect on Incidence Rate of Newcomer Comment Removals",
           x = 1.4, y = 1.01, hjust = 0, size=6) +
  #ggtitle("Posting Rules Increases the Rate of Newcomer Comment Removals") +
  ggsave("illustrations/r_science_h2_newcomers_removed.pdf", width = 8.5, height = 3, units = "in")

## Hypothesis 3
h3.summary <- summary(h3.unadjusted.nonama)
h3.a <- exp(h3.summary$coefficients['TREAT',][['Estimate']])
h3.a.upr <- exp(h3.summary$coefficients['TREAT',][['Estimate']] + critval * h3.summary$coefficients['TREAT',][['Std. Error']])
h3.a.lwr <- exp(h3.summary$coefficients['TREAT',][['Estimate']] - critval * h3.summary$coefficients['TREAT',][['Std. Error']])
h3.p <- p.adjust(h3.summary$coefficients['TREAT',][['Pr(>|z|)']], n=3, method="bonferroni")
h3.effect <- data.frame(fit = h3.a, upr = h3.a.upr, lwr = h3.a.lwr)


ggplot(h3.effect, aes("H3", fit)) +
  geom_hline(yintercept=1, color="#999999") +
  geom_point(size=2, colour=cbbPalette[4]) +
  geom_errorbar(aes(ymax=upr, ymin=lwr), width=0.2, size=1, colour=cbbPalette[4]) +
  ylim(0.92,2.5) + 
  coord_flip() +
  #  geom_text(aes(label=sprintf("%1.2f%%", 100*fit)), vjust=1.5, #hjust=0.7, 
  #            size=7, color="#ffffff", fontface="bold") +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(hjust=0.2, vjust = -5),
        axis.text = element_text(hjust=0, margin=margin(b=10, t=-10)),
        axis.title =  element_text(hjust=0, vjust = 0, angle=0)) +
  ylab(paste("Effect of posting rules on incidence rate of number of newcomer comments\n",
             "by moderators in r/science community from 08/25/2016 - 09/23/2016.\n", 
             "n = ", nrow(newcomer.comments.nonama), " newcomers, ", nrow(exs.posts.nonama), 
             " posts, in a negative binomial model (p < 0.001).", sep="")) +
  annotate("text", label = "B. Effect on Incidence Rate of Newcomer Comments",
           x = 1.4, y = 1.01, hjust = 0, size=6) +
  #ggtitle("Posting Rules Increases the Incidence Rate of Newcomer Comments") +
  ggsave("illustrations/r_science_h3_newcomer_count.pdf", width = 8.5, height = 3, units = "in")


##################################
#### PREDICT FITTED PROBABILITIES
#### FOR NEWCOMER COMMENT HYPOTHES (H1)

### BASELINE MODEL
baseline_prob = 1/(1+exp(-1 *(h1.summary$coefficients['(Intercept)',]['Estimate'] )))
baseline_prob
treat_prob = 1/(1+exp(-1 *(h1.summary$coefficients['(Intercept)',]['Estimate'] + 
                             h1.summary$coefficients['post.treatment',]['Estimate'])))
treat_prob

bp2 = 1/(1+exp(-1 *(h1.summary$coefficients['(Intercept)',]['Estimate'])))


h1.prob.effect <- treat_prob - baseline_prob
h1.prob.effect

##################################
#### OUTPUT LaTeX TABLES

#### Unadjusted H1 (nonama) for main paper

screenreg(h1.unadjusted.nonama,
          custom.coef.names = c("(Intercept)","TREAT"),
          custom.model.names= c("Rule Compliance"),
          label="table:rulecompliance")

texreg(h1.unadjusted.nonama,
       custom.coef.names = c("(Intercept)","TREAT"),
       custom.model.names= c("Rule Compliance"),
       label="table:rulecompliance")



#### PRE-REGISTERED ANALYSES
screenreg(list(h1.preregistered, h3.preregistered, h2.preregistered), include.deviance = TRUE,
          custom.coef.names = c("(Intercept)", "Post Visible", "Live Q\\&A (AMA)", "Minutes in Top 5", "TREAT",
                                "Count (Intercept)", "Count Post Visible", "Count Live Q\\&A (AMA)", "Count Minutes in Top 5", 
                                "Topic: animalsci", "Topic: anthropology", "Topic: Astronomy", 
                                "Topic: Biology", "Topic: Cancer", "Topic: Chemistry", "Topic: Compsci", 
                                "Topic: Earth Science", "Topic: Engineering", "Topic: Environment",
                                "Topic: Epidemiology", "Topic: Geology", "Topic: Health", "Topic: Math",
                                "Topic: Medicine", "Topic: Meta", "Topic: Nanotech", "Topic: Neuroscience",
                                "Topic: Paleontology", "Topic: Physics", "Topic: Psychology", "Topic: Sociology",
                                "Weekend", "Post Hour", "Post Hour ^2", "Treatment", 
                                "Zero: (Intercept)", "Zero: Post Visible", "Zero: Minutes in Top 5"),
          custom.model.names = c("Chance of Removal","Num Comments", "Num Comments Removed"),
          label="all.preregistered")

texreg(list(h1.preregistered, h3.preregistered, h2.preregistered), include.deviance = TRUE,
       custom.coef.names = c("(Intercept)", "Post Visible", "Live Q\\&A (AMA)", "Minutes in Top 5", "TREAT",
                             "Count (Intercept)", "Count Post Visible", "Count Live Q\\&A (AMA)", "Count Minutes in Top 5", 
                             "Topic: animalsci", "Topic: anthropology", "Topic: Astronomy", 
                             "Topic: Biology", "Topic: Cancer", "Topic: Chemistry", "Topic: Compsci", 
                             "Topic: Earth Science", "Topic: Engineering", "Topic: Environment",
                             "Topic: Epidemiology", "Topic: Geology", "Topic: Health", "Topic: Math",
                             "Topic: Medicine", "Topic: Meta", "Topic: Nanotech", "Topic: Neuroscience",
                             "Topic: Paleontology", "Topic: Physics", "Topic: Psychology", "Topic: Sociology",
                             "Weekend", "Post Hour", "Post Hour ^2", "Treatment", 
                             "Zero: (Intercept)", "Zero: Post Visible", "Zero: Minutes in Top 5"),
       custom.model.names = c("Chance of Removal","Num Comments", "Num Comments Removed"),
       label="results:preregistered")

texreg(list(h1.preregistered, h3.preregistered, h2.preregistered), include.deviance = TRUE,
       #          custom.coef.names = c("(Intercept)", "Treatment", "Live Q\\&A (AMA)", "Post Visible",  "ln Minutes in Top 5"),
       custom.model.names = c("Chance of Removal","Num Comments", "Num Comments Removed"),
       label="all.preregistered")


#### TABLE OF ANALYSES OF NUM COMMENTS AND NUM COMMENTS REMOVE

followup.analyses = data.frame(
  subgroup = c(
    "Preregistered", "Preregistered", "Preregistered","Articles", "Articles", "Articles", "Articles", "Articles",
    "Live Q&A", "Live Q&A", "Live Q&A", "Live Q&A", "Live Q&A"),
  dv =  c(
    "visible","num.comments","num.comments.removed",
    "visible","num.comments","num.comments","num.comments.removed","num.comments.removed",
    "visible","num.comments","num.comments","num.comments.removed","num.comments.removed"),
  estimator = c(
    "logistic", "Z Poisson", "Z Poisson",
    "logistic","NegBin", "RI", "NegBin", "RI",
    "logistic","NegBin", "RI", "NegBin", "RI"),
  estimates = c(
    #PRE-REGISTERED
    summary(h1.preregistered)$coefficients['post.treatment',][['Estimate']],
    summary(h3.preregistered)$coefficients$count['TREAT',][['Estimate']],
    summary(h2.preregistered)$coefficients$count['TREAT',][['Estimate']],
    #NON-AMA
    summary(h1.unadjusted.nonama)$coefficients['post.treatment',][['Estimate']],
    summary(h3.unadjusted.nonama)$coefficients['TREAT',][['Estimate']],
    h3.nonama.ri2.estimate,
    summary(h2.unadjusted.nonama)$coefficients['TREAT',][['Estimate']],
    h2.nonama.ri2.estimate,
    ## AMA
    summary(h1.unadjusted.ama)$coefficients['post.treatment',][['Estimate']],
    summary(h3.unadjusted.ama)$coefficients['TREAT',][['Estimate']],
    h3.ama.ri2.estimate,
    summary(h2.unadjusted.ama)$coefficients['TREAT',][['Estimate']],
    h2.ama.ri2.estimate
  ),
  stderr = c(#PRE-REGISTERED
    summary(h1.preregistered)$coefficients['post.treatment',][['Std. Error']],
    summary(h3.preregistered)$coefficients$count['TREAT',][['Std. Error']],
    summary(h2.preregistered)$coefficients$count['TREAT',][['Std. Error']],
    ### NON-AMA
    summary(h1.unadjusted.nonama)$coefficients['post.treatment',][['Std. Error']],
    summary(h3.unadjusted.nonama)$coefficients['TREAT',][['Std. Error']],
    NA,
    summary(h2.unadjusted.nonama)$coefficients['TREAT',][['Std. Error']],
    NA,
    ### AMA
    summary(h1.unadjusted.ama)$coefficients['post.treatment',][['Std. Error']],
    summary(h3.unadjusted.ama)$coefficients['TREAT',][['Std. Error']],
    NA,
    summary(h2.unadjusted.ama)$coefficients['TREAT',][['Std. Error']],
    NA
  ),
  pvalue = c(
    ### PREREGISTERED
    adjusted.p.values[adjusted.p.values$estimator=="h1.preregistered",]$pvalue,
    adjusted.p.values[adjusted.p.values$estimator=="h3.preregistered",]$pvalue,
    adjusted.p.values[adjusted.p.values$estimator=="h2.preregistered",]$pvalue,
    ### NONAMA
    adjusted.p.values[adjusted.p.values$estimator=="h1.unadjusted.nonama",]$pvalue,
    adjusted.p.values[adjusted.p.values$estimator=="h3.unadjusted.nonama",]$pvalue,
    adjusted.p.values[adjusted.p.values$estimator=="h3.nonama.ri",]$pvalue,
    adjusted.p.values[adjusted.p.values$estimator=="h2.unadjusted.nonama",]$pvalue,
    adjusted.p.values[adjusted.p.values$estimator=="h2.nonama.ri",]$pvalue,
    ### AMA
    adjusted.p.values[adjusted.p.values$estimator=="h1.unadjusted.ama",]$pvalue,
    adjusted.p.values[adjusted.p.values$estimator=="h3.unadjusted.ama",]$pvalue,
    adjusted.p.values[adjusted.p.values$estimator=="h3.ama.ri",]$pvalue,
    adjusted.p.values[adjusted.p.values$estimator=="h2.unadjusted.ama",]$pvalue,
    adjusted.p.values[adjusted.p.values$estimator=="h2.ama.ri",]$pvalue                     
  ),
  adjusted.pvalue = c(
    ### PREREGISTERED
    adjusted.p.values[adjusted.p.values$estimator=="h1.preregistered",]$pvalue.bh,
    adjusted.p.values[adjusted.p.values$estimator=="h3.preregistered",]$pvalue.bh,
    adjusted.p.values[adjusted.p.values$estimator=="h2.preregistered",]$pvalue.bh,
    ### NONAMA
    adjusted.p.values[adjusted.p.values$estimator=="h1.unadjusted.nonama",]$pvalue.bh,
    adjusted.p.values[adjusted.p.values$estimator=="h3.unadjusted.nonama",]$pvalue.bh,
    adjusted.p.values[adjusted.p.values$estimator=="h3.nonama.ri",]$pvalue.bh,
    adjusted.p.values[adjusted.p.values$estimator=="h2.unadjusted.nonama",]$pvalue.bh,
    adjusted.p.values[adjusted.p.values$estimator=="h2.nonama.ri",]$pvalue.bh,
    ### AMA
    adjusted.p.values[adjusted.p.values$estimator=="h1.unadjusted.ama",]$pvalue.bh,
    adjusted.p.values[adjusted.p.values$estimator=="h3.unadjusted.ama",]$pvalue.bh,
    adjusted.p.values[adjusted.p.values$estimator=="h3.ama.ri",]$pvalue.bh,
    adjusted.p.values[adjusted.p.values$estimator=="h2.unadjusted.ama",]$pvalue.bh,
    adjusted.p.values[adjusted.p.values$estimator=="h2.ama.ri",]$pvalue.bh                     
  )
)
followup.analyses
#adjusted.p.values

followup.analyses$estimates <- as.numeric(as.character(followup.analyses$estimates))

##ESTIMATES AND P VALUE TABLE
print(xtable(followup.analyses, digits=c(0,0,0,0,4,4,4,4), display=c("s","s","s","s", "f", "f", "f", "f")), include.rownames=FALSE)
##ESTIMATES AND P VALUE TABLE CAPTION
paste("Results of un-adjusted followup estimates for article discussions (newcomer comments = ",
      nrow(subset(newcomer.comments, post.ama==FALSE)),
      ", discussions = ",nrow(exs.posts.nonama),
      ") and Live Q&A discussions (newcomer comments = ",
      nrow(subset(newcomer.comments, post.ama==TRUE)),
      ", discussions = ",nrow(exs.posts.ama),
      "). RI estimates are one-tailed randomization inference tests. p-values are adjusted using the Benjamini-Hochberg method.", sep="") 


########################################
###### REPORT MEANS OF THE DVs
###### table posts:balance in the paper


print(xtable(aggregate(exs.posts.nonama[c('treatment','newcomer.comments', 'newcomer.comments.removed')], 
                       list(exs.posts.nonama$treatment),  mean)), , include.rownames=FALSE)

aggregate(exs.posts.ama[c('treatment','newcomer.comments', 'newcomer.comments.removed')], list(exs.posts.ama$treatment),  mean)

aggregate(newcomer.comments.nonama[c('post.treatment','visible.int')], list(newcomer.comments.nonama$post.treatment),  mean)


########################################
###### REPORT THE BALANCE OF THE SAMPLE
###### table posts:balance in the paper
CrossTable(exs.posts$visible, exs.posts$treatment, prop.c = FALSE, prop.r=FALSE,prop.t = FALSE, prop.chisq = FALSE)
exs.posts$visible.int <- as.integer(exs.posts$visible.bool)

aggregate(exs.posts[,c("treatment", "post.sub.top.minutes.ln", "visible.int")], list(exs.posts$treatment), mean)

balance.post.sub.top.minutes.ln <- t.test(post.sub.top.minutes.ln ~ TREAT, exs.posts)$p.value
balance.visible.int <- t.test(visible.int ~ TREAT, exs.posts)$p.value

########################################
## REMOVE EXTRANEOUS MODELS FROM MEMORY
########################################
rm(h1.preregistered, h2.preregistered, h3.preregistered)
rm(h1.unadjusted.ama, h1.unadjusted.nonama)
rm(h2.unadjusted.ama, h2.unadjusted.nonama)
rm(h3.unadjusted.ama, h3.unadjusted.nonama)
rm(h1.preregistered.summary, h1.summary)
rm(h2.nonama.declaration, h2.ama.declaration)
rm(h3.declaration, h3.ama.declaration)
rm(h2.ama.ri2_out, h3.ama.ri2_out, h2.nonama.ri2_out, h3.ri2_out)
##################################################################
### LOAD PRE-EXPERIMENT OBSERVATIONAL DATA FOR EXPLAINING CONTEXT
##################################################################
pre.posts <- read.csv("../../outputs/r_science_posts_2016.07.04_04.45.23-2016.08.01_22.14.00.csv")

## obesity discussion example from the paper
obesity.discussion.total <- subset(pre.posts, id=="4ui494")$num.comments
obesity.discussion.removed <- subset(pre.posts, id=="4ui494")$num.comments.removed

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

## summary statistics 
max(pre.posts$created_utc) 

min(pre.posts$created_utc)

signif(sum(pre.posts$newcomer.comments.removed) / as.numeric(max(pre.posts$created_utc)- min(pre.posts$created_utc), units = "days"), 3)

signif(sum(pre.posts$newcomer.comments) / as.numeric(max(pre.posts$created_utc)- min(pre.posts$created_utc), units = "days"), 3)

sum(pre.posts$num.comments.removed) / as.numeric(max(pre.posts$created_utc)- min(pre.posts$created_utc), units = "days")

nrow(pre.posts) / as.numeric(max(pre.posts$created_utc)- min(pre.posts$created_utc), units = "days")

signif(sum(pre.posts$newcomer.comments) / sum(pre.posts$num_comments)*100, 3)

## percentage of removed newcomer comments of all removed
signif(sum(pre.posts$newcomer.comments.removed) / sum(pre.posts$num.comments.removed)*100, 3)

### OUTPUT  TO FILE
rdata_filename <- paste("r_science_experiment_results_", format(min(exs.posts$datetime),"%m.%d.%Y"), "_",
                        format(max(exs.posts$datetime),"%m.%d.%Y"), "_02.01.2019.RData", sep="")
save.image(rdata_filename)