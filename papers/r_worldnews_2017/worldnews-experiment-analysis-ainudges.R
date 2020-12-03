library(ggplot2)
library(lubridate)
library(stargazer)
library(texreg)
library(lme4)
library(lmerTest)
library(gmodels) # Contains CrossTable
library(effects)
library(rms)
library(MASS)
library(pscl)
require(utils)
library(merTools)
library(MASS)
library(data.table)


rm(list=ls())

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## TWO-SIDED CRITICAL Z-VALUES FOR BONFERRONI ADJUSTMENTS ACQUIRED FROM:
## https://people.ucsc.edu/~dgbonett/docs/psyc204/204Tables.pdf


####################################################################
#### EXPERIMENT COMMENTS
####################################################################
comments <- read.csv("datasets/r_worldnews_comments_02.18.2017.csv")

#### LABEL WHICH BLOCK GROUP AN ENTRY IS PART OF
comments$after.score.change <- comments$post.assign.number > 156
comments$sub.original.study <- comments$post.assign.number <= 840
comments$sub.before.notified <- comments$post.assign.number <=1032
comments$sub.after.notified <- comments$post.assign.number > 1044
comments$sub.after.300 <- comments$post.assign.number > 744

comments$datetime <- as.POSIXct(comments$created_utc,  origin = "1960-01-01")
comments$hour <- hour(comments$created)
comments$hour.f <- factor(comments$hour)
comments$weekday <- factor(wday(as.Date(comments$created)))
comments$weekend <- wday(as.Date(comments$created))>5
comments$includes.links <- comments$link_count > 0

comments$treatment.a <- comments$post.treatment == 1
comments$treatment.b <- comments$post.treatment == 2
comments$post.comments.ln <- log1p(comments$post.comments)

## REMOVE SPOILED EXPERIMENT BLOCKS
paper.comments <- subset(comments, (after.score.change==TRUE & post.block.id!="block.a.39"))

rm(comments)

####################################################################
#### EXPERIMENT POSTS
####################################################################
ranked.posts <- read.csv("datasets/r_worldnews_posts_ranked_with_median_score_at_max_position_02.18.2017.csv")

### LABEL EXPERIMENT BLOCKS FOR KEY PERIODS IN THE STUDY
ranked.posts$after.score.change <- ranked.posts$post.assign.number > 156
ranked.posts$sub.original.study <- ranked.posts$post.assign.number <= 840
ranked.posts$sub.before.notified <- ranked.posts$post.assign.number <=1032
ranked.posts$sub.after.notified <- ranked.posts$post.assign.number > 1044
ranked.posts$sub.after.300 <- ranked.posts$post.assign.number > 744


ranked.posts$treatment.a <- ranked.posts$post.treatment == 1
ranked.posts$treatment.b <- ranked.posts$post.treatment == 2
ranked.posts$hour <- hour(ranked.posts$created)
ranked.posts$post.weekend <- wday(as.Date(ranked.posts$created))>5

ranked.posts$zero.rank <- ranked.posts$max.rank.300==0
ranked.posts$zero.flair <- ranked.posts$link_flair_css==""

paper.posts <- subset(ranked.posts, (after.score.change==TRUE & post.block.id!="block.a.39"))
paper.posts.300 <- subset(ranked.posts, (sub.after.300==TRUE & post.block.id!="block.a.39"))


############################
### COMMENTS ANALYSIS

ec2a <- robcov(lrm(includes.links ~ treatment.a + treatment.b, x=T, y=T,data=paper.comments), cluster=paper.comments$link_id)
ec2 <- robcov(lrm(includes.links ~ post.visible + treatment.a + treatment.b, x=T, y=T,data=paper.comments), cluster=paper.comments$link_id)

ec2.treatment.a.pvalue <- p.adjust(.0014, method="bonferroni", n=2)
ec2.treatment.b.pvalue <- p.adjust(0.0158, method="bonferroni", n=2)

screenreg(list(ec2a, ec2), digits=3,  label="table:comment.effect", 
          custom.model.names=c("Base Model","Main Model"), 
          caption="Encouraging fact-checking at the top of an online discussion 
          of unreliable news increased the chance that a comment ")


texreg(list(ec2a, ec2), digits=3, label="table:comment.effect", 
       custom.model.names=c("Without Adjustment","Main Model"),
       custom.coef.names = c("Intercept", 
                             "Encourage Fact-Checking","Encourage Fact-Checking + Voting", "Article Permitted"),
       caption="Encouraging fact-checking at the top of an online discussion
       of tabloid news increased the chance that a comment would
       include links to further evidence. Standard errors in this logistic regression are adjusted using the maximum-likelihood Huber-White method for comments clustered within discussions that received the treatment.")


ec2$stderrs <- sqrt(diag( vcov(ec2)))

fp2.0 <- 1/(1+exp(-1*(ec2$coefficients['Intercept'] + ec2$coefficients['post.visible=True'])))
fp2.a <- 1/(1+exp(-1*(ec2$coefficients['Intercept'] + ec2$coefficients['post.visible=True'] + ec2$coefficients['treatment.a'])))
fp2.b <- 1/(1+exp(-1*(ec2$coefficients['Intercept'] + ec2$coefficients['post.visible=True'] + ec2$coefficients['treatment.b'])))


fp2.a.upr <- 1/(1+exp(-1*(ec2$coefficients['Intercept'] + ec2$coefficients['post.visible=True'] + ec2$coefficients['treatment.a'] + 2.24*ec2$stderrs['treatment.a'])))
fp2.a.lwr <- 1/(1+exp(-1*(ec2$coefficients['Intercept'] + ec2$coefficients['post.visible=True'] + ec2$coefficients['treatment.a'] - 2.24*ec2$stderrs['treatment.a'])))
fp2.b.upr <- 1/(1+exp(-1*(ec2$coefficients['Intercept'] + ec2$coefficients['post.visible=True'] + ec2$coefficients['treatment.b'] + 2.24*ec2$stderrs['treatment.b'])))
fp2.b.lwr <- 1/(1+exp(-1*(ec2$coefficients['Intercept'] + ec2$coefficients['post.visible=True'] + ec2$coefficients['treatment.b'] - 2.24*ec2$stderrs['treatment.b'])))


ec2df <- data.frame(
  exp.group = factor(c("Control Group","Fact-Check","Fact-Check + Voting")),
  fit = c(fp2.0,fp2.a,fp2.b),
  upr = c(fp2.0- 0.01,fp2.a.upr, fp2.b.upr),
  lwr = c(fp2.0- 0.01, fp2.a.lwr, fp2.b.lwr),
  g = c(0,1,2)
)


ggplot(ec2df, aes(g,fit, fill=exp.group)) +
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width=0.1) +
  ylab("Chance of Including At Least 1 Link") +
  scale_fill_manual(values=cbbPalette, name="Intervention",labels=ec2df$exp.group) +
  scale_y_continuous(expand = c(0,0), labels = scales::percent, limits=c(0,0.08)) +
  ## the size hjust needed varies by the width of the plot
  geom_text(aes(label=sprintf("%1.2f%%", 100*fit)), vjust=1.5, #hjust=0.7, 
            size=7, color="#ffffff", fontface="bold") +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),
        title = element_text(hjust=0.2)) +
  #  xlab(paste("Experiment in r/worldnews. Experiment post count: ", toString(length(unique(paper.comments$post_id))),", comments:",toString(nrow(paper.comments)), sep="")) +
  #  ggtitle("A) Effects of Encouraging Fact-Checking on Comments Including Links") +
  ggsave("illustrations/effects_on_comment_link_behavior.pdf", width = 6.5, height = 4, units = "in")


### NOW JUST PLOT THE EFFECT SIZES (FOR PAPER)

ec2df.effect <- data.frame(
  exp.group = factor(c("Fact-Checking","Fact-Checking + Voting")),
  fit = c(ec2$coefficients['treatment.a'],ec2$coefficients['treatment.b']),
  upr = c(ec2$coefficients['treatment.a'] + 2.24*ec2$stderrs['treatment.a'], 
          ec2$coefficients['treatment.b'] + 2.24*ec2$stderrs['treatment.b']),
  lwr = c(ec2$coefficients['treatment.a'] - 2.24*ec2$stderrs['treatment.a'], 
          ec2$coefficients['treatment.b'] - 2.24*ec2$stderrs['treatment.b']),
  sort.order = c(2,1)
)

ec2df.effect$exp.group <- factor(ec2df.effect$exp.group, levels = c("Fact-Checking", "Fact-Checking + Voting"))

ggplot(ec2df.effect, aes(sort.order,fit, color=exp.group)) +
  geom_point(size=5) +
  xlim(0.5, 2.5) +
  ylim(0,1) +
  expand_limits(x = 0, y = 0) +
  geom_hline(yintercept=0, color="#aaaaaa", linetype="dashed") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width=0.2, size=2) +
  coord_flip() +
  ylab("Effect of encouraging fact-checking on log-odds of a comment including links") +
  scale_color_manual(values=c(cbbPalette[2], cbbPalette[3]), name="Experiment Arms",labels=ec2df.effect$exp.group) +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  theme(title = element_text(hjust=0),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  xlab("") +
  ggsave("illustrations/effects_on_comment_link_log_odds.pdf", width = 8, height = 5, units = "in")


####################################################################
#### EFFECT ON MAX RANKINGS
####################################################################

summary(nplm2a <- lm(max.position.100 ~ treatment.a + treatment.b, data=paper.posts))

sprintf("%1.3f", p.adjust(summary(nplm2a)$coefficients['treatment.aTRUE',][['Pr(>|t|)']], method="bonferroni", n=2))summary(nplm2 <- lm(max.position.100 ~ visible + hour + I(hour^2) + post.weekend + treatment.a + treatment.b, data=paper.posts))


screenreg(list(nplm2a, nplm2), 
          custom.coef.names = c("Intercept", "Fact-Checking",
                                "Fact-Checking + Voting",
                                "Article Permitted", 
                                "Hour Posted", "Hour Posted ^2",
                                "Weekend"
          ),
          custom.model.names=c("Without Adjustment","Main Model"),
          caption="A linear regression predicting the maximum 24 hour rank of 
          unreliable news articles fails to observe an effect from encouraging
          fact-checking or fact-checking and voting.")

texreg(list(nplm2a, nplm2), 
       custom.coef.names = c("Intercept", "Fact-Checking",
                             "Fact-Checking + Voting",
                             "Article Permitted", 
                             "Hour Posted", "Hour Posted ^2",
                             "Weekend"
       ),
       label="table:max.rank",
       custom.model.names=c("Without Adjustment","Main Model"),
       caption="A linear regression predicting the maximum 7 hour rank of 
       unreliable news articles fails to observe an effect from encouraging
       fact-checking or fact-checking and voting.")


paper.posts$includes.links.with.comments <- paper.posts$comment.links.comments > 0

summary(pc1 <- glm(includes.links.with.comments ~ visible + treatment.a + treatment.b, data=paper.posts, family=binomial))

screenreg(list(pc1), digits=3,  label="table:comment.effect", 
          custom.model.names=c("Discussions"), 
          caption="Encouraging fact-checking at the top of an online discussion 
          of unreliable news increased the chance that at least one comment
          would include links to further evidence")


texreg(list(pc1), digits=3,  label="table:post.effect", 
       custom.model.names=c("Discussions"), 
       caption="Encouraging fact-checking at the top of an online discussion 
       of unreliable news increased the chance that at least one comment
       would include links to further evidence")


pc1$stderrs <- sqrt(diag( vcov(pc1)))

pc1.0 <- 1/(1+exp(-1*(pc1$coefficients['(Intercept)'] + pc1$coefficients['visibleTrue'])))
pc1.a <- 1/(1+exp(-1*(pc1$coefficients['(Intercept)'] + pc1$coefficients['visibleTrue'] + pc1$coefficients['treatment.aTRUE'])))
pc1.b <- 1/(1+exp(-1*(pc1$coefficients['(Intercept)'] + pc1$coefficients['visibleTrue'] + pc1$coefficients['treatment.bTRUE'])))


pc1.a.upr <- 1/(1+exp(-1*(pc1$coefficients['(Intercept)'] + pc1$coefficients['visibleTrue'] + pc1$coefficients['treatment.aTRUE'] + 1.96*pc1$stderrs['treatment.aTRUE'])))
pc1.a.lwr <- 1/(1+exp(-1*(pc1$coefficients['(Intercept)'] + pc1$coefficients['visibleTrue'] + pc1$coefficients['treatment.aTRUE'] - 1.96*pc1$stderrs['treatment.aTRUE'])))
pc1.b.upr <- 1/(1+exp(-1*(pc1$coefficients['(Intercept)'] + pc1$coefficients['visibleTrue'] + pc1$coefficients['treatment.bTRUE'] + 1.96*pc1$stderrs['treatment.bTRUE'])))
pc1.b.lwr <- 1/(1+exp(-1*(pc1$coefficients['(Intercept)'] + pc1$coefficients['visibleTrue'] + pc1$coefficients['treatment.bTRUE'] - 1.96*pc1$stderrs['treatment.bTRUE'])))
pc1df <- data.frame(
  exp.group = factor(c("Control Group","Fact-Check","Fact-Check + Voting")),
  fit = c(pc1.0,pc1.a,pc1.b),
  upr = c(pc1.0- 0.01,pc1.a.upr, pc1.b.upr),
  lwr = c(pc1.0- 0.01, pc1.a.lwr, pc1.b.lwr),
  g = c(0,1,2)
)


ggplot(pc1df, aes(g,fit, fill=exp.group)) +
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width=0.1) +
  ylab("Chance of 1+ Comments w/ Links") +
  scale_fill_manual(values=cbbPalette, name="Intervention",labels=pc1df$exp.group) +
  scale_y_continuous(expand = c(0,0), labels = scales::percent, limits=c(0,0.42)) +
  ## the size hjust needed varies by the width of the plot
  geom_text(aes(label=sprintf("%1.1f%%", 100*fit)), vjust=1.5, #hjust=0.7, 
            size=7, color="#ffffff", fontface="bold") +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),
        title = element_text(hjust=0.22)) +
  #  xlab(paste("Experiment in r/worldnews. Experiment post count: ", toString(length(unique(paper.comments$post_id))),", comments:",toString(nrow(paper.comments)), sep="")) +
  ggtitle("B) Effects of Encouraging Fact-Checking on Discussions Including Links")  +
  ggsave("illustrations/effects_on_comment_links_in_discussions.pdf", width = 8.5, height = 4, units = "in")

####################################################################
#### EFFECT ON RANK POSITION AT TIME T
####################################################################
exp.rankings <-  read.csv("datasets/r_worldnews_exp_rankings_07.04.2017.csv")

exp.rankings$after.score.change <- exp.rankings$post.assign.number > 156
exp.rankings$sub.original.study <- exp.rankings$post.assign.number <= 840
exp.rankings$sub.before.notified <- exp.rankings$post.assign.number <=1032
exp.rankings$sub.after.notified <- exp.rankings$post.assign.number > 1044
exp.rankings$sub.after.300 <- exp.rankings$post.assign.number > 744

exp.rankings$treatment.a <- exp.rankings$post.treatment == 1
exp.rankings$treatment.b <- exp.rankings$post.treatment == 2

paper.rankings <- subset(exp.rankings, (after.score.change==TRUE & post.block.id!="block.a.39"))
paper.rankings <- subset(paper.rankings, post.snapshot.number > 0)
paper.rankings <- subset(paper.rankings, post.snapshot.number <= 354)

paper.rankings$snapshot.number <- paper.rankings$post.snapshot.number
paper.rankings$post.age.hours <- paper.rankings$post.age/60
paper.rankings$median.age.100.hours <- paper.rankings$median.age.100/60

paper.rankings.300 <- subset(paper.rankings, (sub.after.300==TRUE & post.block.id!="block.a.39"))

rm(exp.rankings)

####################################################################
#### RANKING TIMESLICE EFFECTS (Top 300)
####################################################################

## IDS TO INCLUDE IN RANKINGS EXAMPLE ILLUSTRATION
ranking.subsample <- subset(paper.rankings.300, post.id %in% c("5tl71h","5sgjxd","5p6545","5o06yd","5pfl82","5sej5h"))

ggplot(subset(ranking.subsample, post.age.hours <=7), aes(y=position.300, x=post.age.hours, color=post.visible)) +
  geom_line(size=1.5) +
  ylim(0,300) +
  facet_wrap(~ post.id, ncol=2) +
  scale_color_manual(values=c(cbbPalette[1], cbbPalette[2]), name="Article Permitted",labels=c("False","True")) +
  scale_x_continuous(breaks = seq(0, max(ranking.subsample$post.age.hours), by = 1), 
                     limits = c(0,7.05), 
                     expand=c(0,0)) +
  theme_bw(base_size = 12, base_family = "Helvetica") +
  ylab("Ranking Position") +
  xlab("Hours Since Article Submitted") +
  theme(strip.background = element_blank(), strip.text = element_blank(),
        axis.title=element_text(size=11),
        plot.title = element_text(size = 12, colour = "black", vjust = -1),
        panel.margin = unit(0.65, "lines")) +
  ggsave("illustrations/rank_position_at_time_examples.pdf", width = 8.5, height = 4, units = "in")

ggplot(ranking.subsample, aes(y=log1p(post.score), x=ranking.subsample$post.age.hours, color=post.visible)) +
  geom_point() +
  facet_grid(post.id ~ .) +
  ggtitle("Score Over Time")

### NOW GENERATE SLICES AND PLOT EFFECT

gen.slice.effect.300 <- function(i){
  snapshot.num = i
  snapshot <- subset(paper.rankings.300, snapshot.number == snapshot.num)
  slice.lm <- lm(position.300 ~ 
                   post.visible + 
                   #                   median.score.300 + 
                   rank.overspill.a.300 +
                   rank.overspill.b.300 + 
                   #                   snapshot.offset +
                   treatment.a + 
                   treatment.b, data=snapshot)
  coef.a <- slice.lm$coefficients['treatment.aTRUE']
  coef.b <- slice.lm$coefficients['treatment.bTRUE']
  stderr.a <- coef(summary(slice.lm))[, "Std. Error"]['treatment.aTRUE']
  stderr.b <- coef(summary(slice.lm))[, "Std. Error"]['treatment.bTRUE']
  upr.a <- coef.a + 2.39*stderr.a
  lwr.a <- coef.a - 2.39*stderr.a
  p.a <- p.adjust(coef(summary(slice.lm))[, 'Pr(>|t|)']['treatment.aTRUE'], method="bonferroni", n=3)
  #p.a <- coef(summary(slice.lm))[, 'Pr(>|t|)']['treatment.aTRUE']
  #   upr.b <- coef.b + 1.96*stderr.b
  #   lwr.b <- coef.b - 1.96*stderr.b
  upr.b <- coef.b + 2.39*stderr.b
  lwr.b <- coef.b - 2.39*stderr.b
  p.b <- p.adjust(coef(summary(slice.lm))[, 'Pr(>|t|)']['treatment.bTRUE'], method="bonferroni", n=3)
  #  p.b <- coef(summary(slice.lm))[, 'Pr(>|t|)']['treatment.bTRUE']
  
  return.value <- c(i, coef.a, stderr.a, upr.a, lwr.a, p.a,
                    coef.b, stderr.b, upr.b, lwr.b, p.b)  
  return(return.value)
}


### GENERATE SLICE REGRESSIONS FOR THE FIRST 7 HOURS
slice.count = 105
n.300 = data.frame(seq(1, slice.count, by=1))
vals <- apply(n.300, 1, gen.slice.effect.300)

names <- c("timeslice",
           "coef.a", "stderr.a", "upr.a", "lwr.a",  "p.a", 
           "coef.b", "stderr.b", "upr.b", "lwr.b", "p.b")
for(i in 1:length(names)){
  n.300[[names[i]]] <- vals[i,]
}
n.300$minutes <- n.300$timeslice*4

### GENERATE THE CUBIC SMOOTHED CONFIDENCE INTERVAL
### AND UPPER AND LOWER BOUNDS (IDENTICAL TO ABOVE)
n.300$confint.a <-  2.39*n.300$stderr.a
confint.cubic.a.300 <- lm(confint.a ~ log1p(minutes) + I(log1p(minutes)^2) + I(log1p(minutes)^3), data=n.300)
n.300$confint.a.cubic <- predict(confint.cubic.a.300, n.300)

n.300$confint.b <-  2.39*n.300$stderr.b
confint.cubic.b.300 <- lm(confint.b ~ log1p(minutes) + I(log1p(minutes)^2) + I(log1p(minutes)^3), data=n.300)
n.300$confint.b.cubic <- predict(confint.cubic.b.300, n.300)

### GENERATE THE CUBIC SMOOTHED EFFECT
effect.cubic.a.300 <- lm(coef.a ~ log1p(minutes) + I(log1p(minutes)^2) + I(log1p(minutes)^3), data=n.300)
n.300$coef.a.cubic <- predict(effect.cubic.a.300, n.300)
n.300.coef.a.r2 <- summary(effect.cubic.a.300)$r.squared
n.300$upr.a.lm <- n.300$coef.a.cubic + n.300$confint.a.cubic
n.300$lwr.a.lm <- n.300$coef.a.cubic - n.300$confint.a.cubic
rm(confint.cubic.a.300)

effect.cubic.b.300 <- lm(coef.b ~ log1p(minutes) + I(log1p(minutes)^2) + I(log1p(minutes)^3), data=n.300)
n.300$coef.b.cubic <- predict(effect.cubic.b.300, n.300)
n.300.coef.b.r2 <- summary(effect.cubic.b.300)$r.squared
n.300$upr.b.lm <- n.300$coef.b.cubic + n.300$confint.b.cubic
n.300$lwr.b.lm <- n.300$coef.b.cubic - n.300$confint.b.cubic
rm(confint.cubic.b.300)

###### TABLES
# texreg(list(effect.cubic.a, effect.cubic.b, effect.cubic.a.300, effect.cubic.b.300), digits=3,  label="table:cubic.smoothing", 
#        custom.model.names=c("Fact-Checking (100)", " Fact-Checking + Voting (100)", "Fact-Checking (300)", "Fact-Checking + Voting (300)"), 
#        caption="Cubic Model Smoothing the Average Treatment Effect
#        of Encouraging Fact-Checking on the Relative Ranking Position
#        of a News Article After a Period of Time")

texreg(list(effect.cubic.a.300, effect.cubic.b.300), digits=3,  label="table:cubic.smoothing", 
       custom.model.names=c("Fact-Checking", "Fact-Checking + Voting"), 
       caption="Cubic Model Smoothing the Average Treatment Effect
       of Encouraging Fact-Checking on the Relative Ranking Position
       of a News Article After a Period of Time")

screenreg(list(effect.cubic.a.300, effect.cubic.b.300), digits=3,  label="table:cubic.smoothing", 
       custom.model.names=c("Fact-Checking", "Fact-Checking + Voting"), 
       caption="Cubic Model Smoothing the Average Treatment Effect
       of Encouraging Fact-Checking on the Relative Ranking Position
       of a News Article After a Period of Time")


#### THE PLOT OF ESTIMATED EFFECTS FOR TREATMENT A (300)
ggplot(n.300, aes(timeslice*4, coef.a)) +
  scale_x_continuous(breaks = round(seq(min(n.300$minutes)-4, max(n.300$minutes), by = 60),1), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(-50, 30, by = 10), limits=c(-45,20)) +
  geom_hline(yintercept=0, color="black") +
  geom_vline(xintercept=0, color="black") +
  geom_ribbon(aes(ymax = upr.a, ymin = lwr.a), fill = cbbPalette[2], alpha=0.45) +
  geom_line(color=cbbPalette[2], size=1)  +
  xlab("Minutes since post submitted") +
  ylab("Effect on article position (top 300)") +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  theme(plot.margin = unit(c(1, 5, 1, 1), "mm")) +
  ggtitle("Encouraging Fact-Checking") + #: Effect in Top 300") +
  ggsave("illustrations/rank_position_effect_fact_check_300.pdf", width = 5.5, height = 5, units = "in")


#SMOOTHED PLOT FOR TREATMENT A (300)
ggplot(n.300) +
  geom_ribbon(aes(x = minutes, ymin = lwr.a.lm, ymax = upr.a.lm),
              fill = cbbPalette[2], alpha = 0.45) +
  geom_line(data=n.300, aes(y=coef.a.cubic, x=minutes), color=cbbPalette[2], size=1) +
  geom_point(y=n.300$coef.a, x=n.300$minutes, color=cbbPalette[2]) +
  geom_hline(yintercept=0, color="black") +
  geom_vline(xintercept=0, color="black") +
  scale_x_continuous(breaks = round(seq(min(n.300$minutes)-4, max(n.300$minutes), by = 60),1), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(-40, 30, by = 10), limits=c(-42,20)) +
  xlab("Minutes since post submitted") +
  ylab("Effect on rank position (1 to 300)") +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  theme(plot.margin = unit(c(1, 5, 1, 1), "mm")) +
  geom_text(x = Inf, y = Inf, label = paste("R2 = ",signif(n.300.coef.a.r2,2)), hjust = 1.15, vjust = 1.5, family="Helvetica", fontface="plain", size=4.5, color="#999999") +
  #  ggtitle("Encouraging Fact-Checking caused articles to drop in reddit rankings by as many as 20 positions on average")
  ggtitle("Encouraging fact-checking") + # : Effect in Top 300") +
  ggsave("illustrations/rank_position_effect_fact_check_300_cubic.pdf", width = 5.5, height = 5, units = "in")


#### THE PLOT OF ESTIMATED EFFECTS FOR TREATMENT B
ggplot(n.300, aes(minutes, coef.b)) +
  geom_hline(yintercept=0, color="black") +
  geom_vline(xintercept=0, color="black") +
  geom_ribbon(aes(ymax = upr.b, ymin = lwr.b), fill = cbbPalette[3], alpha=0.45) +
  geom_line(color=cbbPalette[3], size=1)  +
  scale_x_continuous(breaks = round(seq(min(n.300$minutes)-4, max(n.300$minutes), by = 60),1), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(-40, 30, by = 10), limits=c(-42,20)) +
  #  scale_color_manual(limits=c("Fact-Checking & Voting"),  name="", values=c(cbbPalette[3])) +
  xlab("Minutes Since Post Submitted") +
  ylab("Effect On Rank Position (Top 300)") +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  theme(plot.margin = unit(c(1, 5, 1, 1), "mm")) +
  ggtitle("Encouraging Fact-Checking and Voting") + #: Effect in Top 300") +
  ggsave("illustrations/rank_position_effect_fact_check_vote_300.pdf", width = 5.5, height = 5, units = "in")

### SMOOTHED PLOT TREATMENT B
ggplot(n.300) +
  geom_ribbon(aes(x = minutes, ymin = lwr.b.lm, ymax = upr.b.lm), fill = cbbPalette[3], alpha = 0.45) +
  geom_line(data=n.300, aes(y=coef.b.cubic, x=minutes), color=cbbPalette[3], size=1) +
  geom_point(y=n.300$coef.b, x=n.300$minutes, color=cbbPalette[3]) +
  geom_hline(yintercept=0, color="black") +
  geom_vline(xintercept=0, color="black") +
  scale_x_continuous(breaks = round(seq(min(n.300$minutes)-4, max(n.300$minutes), by = 60),1), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(-40, 30, by = 10), limits=c(-42,20)) +
  xlab("Minutes Since Post Submitted") +
  ylab("Effect On Rank Position (1 to 300)") +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  theme(plot.margin = unit(c(1, 5, 1, 1), "mm")) +
  geom_text(x = Inf, y = Inf, label = paste("R2 = ",signif(n.300.coef.b.r2,2)), hjust = 1.15, vjust = 1.5, family="Helvetica", fontface="plain", size=4.5, color="#999999") +
  ggtitle("Encouraging Fact-Checking and Voting") + #: Effect in Top 300") +
  ggsave("illustrations/rank_position_effect_fact_check_vote_300_cubic.pdf", width = 5.5, height = 5, units = "in")



ggplot(n.300) +
  geom_ribbon(aes(x = minutes, ymin = lwr.b.lm, ymax = upr.b.lm), fill = cbbPalette[3], alpha = 0.45) +
  geom_line(data=n.300, aes(y=coef.b.cubic, x=minutes), color=cbbPalette[3], size=1) +
  geom_point(y=n.300$coef.b, x=n.300$minutes, color=cbbPalette[3]) +
  geom_hline(yintercept=0, color="black") +
  geom_vline(xintercept=0, color="black") +
  scale_x_continuous(breaks = round(seq(min(n.300$minutes)-4, max(n.300$minutes), by = 60),1), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(-40, 30, by = 10), limits=c(-42,20)) +
  xlab("Minutes Since Post Submitted") +
  ylab("Effect On Rank Position (1 to 300)") +
  theme_bw(base_size = 40, base_family = "Helvetica") +
  theme(plot.margin = unit(c(1, 10, 1, 1), "mm")) +
  geom_text(x = Inf, y = Inf, label = paste("R2 = ",signif(n.300.coef.a.r2,2)), hjust = 1.15, vjust = 1.5, family="Helvetica", fontface="plain", size=4.5, color="#999999")

##### THE REGRESSION RESULTS FOR A
texreg(list(effect.cubic.a.300),  digits=3,  label="table:effect.cubic.a", 
       custom.model.names = c("Fact-Check Effect in Top 300"),
       custom.coef.names = c("Intercept", "ln Minutes", "ln Minutes^2", "ln Minutes^3"),
       caption="Cubic polynomial models of average treatment effect of encouraging fact-checking over time
       on the top 300 ranked items")


####################################################################
#### SCORE TIMESLICE EFFECTS (Top 300)
####################################################################

gen.slice.effect.score.300 <- function(i){
  snapshot.num = i
  snapshot <- subset(paper.rankings.300, snapshot.number == snapshot.num)
  slice.lm <- lm(post.score ~ 
                   post.visible + 
                   #                   median.score.300 + 
                   rank.overspill.a.300 +
                   rank.overspill.b.300 + 
                   #                   snapshot.offset +
                   treatment.a + 
                   treatment.b, data=snapshot)
  coef.a <- slice.lm$coefficients['treatment.aTRUE']
  coef.b <- slice.lm$coefficients['treatment.bTRUE']
  stderr.a <- coef(summary(slice.lm))[, "Std. Error"]['treatment.aTRUE']
  stderr.b <- coef(summary(slice.lm))[, "Std. Error"]['treatment.bTRUE']
  upr.a <- coef.a + 1.96*stderr.a
  lwr.a <- coef.a - 1.96*stderr.a
  #upr.a <- coef.a + 2.39*stderr.a
  #lwr.a <- coef.a - 2.39*stderr.a
  #p.a <- p.adjust(coef(summary(slice.lm))[, 'Pr(>|t|)']['treatment.aTRUE'], method="bonferroni", n=3)
  p.a <- coef(summary(slice.lm))[, 'Pr(>|t|)']['treatment.aTRUE']
  upr.b <- coef.b + 1.96*stderr.b
  lwr.b <- coef.b - 1.96*stderr.b
  #upr.b <- coef.b + 2.39*stderr.b
  #lwr.b <- coef.b - 2.39*stderr.b
  #p.b <- p.adjust(coef(summary(slice.lm))[, 'Pr(>|t|)']['treatment.bTRUE'], method="bonferroni", n=3)
  p.b <- coef(summary(slice.lm))[, 'Pr(>|t|)']['treatment.bTRUE']
  intercept <- coef(summary(slice.lm))[, 'Estimate'][['(Intercept)']]
  
  return.value <- c(i, coef.a, stderr.a, upr.a, lwr.a, p.a,
                    coef.b, stderr.b, upr.b, lwr.b, p.b, intercept)  
  return(return.value)
}



### GENERATE SLICE REGRESSIONS FOR THE FIRST 7 HOURS
slice.count = 105
n.300.score = data.frame(seq(1, slice.count, by=1))
vals <- apply(n.300.score, 1, gen.slice.effect.score.300)

names <- c("timeslice",
           "coef.a", "stderr.a", "upr.a", "lwr.a",  "p.a", 
           "coef.b", "stderr.b", "upr.b", "lwr.b", "p.b", "intercept")
for(i in 1:length(names)){
  n.300.score[[names[i]]] <- vals[i,]
}
n.300.score$minutes <- n.300.score$timeslice*4


#### THE PLOT OF ESTIMATED EFFECTS FOR TREATMENT A (300)
ggplot(subset(n.300.score, timeslice*4<=190), aes(timeslice*4, coef.a)) +
  scale_x_continuous(breaks = round(seq(min(n.300$minutes)-4, max(n.300$minutes), by = 30),1), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(-70, 0, by = 10), limits=c(-60,5)) +
  geom_hline(yintercept=0, color="black") +
  geom_vline(xintercept=0, color="black") +
  geom_ribbon(aes(ymax = upr.a, ymin = lwr.a), fill = cbbPalette[2], alpha=0.45) +
  geom_line(color=cbbPalette[2], size=1)  +
  xlab("Minutes since post submitted") +
  ylab("Effect on Score") +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  theme(plot.margin = unit(c(1, 5, 1, 1), "mm")) +
  ggtitle("Encouraging Fact-Checking") + #: Effect in Top 300") +
  ggsave("illustrations/score_effect_fact_check_300.pdf", width = 5.5, height = 5, units = "in")

#### THE PLOT OF ESTIMATED EFFECTS FOR TREATMENT B
ggplot(subset(n.300.score, timeslice*4<=190), aes(minutes, coef.b)) +
  geom_hline(yintercept=0, color="black") +
  geom_vline(xintercept=0, color="black") +
  geom_ribbon(aes(ymax = upr.b, ymin = lwr.b), fill = cbbPalette[3], alpha=0.45) +
  geom_line(color=cbbPalette[3], size=1)  +
  scale_x_continuous(breaks = round(seq(min(n.300$minutes)-4, max(n.300$minutes), by = 60),1), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(-70, 0, by = 10), limits=c(-60,5)) +
  #  scale_color_manual(limits=c("Fact-Checking & Voting"),  name="", values=c(cbbPalette[3])) +
  xlab("Minutes Since Post Submitted") +
  ylab("Effect On Score") +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  theme(plot.margin = unit(c(1, 5, 1, 1), "mm")) +
  ggtitle("Encouraging Fact-Checking and Voting") + #: Effect in Top 300") +
  ggsave("illustrations/score_effect_fact_check_vote_300.pdf", width = 5.5, height = 5, units = "in")


#### CREATE SINGLE COMPARATIVE PLOT BETWEEN THE TWO INTERVENTIONS
n.300.score.effects.a <- n.300.score
n.300.score.effects.a$condition <- "Fact-Checking"
n.300.score.effects.a$coef <- n.300.score.effects.a$coef.a
n.300.score.effects.a$upr <- n.300.score.effects.a$upr.a
n.300.score.effects.a$lwr <- n.300.score.effects.a$lwr.a
n.300.score.effects.a$minutes <- n.300.score.effects.a$minutes - 1

n.300.score.effects.b <- n.300.score
n.300.score.effects.b$condition <- "Fact-Checking + Voting"
n.300.score.effects.b$coef <- n.300.score.effects.b$coef.b
n.300.score.effects.b$upr <- n.300.score.effects.b$upr.b
n.300.score.effects.b$lwr <- n.300.score.effects.b$lwr.b

## MERGE DATASET OF EFFECTS
n.300.score.effects <- rbind(n.300.score.effects.b, n.300.score.effects.a)

ggplot(subset(n.300.score.effects, timeslice*4<=200), aes(minutes, coef, color=factor(condition))) +
  geom_line(size=0.4) +
  geom_point(size=0.8) +
  geom_errorbar(aes(ymax = upr, ymin = lwr, width=0.2)) +
  scale_x_continuous(breaks = round(seq(min(n.300$minutes)-4, max(n.300$minutes), by = 20),1), expand = c(0,2)) +
  scale_y_continuous(breaks = seq(-70, 10, by = 10), limits=c(-70,8)) +
  scale_color_manual(values=c(cbbPalette[2], cbbPalette[3]), name="Experiment Arms:") +
  geom_hline(yintercept=0, color="black") +
  geom_vline(xintercept=0, color="black") +
  theme_bw(base_size = 15, base_family = "Helvetica") +
  theme(plot.margin = unit(c(1, 5, 1, 1), "mm"), legend.position="top") +
  ylab("Effect on Vote Score") +
  xlab("Minutes After Submission (Fact-Checking shifted left by 1 for clarity)") +
  #  ggtitle("Encouraging Fact-Checking and Voting") +
  ggsave("illustrations/score_effect_fact_check_both_300.pdf", width = 8.5, height = 5, units = "in")


####################################################################
#### SAVE TO RData FILE FOR PAPER
####################################################################
save.image("datasets/r_worldnews_paper_results-03.2019.RData")

####################################################################
### OUTPUT TABLES OF SLICE REGRESSION RESULTS
####################################################################
library(xtable)
var.labels = c(minutes="Minutes",
               "coef.a" ="FC Coefficient", "stderr.a"="FC Stderr", "upr.a"="FC 95% ConfInt Upper", "lwr.a" = "FC 95% ConfInt Lower", "p.a"="FC p-value", 
               "coef.a.cubic"="FC Coefficient (Smoothed)", "upr.a.lm"="FC 95% Confint Upper (Smoothed)", "lwr.a.lm"="FC 95% ConfInt Lower",
               "coef.b" ="FC+V Coefficient", "stderr.b"="FC+V Stderr", "upr.b"="FC+V 95% ConfInt Upper", "lwr.b" = "FC+V 95% ConfInt Lower", "p.b"="FC+V p-value", 
               "coef.b.cubic"="FC+V Coefficient (Smoothed)", "upr.b.lm"="FC+V 95% Confint Upper (Smoothed)", "lwr.b.lm"="FC+V 95% ConfInt Lower")

n.300.raw <- n.300[,(names(n.300) %in% c("minutes",
                                         "coef.a", "stderr.a", "upr.a", "lwr.a", "p.a", 
                                         "coef.b", "stderr.b", "upr.b", "lwr.b", "p.b"))]

xtable(subset(n.300.raw, select=c("minutes",
                                  "coef.a", "stderr.a", "upr.a", "lwr.a", "p.a", 
                                  "coef.b", "stderr.b", "upr.b", "lwr.b", "p.b")))

### GENERATE TABLE OF THE EFFECT ON VOTE SCORES OVER TIME

n.300.score.raw <- n.300.score[,(names(n.300.score) %in% c("minutes",
                                                           "coef.a", "stderr.a", "upr.a", "lwr.a", "p.a", 
                                                           "coef.b", "stderr.b", "upr.b", "lwr.b", "p.b"))]
xtable(subset(n.300.score.raw, select=c("minutes",
                                        "coef.a", "stderr.a", "upr.a", "lwr.a", "p.a", 
                                        "coef.b", "stderr.b", "upr.b", "lwr.b", "p.b")))

