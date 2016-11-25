library(blockrand)
library(dummies)
library(gmodels)
library(descr)
library(ggplot2)

rm(list=ls())
set.seed(1470109736)

sim.logistic <- function(i, observations, beta.intercept, beta.treat){
#  observations = 10000
  conditions = 2
  block.sizes = 5
  obs <- blockrand(n=observations, num.levels = conditions, block.sizes = c(block.sizes,block.sizes), id.prefix='AMA', block.prefix='AMA',stratum='AMA')
  summary(obs$treatment)
  # 1 = visible
  # 0 = not visible
  # A = treat
  # B = no treat
  pr = cbind(1, dummy(obs$treatment)[,-1]) %*% c(beta.intercept, beta.treat)
  obs$sim.visible <- rbinom(n=observations, size=1, prob=pr)
  
#   CrossTable(pr, obs$sim.visible, prop.r=TRUE, prop.c=FALSE, prop.t=FALSE, prop.chisq=FALSE)
#   CrossTable(obs$sim.visible, obs$treatment, prop.r=TRUE, prop.c=FALSE, prop.t=FALSE, prop.chisq=FALSE)
  
  summary(l1 <- glm(sim.visible ~ treatment, data = obs, family = binomial(link='logit')))
  ctl.prob <- 1/(1+ exp(l1$coefficients['(Intercept)']*-1))
  ctl.prob
  treat.prob <- 1/(1+ exp((l1$coefficients['(Intercept)'] + l1$coefficient['treatmentB'])*-1))
  pct.diff <- treat.prob[[1]] - ctl.prob[[1]]
  coef = coef(summary(l1))[2,]
  coefficient <- coef[['Estimate']]
  stderr <- coef[['Std. Error']]
  pvalue <- coef[['Pr(>|z|)']]
  c(pct.diff, coefficient, stderr, pvalue)
}

simulation.count = 100
observation.count = 12360*2 # with two weeks of observations
beta.intercept = 0.67 ## ACTUAL INTERCEPT FROM SITE
beta.treat = 0.02


n = data.frame(seq(1,simulation.count,by=1))

x <- apply(n, 1, sim.logistic, observations=observation.count,beta.intercept=beta.intercept, beta.treat=beta.treat)
n$pct.diff <- x[1,]
n$coefficient <- x[2,]
n$stderr <- x[3,]
n$pvalue <- x[4,]

n$sig <- 0
n$sig[n$pvalue <= 0.05] <- 1

ggplot(n, aes(pct.diff, stderr, colour=factor(sig))) +
  geom_point()

hist(n$pct.diff)


print(paste("A", beta.treat, "percentage point difference will yield statistical significance", 
      mean(n$sig)*100.,"% of the time with studies of ", observation.count, 
      "observations where the outcome has a baseline", beta.intercept, "chance of occurring."))
