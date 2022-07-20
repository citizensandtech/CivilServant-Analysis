library(blockrand)
library(stringr)

rm(list=ls())

########################################################
########## GENERATE POST RANDOMIZATIONS ##########
########################################################
set.seed(1500048489)

post.observations = 1560 # expected sample size: 780
arms = 3
block.sizes = 2

post.randomizations <- blockrand(n=post.observations, num.levels = arms, block.sizes = c(block.sizes,block.sizes), id.prefix='post', block.prefix='block',stratum='post')
post.randomizations <- post.randomizations[c('treatment', 'block.id', 'block.size')]
post.randomizations$treatment <- as.numeric(post.randomizations$treatment) - 1

post.randomizations$block.id <- str_replace(post.randomizations$block.id , "block", "")
summary(factor(post.randomizations$block.id))

write.table(post.randomizations, "outputs/iama.guestbook.post.randomizations.03.12.2020.csv", sep = ",", col.names = NA)


########################################################
########## GENERATE MOD ACTION RANDOMIZATIONS ##########
########################################################
set.seed(1500465489)

# 194 actions per day on average
# roughly 100 days for the epxeriment
# one out of three posts will get the mod action randomizations
# baseline minimum: 194 * 100 = 19,400 / 3 = 6,467
# multiply by 4 to create buffer: 6,467 * 4 = 25,868

mod.observations = 25872 # added 4 to be divisible by our block size
arms = 2
block.sizes = 3

mod.randomizations <- blockrand(n=mod.observations, num.levels = arms, block.sizes = c(block.sizes,block.sizes), id.prefix='m', block.prefix='block',stratum='m')
mod.randomizations <- mod.randomizations[c('treatment', 'block.id', 'block.size')]

mod.randomizations$treatment <- as.numeric(mod.randomizations$treatment) - 1
mod.randomizations$block.id <- str_replace(mod.randomizations$block.id , "block", "")
summary(factor(mod.randomizations$block.id))


write.table(mod.randomizations, "outputs/iama.guestbook.mod.randomizations.03.12.2020.csv", sep = ",", col.names = NA)
