library(blockrand)

rm(list=ls())
########################################################
########## GENERATE EXPERIMENT RANDOMIZATIONS ##########
########################################################
set.seed(1580048482)

observations = 100002 # enough randomizations to meet the requirements whatever the size of the study
arms = 2
block.sizes = 3

randomizations <- blockrand(n=observations, num.levels = arms, block.sizes = c(block.sizes,block.sizes), id.prefix='post', block.prefix='block',stratum='post')
randomizations <- randomizations[c('treatment', 'block.id', 'block.size')]

randomizations$treatment <- as.numeric(randomizations$treatment) - 1

write.table(randomizations, "outputs/sticky_comment_ffxiv_0.randomizations.csv", sep = ",", col.names = NA)

#########################################
########## GENERATE UNIT TESTS ##########
#########################################
set.seed(1802048490)

observations = 100002 # enough randomizations to meet the requirements whatever the size of the study
arms = 2
block.sizes = 3

randomizations <- blockrand(n=observations, num.levels = arms, block.sizes = c(block.sizes,block.sizes), id.prefix='post', block.prefix='block',stratum='post')
randomizations <- randomizations[c('treatment', 'block.id', 'block.size')]

randomizations$treatment <- as.numeric(randomizations$treatment) - 1

write.table(randomizations, "outputs/sticky_comment_ffxiv_test.randomizations.csv", sep = ",", col.names = NA)
