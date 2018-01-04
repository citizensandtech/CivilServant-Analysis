library(blockrand)

rm(list=ls())
########################################################
########## GENERATE EXPERIMENT RANDOMIZATIONS ##########
########################################################
set.seed(1600049482)

arms = 5
block.sizes = 3
observations = arms*block.sizes * 10
  

randomizations <- blockrand(n=observations, num.levels = arms, block.sizes = c(block.sizes,block.sizes), id.prefix='post', block.prefix='block',stratum='post')
randomizations <- randomizations[c('treatment', 'block.id', 'block.size')]

randomizations$treatment <- as.numeric(randomizations$treatment) - 1

write.table(randomizations, "outputs/twitter_dmca_test.randomizations.csv", sep = ",", col.names = NA)

#########################################
########## GENERATE UNIT TESTS ##########
#########################################
set.seed(1600049483)

arms = 5
block.sizes = 3
observations = arms*block.sizes * 10


randomizations <- blockrand(n=observations, num.levels = arms, block.sizes = c(block.sizes,block.sizes), id.prefix='post', block.prefix='block',stratum='post')
randomizations <- randomizations[c('treatment', 'block.id', 'block.size')]

randomizations$treatment <- as.numeric(randomizations$treatment) - 1

write.table(randomizations, "outputs/twitter_dmca_test.randomizations.csv", sep = ",", col.names = NA)
