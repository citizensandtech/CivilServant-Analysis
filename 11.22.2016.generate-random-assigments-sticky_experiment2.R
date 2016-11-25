library(blockrand)

rm(list=ls())
########################################################
########## GENERATE EXPERIMENT RANDOMIZATIONS ##########
########################################################
set.seed(1571122187)

observations = 660 + 144 # enough randomizations allowing for 12 blocks to be spoiled
arms = 3
block.sizes = 4

randomizations <- blockrand(n=observations, num.levels = arms, block.sizes = c(block.sizes,block.sizes), id.prefix='post', block.prefix='block',stratum='post')
randomizations <- randomizations[c('treatment', 'block.id', 'block.size')]

randomizations$treatment <- as.numeric(randomizations$treatment) - 1

write.table(randomizations, "outputs/sticky_comment_worldnews_0.randomizations.csv", sep = ",", col.names = NA)


#########################################
########## GENERATE UNIT TESTS ##########
#########################################
set.seed(1571122186)

observations = 660 + 144 # enough randomizations allowing for 12 blocks to be spoiled
arms = 3
block.sizes = 4

randomizations <- blockrand(n=observations, num.levels = arms, block.sizes = c(block.sizes,block.sizes), id.prefix='post', block.prefix='block',stratum='post')
randomizations <- randomizations[c('treatment', 'block.id', 'block.size')]

randomizations$treatment <- as.numeric(randomizations$treatment) - 1

write.table(randomizations, "outputs/sticky_comment_multiarm.randomizations.csv", sep = ",", col.names = NA)
