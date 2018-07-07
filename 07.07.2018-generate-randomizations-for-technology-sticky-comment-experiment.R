library(blockrand)

rm(list=ls())
########################################################
########## GENERATE EXPERIMENT RANDOMIZATIONS ##########
########################################################
set.seed(1500048492)

observations = 400 # 200 observations, and then enough randomizations allowing for some blocks to be spoiled
arms = 2
block.sizes = 3

randomizations <- blockrand(n=observations, num.levels = arms, block.sizes = c(block.sizes,block.sizes), id.prefix='post', block.prefix='block',stratum='post')
randomizations <- randomizations[c('treatment', 'block.id', 'block.size')]

randomizations$treatment <- as.numeric(randomizations$treatment) - 1

write.table(randomizations, "outputs/sticky_comment_technology_1.randomizations.csv", sep = ",", col.names = NA)

#########################################
########## GENERATE UNIT TESTS ##########
#########################################
set.seed(1500048490)

observations = 660 + 144 # enough randomizations allowing for 12 blocks to be spoiled
arms = 2
block.sizes = 3

randomizations <- blockrand(n=observations, num.levels = arms, block.sizes = c(block.sizes,block.sizes), id.prefix='post', block.prefix='block',stratum='post')
randomizations <- randomizations[c('treatment', 'block.id', 'block.size')]

randomizations$treatment <- as.numeric(randomizations$treatment) - 1

write.table(randomizations, "outputs/sticky_comment_frontpage.randomizations.csv", sep = ",", col.names = NA)
