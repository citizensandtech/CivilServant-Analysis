library(blockrand)

rm(list=ls())
########################################################
########## GENERATE EXPERIMENT RANDOMIZATIONS ##########
########################################################
set.seed(1500048489)

observations = 10000 # enough randomizations allowing for 12 blocks to be spoiled
arms = 2
block.sizes = 3

randomizations <- blockrand(n=observations, num.levels = arms, block.sizes = c(block.sizes,block.sizes), id.prefix='post', block.prefix='block',stratum='post')
randomizations <- randomizations[c('treatment', 'block.id', 'block.size')]

randomizations$treatment <- as.numeric(randomizations$treatment) - 1

write.table(randomizations, "outputs/sticky_comment_science_1.randomizations.csv", sep = ",", col.names = NA)
