library(blockrand)

rm(list=ls())
########################################################
########## GENERATE EXPERIMENT RANDOMIZATIONS ##########
########################################################
#source: https://www.brooklynintegers.com/int/1511792391/
set.seed(1511792391)

observations = 20000 # the study needs 13k observations
arms = 2
block.sizes = 5

randomizations <- blockrand(n=observations, num.levels = arms, block.sizes = c(block.sizes,block.sizes), id.prefix='post', block.prefix='block',stratum='post')
randomizations <- randomizations[c('treatment', 'block.id', 'block.size')]

randomizations$treatment <- as.numeric(randomizations$treatment) - 1

randomizations$block.id <- paste("2nd.round.", randomizations$block.id, sep="")

write.table(randomizations, "outputs/newcomer_messaging_randomizations-feminism-01.01.2019.csv", sep = ",", col.names = NA)
