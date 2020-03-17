library(blockrand)

rm(list=ls())
########################################################
########## GENERATE EXPERIMENT RANDOMIZATIONS ##########
########################################################
set.seed(1500058482)

observations = 120 # we won't run for 4 months, but let's offer ourselves plenty of buffer
arms = 2
block.sizes = c(2,3)

randomizations <- blockrand(n=observations, num.levels = arms, block.sizes = block.sizes, id.prefix='day', block.prefix='condition',stratum='day')
randomizations <- randomizations[c('treatment', 'block.id', 'block.size')]

randomizations$treatment <- as.numeric(randomizations$treatment) - 1


write.table(subset(randomizations, block.size == 2*arms), "outputs/stylesheet_politics_special.randomizations.csv", sep = ",", col.names = NA)
write.table(subset(randomizations, block.size == 3*arms), "outputs/stylesheet_politics_normal.randomizations.csv", sep = ",", col.names = NA)

#########################################
########## GENERATE UNIT TESTS ##########
#########################################
set.seed(1500078490)

observations = 120 # we won't run for 4 months, but let's offer ourselves plenty of buffer
arms = 2
block.sizes = c(2,3)

randomizations <- blockrand(n=observations, num.levels = arms, block.sizes = block.sizes, id.prefix='day', block.prefix='condition',stratum='day')
randomizations <- randomizations[c('treatment', 'block.id', 'block.size')]

randomizations$treatment <- as.numeric(randomizations$treatment) - 1

write.table(randomizations, "outputs/stylesheet_politics_test.randomizations.csv", sep = ",", col.names = NA)
