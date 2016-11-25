library(blockrand)

rm(list=ls())
set.seed(1471122187)

########### FIRST FOR NON-AMA POSTS
observations = 4400
conditions = 2
block.sizes = 5

nonama <- blockrand(n=observations, num.levels = conditions, block.sizes = c(block.sizes,block.sizes), id.prefix='post', block.prefix='nonama.block',stratum='post')
nonama <- nonama[c('treatment', 'block.id', 'block.size')]
nonama$treatment <- ifelse(nonama$treatment=="A", 0, 1)


########### NOW FOR AMAs
ama_observations = 24
ama_conditions = 2
ama_block.sizes = 3

ama <- blockrand(n=ama_observations, num.levels = ama_conditions, block.sizes = c(ama_block.sizes,ama_block.sizes), id.prefix='post', block.prefix='ama.block',stratum='ama')
ama <- ama[c('treatment', 'block.id', 'block.size')]
ama$treatment <- ifelse(ama$treatment=="A", 0, 1)
write.table(ama, "outputs/sticky_comment_science_0_ama.conditions.csv", sep = ",", col.names = NA)
write.table(nonama, "outputs/sticky_comment_science_0_nonama.conditions.csv", sep = ",", col.names = NA)


#########################################
########## GENERATE UNIT TESTS ##########
#########################################
set.seed(1471122186)

observations = 4400
conditions = 2
block.sizes = 5

nonama <- blockrand(n=observations, num.levels = conditions, block.sizes = c(block.sizes,block.sizes), id.prefix='post', block.prefix='nonama.block',stratum='post')
nonama <- nonama[c('treatment', 'block.id', 'block.size')]
nonama$treatment <- ifelse(nonama$treatment=="A", 0, 1)


########### NOW FOR AMAs
ama_observations = 24
ama_conditions = 2
ama_block.sizes = 3

ama <- blockrand(n=ama_observations, num.levels = ama_conditions, block.sizes = c(ama_block.sizes,ama_block.sizes), id.prefix='post', block.prefix='ama.block',stratum='ama')
ama <- ama[c('treatment', 'block.id', 'block.size')]
ama$treatment <- ifelse(ama$treatment=="A", 0, 1)
write.table(ama, "outputs/sticky_comment_0_ama.conditions.csv", sep = ",", col.names = NA)
write.table(nonama, "outputs/sticky_comment_0.conditions.csv", sep = ",", col.names = NA)
