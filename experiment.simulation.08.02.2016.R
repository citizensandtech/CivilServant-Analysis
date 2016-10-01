library(ggplot2)
library(lubridate)
library(stargazer)
library(texreg)
library(lme4)
library(lmerTest)

### LOAD POSTS
short.posts <- read.csv("outputs/r_science_posts_2016.07.04_04.46.26-2016.07.14_21.36.36.csv")
short.posts$post.ama <- short.posts$post.ama=="True"
short.posts$datetime <- as.POSIXct(short.posts$created_utc, origin="1970-01-01")
short.posts$post.hour <- hour(short.posts$datetime)
short.posts$newcomer.comments.removed.experiment.day.zero <- short.posts$newcomer.comments.removed.experiment.day == 0
short.posts$newcomer.comments.experiment.day.zero <- short.posts$newcomer.comments.experiment.day == 0
short.posts$newcomer.comments.zero <- short.posts$newcomer.comments == 0
short.posts$newcomer.comments.removed.zero <- short.posts$newcomer.comments == 0


### LOAD COMMENTS
short.comments <- read.csv("outputs/r_science_comments_2016.07.04_04.46.26-2016.07.14_21.36.36.csv")
short.comments$datetime <-  as.POSIXct(short.comments$created_utc, origin="1970-01-01")
short.comments$hour <- hour(short.comments$datetime)
short.comments$post.ama <- short.comments$post.ama=="True"
short.comments$post.datetime <- as.POSIXct(short.comments$post.created)
short.comments$post.hour <- hour(short.comments$post.datetime)

####################
### GENERATE CHARTS
####################
daterange <- paste(strftime(min(short.posts$datetime), "%b %e"), " to ", strftime(max (short.posts$datetime), "%b %e, %Y"))

####################
##### Newcomer Removals Per Post

ggplot(short.posts, aes(log1p(newcomer.comments.removed))) + 
  geom_histogram(breaks=seq(0, 8, by =0.5)) +
  xlab("log1p newcomer deletions per post") +
  ylab("Frequency") +
  ggtitle(paste("Newcomer Deletions Per Post (n=",toString(nrow(short.posts)),"), ", daterange)) +
  theme(plot.title = element_text(lineheight=1.2, face="bold")) +
  ggsave("figures/newcomer_deletions_per_post.pdf", width=8, height=4)


####################
##### Newcomers Per Post

ggplot(short.posts, aes(log1p(newcomer.comments))) + 
  geom_histogram(breaks=seq(0, 8, by =0.5)) +
  xlab("log1p newcomers per post") +
  ylab("Frequency") +
  ggtitle(paste("Newcomers Per Post (n=",toString(nrow(short.posts)),"), ", daterange)) +
  theme(plot.title = element_text(lineheight=1.2, face="bold")) +
  ggsave("figures/newcomers_per_post.pdf", width=8, height=4)


####################
##### Minutes on Top Page Per Post
short.posts$post.sub.top.minutes.ln <- log1p(short.posts$post.sub.top.minutes)

hist(short.posts$post.sub.top.minutes)

####################
##### Newcomer Comments Data
newcomer.comments <- subset(short.comments, author.prev.comments == 0)
summary(newcomer.comments$visible)

