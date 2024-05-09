library(boot, lib.loc = "/Library/Frameworks/R.framework/Versions/4.2/Resources/library")
library(ggplot2)
set.seed(12345)

###### GENERATE THE TWO DATASETS - DISCRETE AND CONTINUOUS POP. ################

################  --- Continuous Population ---  ###############################

dat <- aircondit7$hours
nsize <- 100
bsize <- nsize
B <- 200

# make your datasets
yexp <- rexp(nsize, rate = 1/mean(dat))
ygeom <- rgeom(nsize, prob = 1/mean(dat))

# note that all of this is written so that you can put it into a function to iterate over it

ogsamp <- yexp # specify your sample (cont or discrete)

statvals <- matrix(0, nrow=B, ncol = 4)
colnames(statvals) <- c("Mean", "Median", "StdDev", "MedAbsDev")

# perform normal bootstrap on them
for (b in 1:B)
{
  newSelect <- sample(1:nsize, size = bsize, replace = TRUE)
  newSample <- ogsamp[newSelect]
  smean <- mean(newSample)
  smed <- median(newSample)
  ssd <- sd(newSample)
  smad <- mad(newSample)
  statvals[b,] <- c(smean, smed, ssd, smad)
}

par(mfrow = c(2,2))

hist(statvals[,1], main = "Mean")
hist(statvals[,2], main = "Median")
hist(statvals[,3], main = "Standard deviation")
hist(statvals[,4], main = "Median absolute deviation")

par(mfrow = c(1,1))

# Get the estimated statistics of the bootstrap replications
apply(statvals, 2, mean)
# Get the estimated standard errors of the bootstrap replications
apply(statvals, 2, sd)


