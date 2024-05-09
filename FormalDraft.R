# library(boot, lib.loc = "/Library/Frameworks/R.framework/Versions/4.2/Resources/library")
library(boot, lib.loc = "C:/Program Files/R/R-4.2.3/library")
library(ggplot2)
set.seed(1234)

###### GENERATE THE TWO DATASETS - DISCRETE AND CONTINUOUS POP. ################

dat <- aircondit7$hours
nsize <- 100
# nsizes <- c(5,10,20,50,100,120,150,180,200,300,400,500,750,1000,1500,2000,2500,3000,3500,4000,5000,6000,7000,8000,9000,10000,10500,11000,11500,12000, 15000, 20000)
nsizes <- seq(from = 50, to = 5000, length.out = 30)
bsize <- nsize
B <- 200

BootMeans <- numeric()
BootMeansDevs <- numeric()
BootMeds <- numeric()
BootMads <- numeric()
BootSdev <- numeric()


ogsampmeans <- numeric()
ogsampdevs <- numeric()
ogsampmeds <- numeric()
ogsampmad <- numeric()

for (n in nsizes) 
  {
  bsize <- n
  # make your datasets
  yexp <- rexp(n, rate = 1/mean(dat))

  # note that all of this is written so that you can put it into a function to iterate over it
  
  ################  --- Continuous Population ---  ###############################
  
  ogsamp <- yexp # specify your sample (cont or discrete)
  
  statvals <- matrix(0, nrow=B, ncol = 4)
  colnames(statvals) <- c("Mean", "Median", "StdDev", "MedAbsDev")
  
  # get the statistics for the original sample
  ogsampmeans <- c(ogsampmeans, mean(ogsamp))
  ogsampdevs <- c(ogsampdevs, sd(ogsamp))
  ogsampmeds <- c(ogsampmeds, median(ogsamp))
  ogsampmad <- c(ogsampmad, mad(ogsamp))
  
  # perform normal bootstrap on them
  for (b in 1:B)
    {
    newSelect <- sample(1:n, size = bsize, replace = TRUE)
    newSample <- ogsamp[newSelect]
    smean <- mean(newSample)
    smed <- median(newSample)
    ssd <- sd(newSample)
    smad <- mad(newSample)
    statvals[b,] <- c(smean, smed, ssd, smad)
  }
  
  BootMeans <- c(BootMeans, mean(statvals[,1]))
  BootMeansDevs <- c(BootMeansDevs, sd(statvals[,1]))
  BootMeds <- c(BootMeds, mean(statvals[,2]))
  BootSdev <- c(BootSdev, mean(statvals[,3]))
  BootMads <- c(BootMads, mean(statvals[,4]))
  }

plot(x=nsizes, y = BootMeans, type="l",
     main = "Bootstrap estimate", xlab = "Sample size", ylab ="Bootstrap estimate of mean")
abline(h=mean(dat), lty = 2, col = "magenta3")
#lines(x=nsizes, y=ogsampmeans,lty = 3, col = "mediumturquoise")

plot(x=nsizes, y = BootMeansDevs, type="l",
     main = "Volatility of Bootstrap Estimate", xlab = "Sample size", ylab ="Deviation of Bootstrap Estimate")

par(mfrow = c(2,1))

plot(x=nsizes, y = abs(ogsampmeans-BootMeans), type="l",
     main = "Mean vs Median", xlab = "Sample size", ylab ="|Difference|")
lines(x=nsizes,y = abs(ogsampmeds-BootMeds), col = "green3", lty = 2 )
abline(h=0, lty = 3, col = "cornflowerblue")
legend("topright", legend = c("Mean", "Median"), lty = c(1,2), col = c("black", "green3"), cex = 0.8)

# plot(x=nsizes, y = abs(ogsampmeds-BootMeds), type="l",
#      main = "Median: Sample T - Bootstrap T", xlab = "Sample size", ylab ="Difference")
# abline(h=0, lty = 2, col = "cornflowerblue")

plot(x=nsizes, y = abs(ogsampdevs-BootSdev), type="l",
     main = "StDev vs. MedianAbsDev", xlab = "Sample size", ylab ="|Difference|")
lines(x=nsizes,y = abs(ogsampmad-BootMads), col = "green3", lty = 2 )
abline(h=0, lty = 2, col = "cornflowerblue")
legend("topright", legend = c("Stdev", "MedianAbsDev"), lty = c(1,2), col = c("black", "green3"), cex = 0.8)

# plot(x=nsizes, y = abs(ogsampmad-BootMads), type="l",
#      main = "Median Abs Dev: Sample T - Bootstrap T", xlab = "Sample size", ylab ="Differene")
# abline(h=0, lty = 2, col = "cornflowerblue")

par(mfrow = c(1,1))

par(mfrow = c(2,2))

hist(statvals[,1], main = "Mean")
hist(statvals[,2], main = "Median")
hist(statvals[,3], main = "Standard deviation")
hist(statvals[,4], main = "Median absolute deviation")

par(mfrow = c(1,1))

# Get the estimated statistics of the bootstrap replications
# Get the estimated standard errors of the bootstrap replications



continuousNormalBootResults <- cbind(ogstatvals, apply(statvals, 2, mean), apply(statvals, 2, sd) )
colnames(continuousNormalBootResults) <- c("Og statistic" ,"Mean", "Std Error")
continuousNormalBootResults

################  --- Discrete Population ---  ###############################

ogsamp <- ygeom # specify your sample (cont or discrete)

statvals2 <- matrix(0, nrow=B, ncol = 4)
colnames(statvals2) <- c("Mean", "Median", "StdDev", "MedAbsDev")

# get the statistics for the original sample
ogmean <- mean(ogsamp)
ogmed <- median(ogsamp)
ogsd <- sd(ogsamp)
ogmad <- mad(ogsamp)
ogstatvals <- c(smean, smed, ssd, smad)

# perform normal bootstrap on them
for (b in 1:B)
{
  newSelect <- sample(1:nsize, size = bsize, replace = TRUE)
  newSample <- ogsamp[newSelect]
  smean <- mean(newSample)
  smed <- median(newSample)
  ssd <- sd(newSample)
  smad <- mad(newSample)
  statvals2[b,] <- c(smean, smed, ssd, smad)
}

par(mfrow = c(2,2))

hist(statvals2[,1], main = "Mean")
hist(statvals2[,2], main = "Median")
hist(statvals2[,3], main = "Standard deviation")
hist(statvals2[,4], main = "Median absolute deviation")

par(mfrow = c(1,1))

# Get the estimated statistics of the bootstrap replications
# Get the estimated standard errors of the bootstrap replications

discreteNormalBootResults <- cbind(ogstatvals, apply(statvals2, 2, mean), apply(statvals2, 2, sd))
colnames(discreteNormalBootResults) <- c("Og statistic" ,"Mean", "Std Error")
discreteNormalBootResults

