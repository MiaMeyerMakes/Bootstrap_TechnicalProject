# library(boot, lib.loc = "C:/Program Files/R/R-4.2.3/library")
library(boot, lib.loc = "/Library/Frameworks/R.framework/Versions/4.2/Resources/library")
library(ggplot2)
aircondit
aircondit7

aircontotal <- sort(c(aircondit$hours, aircondit7$hours))

hist(aircondit$hours)

hist(aircondit7$hours, prob = TRUE)
# , breaks = c( 0, 40, 80, 120, 160, 200, 250)
lognorm <- dlnorm(1:250, meanlog = mean(log(aircondit7$hours)), sdlog = sd(log(aircondit7$hours)))
lines(lognorm, col = 'magenta')
expo <- dexp(1:250, rate = 1/mean(aircondit7$hours))
lines(expo, col = "blue")
legend("topright", legend = c("Lognormal", "Exponential"), col = c("magenta", "blue"), lty=1)

## "EXPONENTIAL" : --------------------------
y <- rexp(500, rate = 1/mean(aircondit7$hours))
## Q-Q plot for exponential against true theoretical distribution:
qqplot(qexp(ppoints(500), rate = 1/mean(aircondit7$hours)), y,
       main = "Comparing to the exponential distribution")
qqline(y, distribution = function(p) qexp(p, rate = 1/mean(aircondit7$hours)),
       probs = c(0.1, 0.6), col = 2)

rexp(500, rate = 1/mean(aircondit7$hours))


## "LOGNORMAL" : --------------------------
y <- rlnorm(500, meanlog = mean(log(aircondit7$hours)), sdlog = sd(log(aircondit7$hours)))
## Q-Q plot for exponential against true theoretical distribution:
qqplot(qlnorm(ppoints(500), meanlog = mean(log(aircondit7$hours)), sdlog = sd(log(aircondit7$hours))), y,
       main = "Comparing to the lognormal distribution", asp = 1)
qqline(y, distribution = function(p) qlnorm(p, meanlog = mean(log(aircondit7$hours)), sdlog = sd(log(aircondit7$hours))),
       probs = c(0.1, 0.6), col = 2)

# EXPONENTIAL FITS SOOO MUCH BETTER!!

## "GEOMETRIC" : --------------------------
y <- rgeom(500, prob = 1/mean(aircondit$hours))
## Q-Q plot for exponential against true theoretical distribution:
qqplot(qgeom(ppoints(500), prob = 1/mean(aircondit$hours)), y,
       main = "Comparing to the geometric distribution")
qqline(y, distribution = function(p) qgeom(p, prob = 1/mean(aircondit$hours)),
       probs = c(0.1, 0.6), col = 2)

# Geometric also fits really well!

########################## GGPLOT EXPERIMENTS. ##################################

carrots <- data.frame(length = rnorm(100000, 6, 2))
cukes <- data.frame(length = rnorm(50000, 7, 2.5))

# Now, combine your two dataframes into one.  
# First make a new column in each that will be 
# a variable to identify where they came from later.
carrots$veg <- 'carrot'
cukes$veg <- 'cuke'

# and combine into your new data frame vegLengths
vegLengths <- rbind(carrots, cukes)

ggplot(vegLengths, aes(length, fill = veg)) + geom_density(alpha = 0.2)
# ______________________________________________________________________________

# Create ggplot object with histogram of the data
p <- ggplot(data = aircondit7, aes(x = hours)) +
  geom_histogram(aes(y = ..density..), bins = 5, fill = "lightblue", color = "Data", alpha = 0.7) +
  labs(title = "Histogram with Overlayed Exponential Density",
       x = "Time between Failures of Airconditioning",
       y = "Density") +
  scale_color_manual(name='Distribution',
                     values = c("Data" = "black", "Exponential" = "red"),
                     labels = c("Data", "Exponential"))

# Overlay density from exponential distribution
lambda <- 1 / mean(aircondit7$hours)  # Estimating lambda for exponential distribution
p <- p + 
  stat_function(fun = dexp, args = list(rate = lambda), color = "magenta", size = 0.8) +
  stat_function(fun = dlnorm, args = list(meanlog = mean(log(aircondit7$hours)), 
                                          sdlog = sd(log(aircondit7$hours))), color = "turquoise", size = 0.8)
   # stat_function(fun = dgeom, args = list(prob = 1/mean(aircondit$hours)), color = "magenta2", size = 0.8)

# Display the plot
print(p)


#create data frame
df <- data.frame(x=c(1, 2, 2, 3, 5, 6, 8, 8, 9, 9, 10, 11, 12, 15, 15),
                 y=c(2, 3, 3, 4, 5, 5, 6, 7, 8, 8, 9, 10, 16, 19, 28))

#create plot with three fitted regression models
ggplot(df, aes(x, y)) +
  geom_point() +
  geom_smooth(se=FALSE, aes(color='Linear')) +
  geom_smooth(formula=y~poly(x, 2), se=FALSE, aes(color='Quadratic')) +
  geom_smooth(formula=y~poly(x, 3), se=FALSE, aes(color='Cubic')) +
  scale_color_manual(name='Regression Model',
                     breaks=c('Linear', 'Quadratic', 'Cubic'),
                     values=c('Cubic'='pink', 'Quadratic'='blue', 'Linear'='purple'))

ggplot(data = aircondit7, aes(x = hours)) +
  geom_histogram(aes(y = ..density.., color = "Data"), bins = 5, fill = "lightblue", alpha = 0.7) +
  labs(title = "Histogram with Overlayed Exponential Density",
       x = "Time between Failures of Airconditioning",
       y = "Density") +
  geom_line(aes( y = dexp(1:length(x), rate = 1/mean(x)),color='expo')) +
  scale_color_manual(name='Distribution',
                     values = c("Data" = "black", "expo" = "red"),
                     labels = c("Data", "Exponential"))
  
  
  # , breaks = c( 0, 40, 80, 120, 160, 200, 250)
  lognorm <- dlnorm(1:250, meanlog = mean(log(aircondit7$hours)), sdlog = sd(log(aircondit7$hours)))
lines(lognorm)
expo <- dexp(1:250, rate = 1/mean(aircondit7$hours))
lines(expo, col = "blue")
