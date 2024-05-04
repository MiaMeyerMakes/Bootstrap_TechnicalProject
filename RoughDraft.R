library(boot, lib.loc = "C:/Program Files/R/R-4.2.3/library")
aircondit
aircondit7


hist(aircondit$hours)

hist(aircondit7$hours, prob = TRUE)
# , breaks = c( 0, 40, 80, 120, 160, 200, 250)
lognorm <- dlnorm(1:250, meanlog = mean(log(aircondit7$hours)), sdlog = sd(log(aircondit7$hours)))
lines(lognorm)
expo <- dexp(1:250, rate = 1/mean(aircondit7$hours))
lines(expo, col = "blue")

xexpo <- 
  qqplot()

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
       main = "Comparing to the lognormal distribution")
qqline(y, distribution = function(p) qlnorm(p, meanlog = mean(log(aircondit7$hours)), sdlog = sd(log(aircondit7$hours))),
       probs = c(0.1, 0.6), col = 2)

# EXPONENTIAL FITS SOOO MUCH BETTER!!

