#Normal distirbution
# plot standard normal
x <- seq(-3, 3, length = 1000)
library(ggplot2)
g <- ggplot(data.frame(x = x, y = dnorm(x)),
            aes(x = x, y = y)) + geom_line(size = 2)
g <- g + geom_vline(xintercept = -3 : 3, size = 2)
g

#Example: the number of daily ad clicks for a company is (approximately) normally distributed with a mean of 1020 and a standard deviation of 50
#What’s the probability of getting more than 1,160 clicks in a day?
# calculate number of standard deviations from the mean
(1160 - 1020) / 50
# calculate probability using given distribution
pnorm(1160, mean = 1020, sd = 50, lower.tail = FALSE)
# calculate probability using standard normal
pnorm(2.8, lower.tail = FALSE)
#What number of daily ad clicks would represent the one where 75% of days have fewer clicks 
qnorm(0.75, mean = 1020, sd = 50)

#Poisson Distribution
#Example: number of people that show up at a bus stop can be modeled with Poisson distribution with a mean of 2.5 per hour
#after watching the bus stop for 4 hours, what is the probability that 3 or fewer people show up for the whole time?
# calculate using distribution
ppois(3, lambda = 2.5 * 4)
#Example - Approximating Binomial Distribution
#flip a coin with success probability of 0.01 a total 500 times (low pp, large nn)
#what’s the probability of 2 or fewer successes?
# calculate correct probability from Binomial distribution
pbinom(2, size = 500, prob = .01)
# estimate probability using Poisson distribution
ppois(2, lambda=500 * .01)

#Asymptotics
#Example - LLN for Normal and Bernoulli Distribution
# load library
library(gridExtra)
# specify number of trials
n <- 10000
# calculate sample (from normal distribution) means for different size of n
means <- cumsum(rnorm(n)) / (1  : n)
# plot sample size vs sample mean
g <- ggplot(data.frame(x = 1 : n, y = means), aes(x = x, y = y))
g <- g + geom_hline(yintercept = 0) + geom_line(size = 2)
g <- g + labs(x = "Number of obs", y = "Cumulative mean")
g <- g + ggtitle("Normal Distribution")
# calculate sample (coin flips) means for different size of n
means <- cumsum(sample(0 : 1, n , replace = TRUE)) / (1  : n)
# plot sample size vs sample mean
p <- ggplot(data.frame(x = 1 : n, y = means), aes(x = x, y = y))
p <- p + geom_hline(yintercept = 0.5) + geom_line(size = 2)
p <- p + labs(x = "Number of obs", y = "Cumulative mean")
p <- p + ggtitle("Bernoulli Distribution (Coin Flip)")
# combine plots
grid.arrange(g, p, ncol = 2)

#Confidence Intervals
#example: we will compute the 95% confidence interval for sons height data in inches
# load son height data
install.packages("UsingR")
library(UsingR)
data(father.son); x <- father.son$sheight
# calculate confidence interval for sons height in inches
mean(x) + c(-1, 1) * qnorm(0.975) * sd(x)/sqrt(length(x))    

#Confidence Interval - Bernoulli Distribution/Wald Interval
#example suppose a random sample of 100 likely voters, 56 intent to vote for you, can you secure a victory?
# define sample probability and size
p = 0.56; n = 100
# Wald interval
c("WaldInterval" = p + c(-1, 1) * 1/sqrt(n))
# 95% confidence interval
c("95CI" = p + c(-1, 1) * qnorm(.975) * sqrt(p * (1-p)/n))
# perform binomial test
binom.test(p*100, n*100)$conf.int

#Confidence Interval - Binomial Distribution/Agresti-Coull Interval
# simulate 1000 samples of size 20 each
n <- 20; nosim <- 1000
# simulate for p values from 0.1 to 0.9
pvals <- seq(.1, .9, by = .05)
# calculate the confidence intervals
coverage <- sapply(pvals, function(p){
    # simulate binomial data
    phats <- rbinom(nosim, prob = p, size = n) / n
    # calculate lower 95% CI bound
    ll <- phats - qnorm(.975) * sqrt(phats * (1 - phats) / n)
    # calculate upper 95% CI bound
    ul <- phats + qnorm(.975) * sqrt(phats * (1 - phats) / n)
    # calculate percent of intervals that contain p
    mean(ll < p & ul > p)
})
# plot CI results vs 95%
ggplot(data.frame(pvals, coverage), aes(x = pvals, y = coverage)) + geom_line(size = 2) + geom_hline(yintercept = 0.95) + ylim(.75, 1.0)

# simulate 1000 samples of size 20 each with Agresti-Coull Interval
n <- 20; nosim <- 1000
# simulate for p values from 0.1 to 0.9
pvals <- seq(.1, .9, by = .05)
# calculate the confidence intervals
coverage <- sapply(pvals, function(p){
    # simulate binomial data with Agresti/Coull Interval adjustment
    phats <- (rbinom(nosim, prob = p, size = n) + 2) / (n + 4)
    # calculate lower 95% CI bound
    ll <- phats - qnorm(.975) * sqrt(phats * (1 - phats) / n)
    # calculate upper 95% CI bound
    ul <- phats + qnorm(.975) * sqrt(phats * (1 - phats) / n)
    # calculate percent of intervals that contain p
    mean(ll < p & ul > p)
})
# plot CI results vs 95%
ggplot(data.frame(pvals, coverage), aes(x = pvals, y = coverage)) + geom_line(size = 2) + geom_hline(yintercept = 0.95) + ylim(.75, 1.0)

#Confidence Interval - Poisson Interval
#example: nuclear pump failed 5 times out of 94.32 days, give a 95% confidence interval for the failure rate per day?
# define parameters
x <- 5; t <- 94.32; lambda <- x / t
# calculate confidence interval
round(lambda + c(-1, 1) * qnorm(.975) * sqrt(lambda / t), 3)
# return accurate confidence interval from poisson.test
poisson.test(x, T = 94.32)$conf

# small lambda simulations
lambdavals <- seq(0.005, 0.10, by = .01); nosim <- 1000; t <- 100
# calculate coverage using Poisson intervals
coverage <- sapply(lambdavals, function(lambda){
    # calculate Poisson rates
    lhats <- rpois(nosim, lambda = lambda * t) / t
    # lower bound of 95% CI
    ll <- lhats - qnorm(.975) * sqrt(lhats / t)
    # upper bound of 95% CI
    ul <- lhats + qnorm(.975) * sqrt(lhats / t)
    # calculate percent of intervals that contain lambda
    mean(ll < lambda & ul > lambda)
})
# plot CI results vs 95%
ggplot(data.frame(lambdavals, coverage), aes(x = lambdavals, y = coverage)) + geom_line(size = 2) + geom_hline(yintercept = 0.95)+ylim(0, 1.0)

# small lambda simulations, increase t from 100 to 1000
lambdavals <- seq(0.005, 0.10, by = .01); nosim <- 1000; t <- 1000
# calculate coverage using Poisson intervals
coverage <- sapply(lambdavals, function(lambda){
    # calculate Poisson rates
    lhats <- rpois(nosim, lambda = lambda * t) / t
    # lower bound of 95% CI
    ll <- lhats - qnorm(.975) * sqrt(lhats / t)
    # upper bound of 95% CI
    ul <- lhats + qnorm(.975) * sqrt(lhats / t)
    # calculate percent of intervals that contain lambda
    mean(ll < lambda & ul > lambda)
})
# plot CI results vs 95%
ggplot(data.frame(lambdavals, coverage), aes(x = lambdavals, y = coverage)) + geom_line(size = 2) + geom_hline(yintercept = 0.95) + ylim(0, 1.0)
