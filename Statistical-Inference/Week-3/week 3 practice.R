# Plot normal vs t distributions
k <- 1000; xvals <- seq(-5, 5, length = k); df <- 5
d <- data.frame(y = c(dnorm(xvals), dt(xvals, df)),x = xvals,
                dist = factor(rep(c("Normal", "T"), c(k,k))))
g <- ggplot(d, aes(x = x, y = y))
g <- g + geom_line(size = 2, aes(colour = dist)) + ggtitle("Normal vs T Distribution")
# plot normal vs t quantiles
d <- data.frame(n= qnorm(pvals),t=qt(pvals, df),p = pvals)
h <- ggplot(d, aes(x= n, y = t))
h <- h + geom_abline(size = 2, col = "lightblue")
h <- h + geom_line(size = 2, col = "black")
h <- h + geom_vline(xintercept = qnorm(0.975))
h <- h + geom_hline(yintercept = qt(0.975, df)) + ggtitle("Normal vs T Quantiles")
# plot 2 graphs together
grid.arrange(g, h, ncol = 2)

#t interval example
data(sleep)
head(sleep)
#example: the data used here is for a study of the effects of two soporific drugs (increase in hours of sleep compared to control) on 10 patients
# plot the first and second observations
g <- ggplot(sleep, aes(x = group, y = extra, group = factor(ID)))
g <- g + geom_line(size = 1, aes(colour = ID)) + geom_point(size =10, pch = 21, fill = "salmon", alpha = .5)
g
# define groups
g1 <- sleep$extra[1 : 10]; g2 <- sleep$extra[11 : 20]
# define difference
difference <- g2 - g1
# calculate mean and sd of differences
mn <- mean(difference); s <- sd(difference); n <- 10
# calculate intervals manually
mn + c(-1, 1) * qt(.975, n-1) * s / sqrt(n)
# perform the same test to get confidence intervals
t.test(difference)
t.test(g2, g1, paired = TRUE)

# Independent Group t Intervals - Same Variance
x1bar <- 132.86; x2bar <- 127.44; sd1 <- 15.34; sd2 <- 18.23; nx1 <- 8; nx2 <- 21
sp <- sqrt(((nx1-1)*sd1^2+(nx2-1)*sd2^2)/(nx1+nx2-2))
conf_int <- (x1bar-x2bar) + c(-1, 1)*qt(0.975, df=(nx1+nx2-2))*sp*sqrt(1/nx1+1/nx2)

#ChickWeight data in R
library(reshape2); data(ChickWeight)
#define weight gain or loss
wideCW <- dcast(ChickWeight, Diet + Chick ~ Time, value.var = "weight")
names(wideCW)[-(1:2)] <- paste("time",names(wideCW)[-(1:2)], sep="")
install.packages("dplyr")
library(dplyr)
wideCW <- mutate(wideCW, gain = time21 - time0)
#t-test
wideCW14 <- subset(wideCW, Diet %in% c(1, 4))
rbind(
    t.test(gain ~ Diet, paired = FALSE, var.equal = TRUE, data = wideCW14)$conf,
    t.test(gain ~ Diet, paired = FALSE, var.equal = FALSE, data = wideCW14)$conf
)
# 2-group testing
# H0: father-son height difference is 30
library(UsingR); data(father.son)
t.test(father.son$sheight-father.son$fheight)

#Quiz
#Q 1: In a population of interest, a sample of 9 men yielded a sample average brain volume of 1,100cc and a standard deviation of 30cc. What is a 95% Student's T confidence interval for the mean brain volume in this new population?
n <- 9; mean <- 1100; sd <- 30
conf_int <- mean + c(-1, 1)*qt(0.975, df=n-1)*sd/sqrt(n)

#Q 2: A diet pill is given to 9 subjects over six weeks. The average difference in weight (follow up - baseline) is -2 pounds. What would the standard deviation of the difference in weight have to be for the upper endpoint of the 95% T confidence interval to touch 0?
n <- 9; mean <- -2
sd <- (mean * sqrt(n))/qt(0.975, df=n-1)

#Q 3: In a study of emergency room waiting times, investigators consider a new and the standard triage systems. To test the systems, administrators selected 20 nights and randomly assigned the new triage system to be used on 10 nights and the standard system on the remaining 10 nights. They calculated the nightly median waiting time (MWT) to see a physician. The average MWT for the new system was 3 hours with a variance of 0.60 while the average MWT for the old system was 5 hours with a variance of 0.68. Consider the 95% confidence interval estimate for the differences of the mean MWT associated with the new system. Assume a constant variance. What is the interval? Subtract in this order (New System - Old System).
# Independent Group t Intervals - Same Variance
x1bar <- 3; x2bar <- 5; var1 <- 0.6; var2 <- 0.68; nx1 <- 10; nx2 <- 10
sp <- sqrt(((nx1-1)*var1+(nx2-1)*var2)/(nx1+nx2-2))
conf_int <- (x1bar-x2bar) + c(-1, 1)*qt(0.975, df=(nx1+nx2-2))*sp*sqrt(1/nx1+1/nx2)

#Q 6
x1bar <- 4; x2bar <- 6; sd1 <- 0.5; sd2 <- 2; nx1 <- 100; nx2 <- 100
sp <- sqrt(((nx1-1)*sd1^2+(nx2-1)*sd2^2)/(nx1+nx2-2))
conf_int_t <- (x1bar-x2bar) + c(-1, 1)*qt(0.975, df=(nx1+nx2-2))*sp*sqrt(1/nx1+1/nx2)
#Since both nx1 and nx2 are very large (100), the t distribution can be approxinated by a standard normal
conf_int_norm <- (x1bar-x2bar) + c(-1, 1)*qnorm(0.975)*sp*sqrt(1/nx1+1/nx2)

#Q 7
x1bar <- -3; x2bar <- 1; sd1 <- 1.5; sd2 <- 1.8; nx1 <- 9; nx2 <- 9
sp <- sqrt(((nx1-1)*sd1^2+(nx2-1)*sd2^2)/(nx1+nx2-2))
conf_int_t <- (x1bar-x2bar) + c(-1, 1)*qt(0.95, df=(nx1+nx2-2))*sp*sqrt(1/nx1+1/nx2)
