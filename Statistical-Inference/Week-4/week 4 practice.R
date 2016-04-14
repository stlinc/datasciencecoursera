#Power(probability 1- beta of rejecting null hypothesis when alternative hypothesis is true)
library(ggplot2)
# calculates Z1−α
mu0 = 30; mua = 32; sigma = 4; n = 16
alpha = 0.05
z = qnorm(1 - alpha)
#calculates the probability of getting a sample mean that is larger than (Z1−α)*σ/sqrt(n) given that the population mean is μa
nseq = c(8, 16, 32, 64, 128)
mu_a = seq(30, 35, by = 0.1)
power = sapply(nseq, function(n)
    pnorm(mu0 + z * sigma / sqrt(n), mean = mu_a, sd = sigma / sqrt(n),
          lower.tail = FALSE)
)
colnames(power) <- paste("n", nseq, sep = "")
d <- data.frame(mu_a, power)
library(reshape2)
d2 <- melt(d, id.vars = "mu_a")
names(d2) <- c("mu_a", "n", "power")
g <- ggplot(d2,
            aes(x = mu_a, y = power, col = n)) + geom_line(size = 2)
g

#adjusting distribution parameters of 2 competing hypothesis to see changes in power
install.packages("manipulate")
library(manipulate)
mu0 <- 30
myplot <- function(sigma, mua, n, alpha){
    g = ggplot(data.frame(mu = c(27, 36)), aes(x = mu))
    g = g + stat_function(fun=dnorm, geom = "line", 
                          args = list(mean = mu0, sd = sigma / sqrt(n)), 
                          size = 2, col = "red")
    g = g + stat_function(fun=dnorm, geom = "line", 
                          args = list(mean = mua, sd = sigma / sqrt(n)), 
                          size = 2, col = "blue")
    xitc = mu0 + qnorm(1 - alpha) * sigma / sqrt(n)
    g = g + geom_vline(xintercept=xitc, size = 3)
    g
}
manipulate(
    myplot(sigma, mua, n, alpha),
    sigma = slider(1, 10, step = 1, initial = 4),
    mua = slider(30, 35, step = 1, initial = 32),
    n = slider(1, 50, step = 1, initial = 16),
    alpha = slider(0.01, 0.1, step = 0.01, initial = 0.05)
)

#t-test power
# delta: mua - mu0
#The powers of the 3 tests below are identical, because their effect unit (delta/sd) are the same.
power.t.test(n = 16, delta = 2 / 4, sd=1, type = "one.sample",  alt = "one.sided")$power
power.t.test(n = 16, delta = 2, sd=4, type = "one.sample",  alt = "one.sided")$power
power.t.test(n = 16, delta = 100, sd=200, type = "one.sample", alt = "one.sided")$power

# Calculate the sample size n if we want to achieve a power of 0.8
power.t.test(power = .8, delta = 2 / 4, sd=1, type = "one.sample",  alt = "one.sided")$n
power.t.test(power = .8, delta = 2, sd=4, type = "one.sample",  alt = "one.sided")$n
power.t.test(power = .8, delta = 100, sd=200, type = "one.sample", alt = "one.sided")$n

# Calculate the delta if we want to achieve a power of 0.8
power.t.test(n=16, power = .8, sd=1, type = "one.sample",  alt = "one.sided")$delta
power.t.test(n=16, power = .8, sd=4, type = "one.sample",  alt = "one.sided")$delta
power.t.test(n=16, power = .8, sd=200, type = "one.sample", alt = "one.sided")$delta

# Case study I: no true positives
set.seed(1010093)
pValues <- rep(NA,1000)
for(i in 1:1000){
    y <- rnorm(20)
    x <- rnorm(20)
    pValues[i] <- summary(lm(y ~ x))$coeff[2,4]
}

# Controls false positive rate
sum(pValues < 0.05)

# Case study I: no true positives
# Controls FWER 
sum(p.adjust(pValues,method="bonferroni") < 0.05)
# Controls FDR 
sum(p.adjust(pValues,method="BH") < 0.05)

# Case study II: 50% true positives
set.seed(1010093)
pValues <- rep(NA,1000)
for(i in 1:1000){
    x <- rnorm(20)
    # First 500 beta=0, last 500 beta=2
    if(i <= 500){y <- rnorm(20)}else{ y <- rnorm(20,mean=2*x)}
    pValues[i] <- summary(lm(y ~ x))$coeff[2,4]
}
trueStatus <- rep(c("zero","not zero"),each=500)
table(pValues < 0.05, trueStatus)
#Adjust p-values
# Controls FWER 
table(p.adjust(pValues,method="bonferroni") < 0.05,trueStatus)
# Controls FDR 
table(p.adjust(pValues,method="BH") < 0.05,trueStatus)

#P-values versus adjusted P-values
par(mfrow=c(1,2))
plot(pValues,p.adjust(pValues,method="bonferroni"),pch=19)
plot(pValues,p.adjust(pValues,method="BH"),pch=19)

#Re-sampling
#Sample of 50 die rolls
library(ggplot2)
library(gridExtra)
nosim <- 1000

cfunc <- function(x, n) mean(x)
g1 = ggplot(data.frame(y = rep(1/6, 6), x = 1 : 6), aes(y = y, x = x))
g1 = g1 + geom_bar(stat = "identity", fill = "lightblue", colour = "black")

dat <- data.frame(x = apply(matrix(sample(1 : 6, nosim * 50, replace = TRUE), 
                                   nosim), 1, mean))
g2 <- ggplot(dat, aes(x = x)) + geom_histogram(binwidth=.2, colour = "black", fill = "salmon", aes(y = ..density..)) 

grid.arrange(g1, g2, ncol = 2)

# What if we only had one sample?
n = 50
B = 1000
## our data
x = sample(1 : 6, n, replace = TRUE)
## bootstrap resamples
resamples = matrix(sample(x,
                          n * B,
                          replace = TRUE),
                   B, n)
resampledMeans = apply(resamples, 1, mean)
g1 <- ggplot(as.data.frame(prop.table(table(x))), aes(x = x, y = Freq)) + geom_bar(colour = "black", fill = "lightblue", stat = "identity") 
g2 <- ggplot(data.frame(x = resampledMeans), aes(x = x)) + geom_histogram(binwidth=.2, colour = "black", fill = "salmon", aes(y = ..density..)) 
grid.arrange(g1, g2, ncol = 2)

# Consider a data set
library(UsingR)
data(father.son)
x <- father.son$sheight
n <- length(x)
B <- 10000
resamples <- matrix(sample(x,
                           n * B,
                           replace = TRUE),
                    B, n)
resampledMedians <- apply(resamples, 1, median)
#standard deviation of the medians
sd(resampledMedians)
#quantiles of 2.5% and 97.5%
quantile(resampledMedians, c(0.025, 0.975))

# Histogram of bootstrap resamples
g = ggplot(data.frame(resampledMedians = resampledMedians), aes(x = resampledMedians))
g = g + geom_histogram(color = "black", fill = "lightblue", binwidth = 0.05)
g

# Group comparisons
# Example, comparing sprays B and C
data(InsectSprays)
g = ggplot(InsectSprays, aes(spray, count, fill = spray))
g = g + geom_boxplot()
g

# Permutation tests
# Permutation test B v C
subdata <- InsectSprays[InsectSprays$spray %in% c("B", "C"),]
y <- subdata$count
group <- as.character(subdata$spray)
testStat <- function(w, g) mean(w[g == "B"]) - mean(w[g == "C"])
observedStat <- testStat(y, group)
permutations <- sapply(1 : 10000, function(i) testStat(y, sample(group)))
observedStat
mean(permutations > observedStat)

# Histogram of permutations B v C
g = ggplot(data.frame(permutations = permutations),
           aes(permutations))
g = g + geom_histogram(fill = "lightblue", color = "black", binwidth = 1)
g = g + geom_vline(xintercept = observedStat, size = 2)
g

#Quiz 4
#Q 1: A pharmaceutical company is interested in testing a potential blood pressure lowering medication. Their first examination considers only subjects that received the medication at baseline then two weeks later. The data are as follows (SBP in mmHg)
# define groups
g1 <- c(140, 138, 150, 148, 135); g2 <- c(132, 135, 151, 146, 130)
# perform t test to get confidence intervals
t.test(g2, g1, paired = TRUE)

#Q 2: A sample of 9 men yielded a sample average brain volume of 1,100cc and a standard deviation of 30cc. What is the complete set of values of μ0 that a test of H0:μ=μ0 would fail to reject the null hypothesis in a two sided 5% Students t-test?
n <- 9; mean <- 1100; sd <- 30
conf_int <- mean + c(-1, 1)*qt(0.975, df=n-1)*sd/sqrt(n)

#Q 3: The data was such that 3 of the 4 people chose Coke. Assuming that this sample is representative, report a P-value for a test of the hypothesis that Coke is preferred to Pepsi using a one sided exact test.
Coke <- c(1, 1, 1, 0); Pepsi <- c(0,0,0,1)
# perform binomial test whether people is indiffrent betwen Coke and Pepsi (i.e. p = 0.5)
n <- 4
x <- 3
test <- binom.test(x=x, n=n, alt="greater")
round(test$p.value,2)

#Q 4: Infection rates at a hospital above 1 infection per 100 person days at risk are believed to be too high and are used as a benchmark. A hospital that had previously been above the benchmark recently had 10 infections over the last 1,787 person days at risk. About what is the one sided P-value for the relevant test of whether the hospital is *below* the standard?
#example: nuclear pump failed 5 times out of 94.32 days, give a 95% confidence interval for the failure rate per day?
# define parameters
lambda0 <- 1/100
t <- 1787
lambda <- 10/t
lambdaDelta <-lambda-lambda0
# calculate one-sided confidence interval
round(lambdaDelta+ qnorm(.95) * sqrt(lambda / t), 3)
lambda0 <- 1/100
x <- 10
t <- 1787
poisson.test(x, T = t, r = lambda0, alt="less")

#Q 5 18 obese subjects were randomized, 9 each, to a new diet pill and a placebo. BMIs were measured at a baseline and again after having received the treatment or placebo for four weeks. The average difference from follow-up to the baseline (followup - baseline) was −3 kg/m2 for the treated group and 1 kg/m2 for the placebo group. The corresponding standard deviations of the differences was 1.5 kg/m2 for the treatment group and 1.8 kg/m2 for the placebo group. Does the change in BMI appear to differ between the treated and placebo groups? Assuming normality of the underlying data and a common population variance, give a pvalue for a two sided t test.
x1bar <- -3; x2bar <- 1; sd1 <- 1.5; sd2 <- 1.8; nx1 <- 9; nx2 <- 9
sp <- sqrt(((nx1-1)*sd1^2+(nx2-1)*sd2^2)/(nx1+nx2-2))
pval <- pt((x1bar - x2bar) / (sp * (1 / nx1 + 1 / nx2)^.5), df=nx1 + nx2 -2)

#Q 7 Researchers would like to conduct a study of 100 healthy adults to detect a four year mean brain volume loss of .01 mm3. Assume that the standard deviation of four year volume loss in this population is .04 mm3. About what would be the power of the study for a 5% one sided test versus a null hypothesis of no volume loss?
n <- 100; sd <- 0.04; mua <- 0.01
z0.95 <- mu0 + qnorm(0.95, sd = sd)
pnorm(z0.95, mean = mua, sd = sd)
pow <- power.t.test(n=n, delta=mua, sd=sd, sig.level=p, type="one.sample", alt="one.sided")$power
round(pow, 2)

#Q 8  About what would be the value of n needed for 90% power of type one error rate of 5% one sided test versus a null hypothesis of no volume loss?
power.t.test(power = .9, delta = mua, sd=sd, type = "one.sample",  alt = "one.sided")$n
