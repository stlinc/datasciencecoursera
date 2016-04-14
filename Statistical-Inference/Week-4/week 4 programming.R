##Part One
# investigate the exponential distribution in R and compare it with the Central Limit Theorem
# rexp(n, lambda) where lambda is the rate parameter
# The mean of exponential distribution is 1/lambda and the standard deviation is also 1/lambda.

#Warmup exercise
# distribution of 1000 random uniforms
hist(runif(1000))
# distribution of 1000 averages of 40 random uniforms
mns = NULL
for (i in 1 : 1000) mns = c(mns, mean(runif(40)))
hist(mns)


#1. Show the sample mean and compare it to the theoretical mean of the distribution.
lambda <- 0.2
n_trial <- 40
n <- 1000
par(mfrow=c(1,2))
#Theoretical exponential distribution
meanExp <- 1/lambda
sdExp <- (1/lambda)/sqrt(n_trial)
hist(rexp(n, lambda), breaks=20, main="Exponential distribution", xlab="")
#Distribution of simulated means of exponential distribution
set.seed(333)
mns = NULL
for (i in 1 : n) mns = c(mns, mean(rexp(n_trial, lambda)))
meanSample <- mean(mns)
sdSample <- sd(mns)
hist(mns, breaks=20, main="Sample means distribution", xlab="")

#2. Show how variable the sample is (via variance) and compare it to the theoretical variance of the distribution.
varExp <- sdExp^2
varSample <- var(mns)

#3. Show that the distribution is approximately normal.
par(mfrow=c(1,1))
hist(mns, freq=FALSE, breaks=20, main=NULL, xlab = "mean of sampled exponential distribution")
#normal distribution
curve(dnorm(x,mean=meanExp,sd=sdExp),from=0, to=10, col='red',add=T) 

##Part Two
#1. Load the ToothGrowth data and perform some basic exploratory data analyses
dataTG <- ToothGrowth
dim(dataTG)
summary(dataTG)
str(dataTG)
head(dataTG)
table(dataTG$supp,dataTG$dose)
#2. Provide a basic summary of the data.
library(ggplot2)
dataTG$dose <- as.factor(dataTG$dose)
g <- ggplot(dataTG, aes(x=dose, y=len, fill=dose))
g <- g + geom_boxplot()
g <- g + facet_grid(.~supp)
g <- g + ggtitle("Distribution of tooth length over supp and dose")
g

#3. Use confidence intervals and/or hypothesis tests to compare tooth growth by supp and dose. (Only use the techniques from class, even if there's other approaches worth considering)
#Pairwise tests for each supp type
# Supp=OJ
# define groups
gOJ <- dataTG[dataTG$supp == "OJ",]
gVC <- dataTG[dataTG$supp == "VC",]
# perform t test on OJ and VC groups
t.test(gOJ$len, gVC$len, paired = FALSE)
#The 95 percent confidence interval: (-0.1710156  7.5710156). We cannot reject the null hypothesis that the difference of lengths between VC and OJ is zero.

# perform t test on OJ and VC groups over different doses.
# OJ
gOJ05 <- gOJ[gOJ$dose == 0.5,]
gOJ10 <- gOJ[gOJ$dose == 1.0,]
gOJ20 <- gOJ[gOJ$dose == 2.0,]
#Pairwise t tests over doses
t.test(gOJ05$len, gOJ10$len, paired = FALSE)
#Reject null hypothesis that dose change has no effect on length, given 95% confidence interval.
t.test(gOJ10$len, gOJ20$len, paired = FALSE)
#Reject null hypothesis that dose change has no effect on length, given 95% confidence interval.
t.test(gOJ05$len, gOJ20$len, paired = FALSE)
#Reject null hypothesis that dose change has no effect on length, given 95% confidence interval.
# All 3 t tests indicate we can reject null hypothesis that does does not have effect on length, given supp is OJ.

# VC
gVC05 <- gVC[gVC$dose == 0.5,]
gVC10 <- gVC[gVC$dose == 1.0,]
gVC20 <- gVC[gVC$dose == 2.0,]
#Pairwise t tests over doses
t.test(gVC05$len, gVC10$len, paired = FALSE)
#Reject null hypothesis that dose change has no effect on length, given 95% confidence interval.
t.test(gVC10$len, gVC20$len, paired = FALSE)
#Reject null hypothesis that dose change has no effect on length, given 95% confidence interval.
t.test(gVC05$len, gVC20$len, paired = FALSE)
#Reject null hypothesis that dose change has no effect on length, given 95% confidence interval.
# All 3 t tests indicate we can reject null hypothesis that does does not have effect on length, given supp is OJ.

#4. State your conclusions and the assumptions needed for your conclusions.
# By t tests along supp, there is no significance of difference in effects of OJ and VC. However, the dose does have significant effect on th length, be supp VC or OJ.
