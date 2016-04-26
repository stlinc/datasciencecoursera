# Demonstration that it works using an example
# Linear model with two variables
n = 100; x = rnorm(n); x2 = rnorm(n); x3 = rnorm(n)
y = 1 + x + x2 + x3 + rnorm(n, sd = .1)
ey = resid(lm(y ~ x2 + x3))
ex = resid(lm(x ~ x2 + x3))
sum(ey * ex) / sum(ex ^ 2)
coef(lm(ey ~ ex - 1))
coef(lm(y ~ x + x2 + x3)) 

# Data set for discussion
install.packages("GGally")
require(datasets); data(swiss); ?swiss
require(datasets); data(swiss); require(GGally); require(ggplot2)
g = ggpairs(swiss, lower = list(continuous = "smooth"),params = c(method = "loess")) #Something is wrong with this formula, but can't figure out why.
g

# Calling lm
summary(lm(Fertility ~ . , data = swiss))
summary(lm(Fertility ~ . , data = swiss))$coefficients
# Regressing agriculture only would produce a different sign of coefficient.
summary(lm(Fertility ~ Agriculture, data = swiss))$coefficients

# How can adjustment reverse the sign of an effect? Let's try a simulation.
n <- 100; x2 <- 1 : n; x1 <- .01 * x2 + runif(n, -.1, .1); y = -x1 + x2 + rnorm(n, sd = .01)
summary(lm(y ~ x1))$coef
summary(lm(y ~ x1 + x2))$coef

dat = data.frame(y = y, x1 = x1, x2 = x2, ey = resid(lm(y ~ x2)), ex1 = resid(lm(x1 ~ x2)))
library(ggplot2)
g = ggplot(dat, aes(y = y, x = x1, colour = x2))
g = g + geom_point(colour="grey50", size = 5) + geom_smooth(method = lm, se = FALSE, colour = "black") 
g = g + geom_point(size = 4) 
g

# Adjusting for x2
g2 = ggplot(dat, aes(y = ey, x = ex1, colour = x2))  
g2 = g2 + geom_point(colour="grey50", size = 5) + geom_smooth(method = lm, se = FALSE, colour = "black") + geom_point(size = 4) 
g2 = g2 + ggtitle("adjusted = y, x1 residuals with x2 removed") + labs(x = "resid(x1~x2)",
                                                                       y = "resid(y~x2)")
g2

# What if we include an unnecessary variable?
z <- swiss$Agriculture + swiss$Education
lm(Fertility ~ . + z, data = swiss)
# when running a linear regression with unnecessary variables, R automatically drops the linear combinations and returns NA as their coefficients.

# Dummy Variables
# Example: 6 Factor Level Insect Spray Data
# load insect spray data
require(datasets);data(InsectSprays); require(stats); require(ggplot2)
g = ggplot(data = InsectSprays, aes(y = count, x = spray, fill  = spray))
g = g + geom_violin(colour = "black", size = 2)
g = g + xlab("Type of spray") + ylab("Insect count")
g

# Linear model fit, group A is the reference
summary(lm(count ~ spray, data = InsectSprays))$coef

#Hard coding the dummy variables
# The number 1 is to force a logic variable to be expressed as numbers, i.e. TRUE as i and FALSE as 0.
summary(lm(count ~ 
               I(1 * (spray == 'B')) + I(1 * (spray == 'C')) + 
               I(1 * (spray == 'D')) + I(1 * (spray == 'E')) +
               I(1 * (spray == 'F'))
           , data = InsectSprays))$coef

#What if we include all 6?
summary(lm(count ~ 
               I(1 * (spray == 'B')) + I(1 * (spray == 'C')) +  
               I(1 * (spray == 'D')) + I(1 * (spray == 'E')) +
               I(1 * (spray == 'F')) + I(1 * (spray == 'A')), data = InsectSprays))$coef
# the coefficient for group A is NA

# What if we omit the intercept?
summary(lm(count ~ spray - 1, data = InsectSprays))$coef
library(dplyr)
summarise(group_by(InsectSprays, spray), mn = mean(count))

# # reorder the levels of spray variable such that C is the lowest level
spray2 <- relevel(InsectSprays$spray, "C")
# rerun linear regression with releveled factor
summary(lm(count ~ spray2, data = InsectSprays))$coef

# Recall the swiss data set
library(datasets); data(swiss)
head(swiss)

#Create a binary variable
library(dplyr); 
swiss = mutate(swiss, CatholicBin = 1 * (Catholic > 50))

# Plot the data
g = ggplot(swiss, aes(x = Agriculture, y = Fertility, colour = factor(CatholicBin)))
g = g + geom_point(size = 6, colour = "black") + geom_point(size = 4)
g = g + xlab("% in Agriculture") + ylab("Fertility")
g

# No effect of religion
summary(lm(Fertility ~ Agriculture, data = swiss))$coef

# The associated fitted line
fit = lm(Fertility ~ Agriculture, data = swiss)
g1 = g
g1 = g1 + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], size = 2)
g1

# Parallel lines
summary(lm(Fertility ~ Agriculture + factor(CatholicBin), data = swiss))$coef

# Fitted lines
fit = lm(Fertility ~ Agriculture + factor(CatholicBin), data = swiss)
g1 = g
g1 = g1 + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], size = 2)
g1 = g1 + geom_abline(intercept = coef(fit)[1] + coef(fit)[3], slope = coef(fit)[2], size = 2)
g1

# Lines with different slopes and intercepts (i.e. interaction between Agriculture and CatholicBin)
summary(lm(Fertility ~ Agriculture * factor(CatholicBin), data = swiss))$coef

#Fitted lines
fit = lm(Fertility ~ Agriculture * factor(CatholicBin), data = swiss)
g1 = g
g1 = g1 + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], size = 2)
g1 = g1 + geom_abline(intercept = coef(fit)[1] + coef(fit)[3], 
                      slope = coef(fit)[2] + coef(fit)[4], size = 2)
g1

# Just to show you it can be done
summary(lm(Fertility ~ Agriculture + Agriculture : factor(CatholicBin), data = swiss))$coef

# Adjustment
# Simulation 1
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), runif(n/2));
beta0 <- 0; beta1 <- 2; tau <- 1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)

# Simulation 2
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), 1.5 + runif(n/2));
beta0 <- 0; beta1 <- 2; tau <- 0; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)

# Simulation 3
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), .9 + runif(n/2));
beta0 <- 0; beta1 <- 2; tau <- -1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)

# Simulation 4
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(.5 + runif(n/2), runif(n/2));
beta0 <- 0; beta1 <- 2; tau <- 1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)

# Simulation 5
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2, -1, 1), runif(n/2, -1, 1));
beta0 <- 0; beta1 <- 2; tau <- 0; tau1 <- -4; sigma <- .2
y <- beta0 + x * beta1 + t * tau + t * x * tau1 + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t + I(x * t))
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2] + coef(fit)[4], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)

# Simulation 6
p <- 1
n <- 100; x2 <- runif(n); x1 <- p * runif(n) - (1 - p) * x2
beta0 <- 0; beta1 <- 1; tau <- 4 ; sigma <- .01
y <- beta0 + x1 * beta1 + tau * x2 + rnorm(n, sd = sigma)
plot(x1, y, type = "n", frame = FALSE)
abline(lm(y ~ x1), lwd = 2)
co.pal <- heat.colors(n)
points(x1, y, pch = 21, col = "black", bg = co.pal[round((n - 1) * x2 + 1)], cex = 2)

# Do this to investigate the bivariate relationship
install.packages("rgl")
library(rgl)
plot3d(x1, x2, y)

# Residual relationship
plot(resid(lm(x1 ~ x2)), resid(lm(y ~ x2)), frame = FALSE, col = "black", bg = "lightblue", pch = 21, cex = 2)
abline(lm(I(resid(lm(x1 ~ x2))) ~ I(resid(lm(y ~ x2)))), lwd = 2)

# The linear model
data(swiss); par(mfrow = c(2, 2))
fit <- lm(Fertility ~ . , data = swiss); plot(fit)

# Influential, high leverage and outlying points
par(mfrow = c(1, 1))
n <- 100; x <- rnorm(n); y <- x + rnorm(n, sd = .3)
plot(c(-3, 6), c(-3, 6), type = "n", frame = FALSE, xlab = "X", ylab = "Y")
abline(lm(y ~ x), lwd = 2)
points(x, y, cex = 2, bg = "lightblue", col = "black", pch = 21)
points(0, 0, cex = 2, bg = "darkorange", col = "black", pch = 21)
points(0, 5, cex = 2, bg = "darkorange", col = "black", pch = 21)
points(5, 5, cex = 2, bg = "darkorange", col = "black", pch = 21)
points(5, 0, cex = 2, bg = "darkorange", col = "black", pch = 21)

# Influence measures
?influence.measures
inflm.SR <- influence.measures(fit)
which(apply(inflm.SR$is.inf, 1, any)) # which observations 'are' influential
# hatvalues is extremely useful not only in identifying high-leveraged outliers, but also in data entry errors.
hatvalues(fit)

# Case 1
x <- c(10, rnorm(n)); y <- c(10, c(rnorm(n)))
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
abline(lm(y ~ x))

# Showing a couple of the diagnostic values
fit <- lm(y ~ x)
round(dfbetas(fit)[1 : 10, 2], 3)
round(hatvalues(fit)[1 : 10], 3)

# Case 2
x <- rnorm(n); y <- x + rnorm(n, sd = .3)
x <- c(5, x); y <- c(5, y)
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
fit2 <- lm(y ~ x)
abline(fit2)            

# Looking at some of the diagnostics
round(dfbetas(fit2)[1 : 10, 2], 3)
round(hatvalues(fit2)[1 : 10], 3)

# Example described by Stefanski TAS 2007 Vol 61.
## Don't everyone hit this server at once.  Read the paper first.
dat <- read.table('http://www4.stat.ncsu.edu/~stefanski/NSF_Supported/Hidden_Images/orly_owl_files/orly_owl_Lin_4p_5_flat.txt', header = FALSE) # cannot open the connection
pairs(dat)

# Got our P-values, should we bother to do a residual plot?
summary(lm(V1 ~ . -1, data = dat))$coef

# Residual plot
# P-values significant, O RLY?
fit <- lm(V1 ~ . - 1, data = dat); plot(predict(fit), resid(fit), pch = '.')

# Back to the Swiss data
data(swiss); par(mfrow = c(2, 2))
fit <- lm(Fertility ~ . , data = swiss); plot(fit)

# Plot of R^2 versus n
# For simulations as the number of variables included equals increases to n=100. No actual regression relationship exist in any simulation
n <- 100
plot(c(1, n), 0 : 1, type = "n", frame = FALSE, xlab = "p", ylab = "R^2")
r <- sapply(1 : n, function(p)
{
    y <- rnorm(n); x <- matrix(rnorm(n * p), n, p)
    summary(lm(y ~ x))$r.squared 
}
)
lines(1 : n, r, lwd = 2)
abline(h = 1)

# Variance inflation
n <- 100; nosim <- 1000
x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n); 
betas <- sapply(1 : nosim, function(i){
    y <- x1 + rnorm(n, sd = .3)
    c(coef(lm(y ~ x1))[2], 
      coef(lm(y ~ x1 + x2))[2], 
      coef(lm(y ~ x1 + x2 + x3))[2])
})
round(apply(betas, 1, sd), 5) # No significant differences in beta for x1, because x2 and x3 are not correlated with x1.

# Variance inflation
n <- 100; nosim <- 1000
x1 <- rnorm(n); x2 <- x1/sqrt(2) + rnorm(n) /sqrt(2)
x3 <- x1 * 0.95 + rnorm(n) * sqrt(1 - 0.95^2); 
betas <- sapply(1 : nosim, function(i){
    y <- x1 + rnorm(n, sd = .3)
    c(coef(lm(y ~ x1))[2], 
      coef(lm(y ~ x1 + x2))[2], 
      coef(lm(y ~ x1 + x2 + x3))[2])
})
round(apply(betas, 1, sd), 5) # Significant differences in beta for x1, because x2 and x 3 are highly correlated with x1.

# Revisting our previous simulation

##doesn't depend on which y you use,
y <- x1 + rnorm(n, sd = .3)
a <- summary(lm(y ~ x1))$cov.unscaled[2,2]
c(summary(lm(y ~ x1 + x2))$cov.unscaled[2,2],
  summary(lm(y~ x1 + x2 + x3))$cov.unscaled[2,2]) / a
temp <- apply(betas, 1, var); temp[2 : 3] / temp[1]

# Swiss data
data(swiss); 
fit1 <- lm(Fertility ~ Agriculture, data = swiss)
a <- summary(fit1)$cov.unscaled[2,2]
fit2 <- update(fit1, Fertility ~ Agriculture + Examination)
fit3 <- update(fit1, Fertility ~ Agriculture + Examination + Education)
c(summary(fit2)$cov.unscaled[2,2],
  summary(fit3)$cov.unscaled[2,2]) / a 

# Swiss data VIFs,
install.packages("car")
install.packages("pbkrtest")
install.packages("lme4")
library(car)
fit <- lm(Fertility ~ . , data = swiss)
vif(fit)
sqrt(vif(fit)) #I prefer sd 

# How to do nested model testing in R
fit1 <- lm(Fertility ~ Agriculture, data = swiss)
fit3 <- update(fit, Fertility ~ Agriculture + Examination + Education)
fit5 <- update(fit, Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality)
anova(fit1, fit3, fit5)

#Quiz
# Q 1. Consider the mtcars data set. Fit a model with mpg as the outcome that includes number of cylinders as a factor variable and weight as confounder. Give the adjusted estimate for the expected change in mpg comparing 8 cylinders to 4.
data(mtcars)
summary(lm(mpg ~ factor(cyl)+wt, data = mtcars))$coef[3,1]

# Q 2. Fit a model with mpg as the outcome that includes number of cylinders as a factor variable and weight as a possible confounding variable. Compare the effect of 8 versus 4 cylinders on mpg for the adjusted and unadjusted by weight models. Here, adjusted means including the weight variable as a term in the regression model and unadjusted means the model without weight included. What can be said about the effect comparing 8 and 4 cylinders after looking at models with and without weight included?.
fit1 <- lm(mpg ~ factor(cyl), data = mtcars)
fit2 <- lm(mpg ~ factor(cyl)+wt, data = mtcars)
summary(fit1)$coef
summary(fit2)$coef

# Q 3. Consider the mtcars data set. Fit a model with mpg as the outcome that considers number of cylinders as a factor variable and weight as confounder. Now fit a second model with mpg as the outcome model that considers the interaction between number of cylinders (as a factor variable) and weight. Give the P-value for the likelihood ratio test comparing the two models and suggest a model using 0.05 as a type I error rate significance benchmark.
fit1 <- lm(mpg ~ factor(cyl)+wt, data = mtcars)
fit2 <- lm(mpg ~ factor(cyl)*wt, data = mtcars)
summary(fit1)
summary(fit2)
install.packages("lmtest")
library(lmtest)
lrtest(fit2, fit1)

# Q 4.How is the wt coefficient interpretted?
fit1 <- lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
fit2 <- lm(mpg ~ wt + factor(cyl), data = mtcars)
summary(fit1)$coef
summary(fit2)$coef
# wt is unit of 1,000 lb, 0.5*wt doubles beta. So the answer is "The estimated expected change in MPG per one ton increase in weight for a specific number of cylinders (4, 6, 8)."

# Q 5.Give the hat diagonal for the most influential point
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit <- lm(y ~ x)
max(hatvalues(fit))

# Q 6.Give the slope dfbeta for the point with the highest hat value.
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit <- lm(y ~ x)
r <- which.max(hatvalues(fit))
inflm <- influence.measures(fit)
dfbetas(fit)[r,2]
