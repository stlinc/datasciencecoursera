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
