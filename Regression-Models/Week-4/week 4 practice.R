# Example Baltimore Ravens win/loss
# Ravens Data
download.file("https://dl.dropboxusercontent.com/u/7710864/data/ravensData.rda", destfile="./data/ravensData.rda",method="wininet")
load("./data/ravensData.rda")
head(ravensData)

# Linear regression in R
lmRavens <- lm(ravensData$ravenWinNum ~ ravensData$ravenScore)
summary(lmRavens)$coef

# Visualizing fitting logistic regression curves
x <- seq(-10, 10, length = 1000)
library(manipulate)
manipulate(
    plot(x, exp(beta0 + beta1 * x) / (1 + exp(beta0 + beta1 * x)), 
         type = "l", lwd = 3, frame = FALSE),
    beta1 = slider(-2, 2, step = .1, initial = 2),
    beta0 = slider(-2, 2, step = .1, initial = 0)
)

# Ravens logistic regression
logRegRavens <- glm(ravensData$ravenWinNum ~ ravensData$ravenScore,family="binomial")
summary(logRegRavens)

# Ravens fitted values
plot(ravensData$ravenScore,logRegRavens$fitted,pch=19,col="blue",xlab="Score",ylab="Prob Ravens Win")

# Odds ratios and confidence intervals
exp(logRegRavens$coeff)
exp(confint(logRegRavens))

# ANOVA for logistic regression
anova(logRegRavens,test="Chisq")

# The Poisson mass function
par(mfrow = c(1, 3))
plot(0 : 10, dpois(0 : 10, lambda = 2), type = "h", frame = FALSE)
plot(0 : 20, dpois(0 : 20, lambda = 10), type = "h", frame = FALSE)
plot(0 : 200, dpois(0 : 200, lambda = 100), type = "h", frame = FALSE) 

# Poisson distribution
x <- 0 : 10000; lambda = 3
mu <- sum(x * dpois(x, lambda = lambda))
sigmasq <- sum((x - mu)^2 * dpois(x, lambda = lambda))
c(mu, sigmasq)

# Website data
download.file("https://dl.dropboxusercontent.com/u/7710864/data/gaData.rda",destfile="./data/gaData.rda",method="curl")
load("./data/gaData.rda")
gaData$julian <- julian(gaData$date)
head(gaData)

# Plot data
plot(gaData$julian,gaData$visits,pch=19,col="darkgrey",xlab="Julian",ylab="Visits")

# Example - Linear Regression
# the traffic can be modeled using linear model as follows
# NHi=β0+β1JDi+ϵi
# plot the visits vs dates
plot(gaData$julian,gaData$visits,pch=19,col="darkgrey",xlab="Julian",ylab="Visits")
# perform linear regression
lm1 <- lm(gaData$visits ~ gaData$julian)
# plot regression line
abline(lm1,col="red",lwd=3)

# Example - log Outcome
round(exp(coef(lm(I(log(gaData$visits + 1)) ~ gaData$julian))), 5)

# Example - Poisson Regression
# plot visits vs dates
plot(gaData$julian,gaData$visits,pch=19,col="darkgrey",xlab="Julian",ylab="Visits")
# construct Poisson regression model
glm1 <- glm(gaData$visits ~ gaData$julian,family="poisson")
# plot linear regression line in red
abline(lm1,col="red",lwd=3)
# plot Poisson regression line in
lines(gaData$julian,glm1$fitted,col="blue",lwd=3)

# Example - Robust Standard Errors with Poisson Regression
# plot residuals vs fitted values
plot(glm1$fitted,glm1$residuals,pch=19,col="grey",ylab="Residuals",xlab="Fitted")

# model agnostic standard errors
# load sandwich package
library(sandwich)
# compute
confint.agnostic <- function (object, parm, level = 0.95, ...)
{
    cf <- coef(object); pnames <- names(cf)
    if (missing(parm))
        parm <- pnames
    else if (is.numeric(parm))
        parm <- pnames[parm]
    a <- (1 - level)/2; a <- c(a, 1 - a)
    pct <- stats:::format.perc(a, 3)
    fac <- qnorm(a)
    ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm,
                                                               pct))
    ses <- sqrt(diag(sandwich::vcovHC(object)))[parm]
    ci[] <- cf[parm] + ses %o% fac
    ci
}
# regular confidence interval from Poisson Model
confint(glm1)

# model agnostic standard errors
confint.agnostic(glm1)

# perform Poisson regression with offset for number of visits
glm2 <- glm(gaData$simplystats ~ julian(gaData$date),offset=log(visits+1),
            family="poisson",data=gaData)
# plot the fitted means (from simply statistics)
plot(julian(gaData$date),glm2$fitted,col="blue",pch=19,xlab="Date",ylab="Fitted Counts")
# plot the fitted means (total visit)
points(julian(gaData$date),glm1$fitted,col="red",pch=19)

# Fitting Functions
# Example - Fitting Piecewise Linear Function
# simulate data
n <- 500; x <- seq(0, 4 * pi, length = n); y <- sin(x) + rnorm(n, sd = .3)
# define 20 knot points
knots <- seq(0, 8 * pi, length = 20);
# define the ()+ function to only take the values that are positive after the knot pt
splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot))
# define the predictors as X and spline term
xMat <- cbind(x, splineTerms)
# fit linear models for y vs predictors
yhat <- predict(lm(y ~ xMat))
# plot data points (x, y)
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue")
# plot fitted values
lines(x, yhat, col = "red", lwd = 2)

# Example - Fitting Piecewise Quadratic Function
# define the knot terms in the model
splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot)^2)
# define the predictors as x, x^2 and knot terms
xMat <- cbind(x, x^2, splineTerms)
# fit linear models for y vs predictors
yhat <- predict(lm(y ~ xMat))
# plot data points (x, y)
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue")
# plot fitted values
lines(x, yhat, col = "red", lwd = 2)

# Example - Harmonics using Linear Models
# frequencies for white keys from c4 to c5
notes4 <- c(261.63, 293.66, 329.63, 349.23, 392.00, 440.00, 493.88, 523.25)
# generate sequence for 2 seconds
t <- seq(0, 2, by = .001); n <- length(t)
# define data for c4 e4 g4 using sine waves with their frequencies
c4 <- sin(2 * pi * notes4[1] * t); e4 <- sin(2 * pi * notes4[3] * t);
g4 <- sin(2 * pi * notes4[5] * t)
# define data for a chord and add a bit of noise
chord <- c4 + e4 + g4 + rnorm(n, 0, 0.3)
# generate profile data for all notes
x <- sapply(notes4, function(freq) sin(2 * pi * freq * t))
# fit the chord using the profiles for all notes
fit <- lm(chord ~ x - 1)

# after generating the data and running the linear regression, we can plot the results to see if the notes are correctly identified
# set up plot
plot(c(0, 9), c(0, 1.5), xlab = "Note", ylab = "Coef^2", axes = FALSE, frame = TRUE, type = "n")
# set up axes
axis(2)
axis(1, at = 1 : 8, labels = c("c4", "d4", "e4", "f4", "g4", "a4", "b4", "c5"))
# add vertical lines for each note
for (i in 1 : 8) abline(v = i, lwd = 3, col = grey(.8))
# plot the linear regression fits
lines(c(0, 1 : 8, 9), c(0, coef(fit)^2, 0), type = "l", lwd = 3, col = "red")
# perform fast fourier transforms on the chord matrix
a <- fft(chord)
# plot only the real components of the fft
plot(Re(a)^2, type = "l")
