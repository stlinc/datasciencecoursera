# Generate 40-level factor variables
f <- gl(40, 10)
str(f)

library(datasets)
head(airquality)
str(airquality)
summary(airquality)
mean(is.na(airquality))
mean(is.na(airquality[,1]))
mean(is.na(airquality[,2]))
mean(complete.cases(airquality))

# matrix structure
m <- matrix(rnorm(100), 10, 10)
str(m)
m[,1]

# list structure
s <- split(airquality, airquality$Month)
str(s)

# Generate Numbers for a Linear Model
set.seed(20)
x <- rnorm(100)          # normal
e <- rnorm(100, 0, 2)
y <- 0.5 + 2* x + e
summary(y)
plot(x, y)

# x is Binary
set.seed(10)
x <- rbinom(100, 1, 0.5) # binomial
e <- rnorm(100, 0, 2)
y <- 0.5 + 2* x + e
summary(y)
plot(x, y)

# Poisson model
x <- rnorm(100)
log.mu <- 0.5 + 0.3* x
y <- rpois(100, exp(log.mu))
summary(y)
plot(x, y)

# Sampling
set.seed(1)
sample(1:10, 4)
sample(1:10, 4)
sample(letters, 5)
sample(1:10)  ## permutation
sample(1:10)
sample(1:10, replace = TRUE)  ## Sample w/replacement
