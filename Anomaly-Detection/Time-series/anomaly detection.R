library(devtools)
# options(download.file.method = "wininet")
# install_github("twitter/AnomalyDetection")
# devtools::install_github("twitter/AnomalyDetection")
# Failed, probably caused by proxy control
# Download zip from https://github.com/twitter/AnomalyDetection
# Unzip it and rename folder names as AnomalyDetection
# Build the package
build("./AnomalyDetection", binary=FALSE)
# Install package locally
install.packages("D:/Security Analytics/Anomaly Detection/AnomalyDetection_1.0.tar.gz", repos = NULL, type = "source")
library(AnomalyDetection)
?AnomalyDetectionTs

# Example data
data(raw_data)
# write 2nd column (values) of raw_data to a csv file, without row or column names, and omit NAs. 
write.table(raw_data[,2], file = "raw_data.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")
res <- AnomalyDetectionTs(raw_data, max_anoms=0.02, direction='both', plot=TRUE)
# To detect only the anomalies on the last day, run the following:
res <- AnomalyDetectionTs(raw_data, max_anoms=0.02, direction='both', only_last="day", plot=TRUE)
res$plot
plot(raw_data, type = "l", xlab = "", ylim=c(20, 300), col="red", alpha = 0.8)

# STL decomposition: https://www.otexts.org/fpp/6/5
fit <- stl(elecequip, t.window=15, s.window="periodic", robust=TRUE)
plot(fit)

#Decompose raw_data
raw_dataTS <- ts(raw_data, start= 1, frequency = 1440)
m <- decompose(raw_dataTS)
m$figure
plot(m)
# Data with no time stamps
?AnomalyDetectionVec
AnomalyDetectionVec(raw_data[,2], max_anoms=0.02, period=1440, direction='both', plot=TRUE)
# To detect only the anomalies in the last period, run the following:
AnomalyDetectionVec(raw_data[,2], max_anoms=0.02, period=1440, direction='both',
                    only_last=TRUE, plot=TRUE)

# There is another package called anomalous for multi-variate anomaly detection.
# Download zip from https://github.com/robjhyndman/anomalous
# Unzip it and rename folder names as anomalous
# Build the package
build("./anomalous", binary=FALSE)
# Install dependent packages
install.packages("ForeCA"); install.packages("pcaPP"); install.packages("hdrcde"); install.packages("RcppRoll")
library(ForeCA); library(pcaPP); library(hdrcde); library(RcppRoll)
# Install package locally
install.packages("D:/Security Analytics/Anomaly Detection/anomalous_0.1.0.tar.gz", repos = NULL, type = "source")
library(anomalous)
?anomalous
# run example
y <- tsmeasures(dat0)
biplot(y)
anomaly(y)

# example 2
z <- ts(matrix(rnorm(3000),ncol=100),freq=4)
y <- tsmeasures(z)
biplot.features(y)
anomaly(y)

# tsoutliers function in the forecast package.
library(forecast)
?tsoutliers

# example
data(gold)
tsoutliers(gold)
# goldDF <-data.frame(gold)
# goldDF <-goldDF[!is.na(goldDF)]
# goldDF <-data.frame(goldDF)
# goldDF <- as.numeric(goldDF)
# AnomalyDetectionVec(goldDF, max_anoms=0.02, period=7, direction='both', plot=TRUE)

# The tso function in the tsoutliers package is another approach to the same problem.
install.packages("tsoutliers")
library(tsoutliers)
?`tsouliers-package`
?tso
# tso(gold)

# Power example
# Manipulate
library(manipulate)
mu0 <- 30
myplot <- function(sigma, mua, n, alpha){
    g = ggplot(data.frame(mu = c(27, 36)), aes(x = mu))
    g = g + stat_function(fun=dnorm, geom = "line", 
                          args = list(mean = mu0, sd = sigma / sqrt(n)), 
                          size = 2, col = "blue")
    g = g + stat_function(fun=dnorm, geom = "line", 
                          args = list(mean = mua, sd = sigma / sqrt(n)), 
                          size = 2, col = "red")
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

mu0 = 30; mua = 31; sigma = 4; n = 16; alpha = 0.05
z = qnorm(1 - alpha)
power <- pnorm(mu0 + z * sigma / sqrt(n), mean = mua, sd = sigma / sqrt(n),
          lower.tail = FALSE)
power

# UKLungDeaths            Monthly Deaths from Lung Diseases in the UK
?UKLungDeaths
plot(ldeaths)
plot(mdeaths, fdeaths)

# Find outliers in WWWusage: Internet Usage per Minute
WebUse <- WWWusage

# "Brexit"

# nottem                  Average Monthly Temperatures at Nottingham,
1920-1939

# precip                  Annual Precipitation in US Cities
