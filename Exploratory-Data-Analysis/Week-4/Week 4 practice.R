#Set working directory
setwd("D:/Coursera Data Science Specialization/Exploratory Data Analysis/Week 4")
#down data from https://github.com/jtleek/dataanalysis/blob/master/week4/001clusteringExample/data/samsungData.rda
#load data frame provided
load("./data/samsungData.rda")
names(samsungData)[1:12]
# table of 6 types of activities
table(samsungData$activity)

#Plotting Average Acceleration for First Subject
# set up 1 x 2 panel plot
par(mfrow=c(1, 2), mar = c(5, 4, 1, 1))
# converts activity to a factor variable
samsungData <- transform(samsungData, activity = factor(activity))
# find only the subject 1 data
sub1 <- subset(samsungData, subject == 1)
# plot mean body acceleration in X direction
plot(sub1[, 1], col = sub1$activity, ylab = names(sub1)[1],
     main = "Mean Body Acceleration for X")
# plot mean body acceleration in Y direction
plot(sub1[, 2], col = sub1$activity, ylab = names(sub1)[2],
     main = "Mean Body Acceleration for Y")
# add legend
legend("bottomright",legend=unique(sub1$activity),col=unique(sub1$activity), pch = 1)

#Clustering Based on Only Average Acceleration
# load myplclust function
source("myplclust.R")
# calculate distance matrix
distanceMatrix <- dist(sub1[,1:3])
# form hclust object
hclustering <- hclust(distanceMatrix)
# run myplclust on data
myplclust(hclustering, lab.col = unclass(sub1$activity))

#Plotting Max Acceleration for the First Subject
# create 1 x 2 panel
par(mfrow=c(1,2))
# plot max accelecrations in x and y direction
plot(sub1[,10],pch=19,col=sub1$activity,ylab=names(sub1)[10],main = "Max Body Acceleration for X")
plot(sub1[,11],pch=19,col = sub1$activity,ylab=names(sub1)[11],main = "Max Body Acceleration for Y")
# add legend
legend("bottomright",legend=unique(sub1$activity),col=unique(sub1$activity), pch = 1)
#Clustering Based on Maximum Acceleration
# calculate distance matrix for max distances
distanceMatrix <- dist(sub1[,10:12])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering,lab.col=unclass(sub1$activity))
#Singular Value Decomposition
# perform SVD minus last two columns (subject and activity)
svd1 = svd(scale(sub1[,-c(562,563)]))
# create 1 x 2 panel plot
par(mfrow=c(1,2))
# plot first two left singular vector
# separate moving from non moving
plot(svd1$u[,1],col=sub1$activity,pch=19, main = "First Left Singular Vector")
plot(svd1$u[,2],col=sub1$activity,pch=19, main = "Second Left Singular Vector")
#Find max. contributor
par(mfrow=c(1,1))
plot(svd1$v[,2], pch=19)
#New Clustering with Maximum Contributers
# find the max contributing feature
maxContrib <- which.max(svd1$v[,2])
# recalculate distance matrix
distanceMatrix <- dist(sub1[, c(10:12,maxContrib)])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering,lab.col=unclass(sub1$activity))
# name of max contributing factor
names(samsungData)[maxContrib]
#K-means Clustering (nstart=1, first try)
# specify 6 centers for data
kClust <- kmeans(sub1[,-c(562,563)],centers=6)
# tabulate 6 clusteres against 6 activity but many clusters contain multiple activities
table(kClust$cluster,sub1$activity)
#K-means clustering (nstart=100, first try)
# run k-means algorithm 100 times
kClust <- kmeans(sub1[,-c(562,563)],centers=6,nstart=100)
# tabulate results
table(kClust$cluster,sub1$activity)
#Cluster 1 Variable Centers (Laying)
# plot first 10 centers of k-means for laying to understand which features drive the activity
plot(kClust$center[1,1:10],pch=19,ylab="Cluster Center",xlab="")
#Cluster 2 Variable Centers (Walking)
# plot first 10 centers of k-means for laying to understand which features drive the activity
plot(kClust$center[4,1:10],pch=19,ylab="Cluster Center",xlab="")

#AQS yearly data
#http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/download_files.html
#https://raw.githubusercontent.com/jtleek/modules/master/04_ExploratoryAnalysis/CaseStudy/pm25_data/RD_501_88101_1999-0.txt
fileUrl1 <- "https://raw.githubusercontent.com/jtleek/modules/master/04_ExploratoryAnalysis/CaseStudy/pm25_data/RD_501_88101_1999-0.txt"
download.file(fileUrl1, destfile="./data/RD_501_88101_1999-0.txt", method="wininet")
fileUrl2 <- "https://raw.githubusercontent.com/jtleek/modules/master/04_ExploratoryAnalysis/CaseStudy/pm25_data/RD_501_88101_2012-0.txt"
download.file(fileUrl2, destfile="./data/RD_501_88101_2012-0.txt", method="wininet")
# read in raw data from 1999
pm0 <- read.table("./data/RD_501_88101_1999-0.txt", comment.char = "#", header = FALSE, sep = "|", na.strings = "")
# read in headers/column lables
cnames <- readLines("./data/RD_501_88101_1999-0.txt", 1)
# convert string into vector
cnames <- strsplit(substring(cnames, 3), "|", fixed = TRUE)
# make vector the column names
names(pm0) <- make.names(cnames[[1]])
# we are interested in the pm2.5 readings in the "Sample.Value" column
x0 <- pm0$Sample.Value
str(x0)
summary(x0)
#Calculate percentage of NA data in all observations
mean(is.na(x0))

# read in the data from 2012
pm1 <- read.table("./data/RD_501_88101_2012-0.txt", comment.char = "#", header = FALSE, sep = "|",na.strings = "", nrow = 1304290)
# make vector the column names
names(pm1) <- make.names(cnames[[1]])
# take the 2012 data for pm2.5 readings
x1 <- pm1$Sample.Value
str(x1)
summary(x1)
#Calculate percentage of NA data in all observations
mean(is.na(x1))

#Make a boxplot of both 1999 and 2012
par(mfrow = c(1,2))
# regular boxplot, data too right skewed
boxplot(x0, x1, main = "Regular Boxplot")
# log boxplot, significant difference in means, but more spread
boxplot(log10(x0), log10(x1), main = "log Boxplot")

#Check for Negative Values in ‘x1’
# create logical vector for
negative <- x1 < 0
# count number of negatives
sum(negative, na.rm = T)
## [1] 26474
# calculate percentage of negatives
mean(negative, na.rm = T)
## [1] 0.0215034

# capture the date data
dates <- pm1$Date
dates <- as.Date(as.character(dates), "%Y%m%d")
# plot the histogram
hist(dates, "month")  ## Check what's going on in months 1--6
#Check # data with negative values
hist(dates[negative], "month")  ## Check negative x1 on in months 1--6
#Check Same New York Monitors at 1999 and 2012
# find unique monitors in New York in 1999
site0 <- unique(subset(pm0, State.Code == 36, c(County.Code, Site.ID)))
# find unique monitors in New York in 2012
site1 <- unique(subset(pm1, State.Code == 36, c(County.Code, Site.ID)))
# combine country codes and siteIDs of the monitors
site0 <- paste(site0[,1], site0[,2], sep = ".")
site1 <- paste(site1[,1], site1[,2], sep = ".")
# find common monitors in both
both <- intersect(site0, site1)
# print common monitors in 1999 and 2012
print(both)
#Find how many observations available at each monitor
# add columns for combined county/site for the original data
pm0$county.site <- with(pm0, paste(County.Code, Site.ID, sep = "."))
pm1$county.site <- with(pm1, paste(County.Code, Site.ID, sep = "."))
# find subsets where state = NY and county/site = what we found previously
cnt0 <- subset(pm0, State.Code == 36 & county.site %in% both)
cnt1 <- subset(pm1, State.Code == 36 & county.site %in% both)
# split data by the county/size values and count oberservations
sapply(split(cnt0, cnt0$county.site), nrow)
##    1.12     1.5   101.3   13.11    29.5    31.3    5.80 63.2008 67.1015 
##      61     122     152      61      61     183      61     122     122 
##   85.55 
##       7
sapply(split(cnt1, cnt1$county.site), nrow)
##    1.12     1.5   101.3   13.11    29.5    31.3    5.80 63.2008 67.1015 
##      31      64      31      31      33      15      31      30      31 
##   85.55 
##      31
#Choose Monitor where County = 63 and Side ID = 2008
# filter data by state/county/siteID
pm1sub <- subset(pm1, State.Code == 36 & County.Code == 63 & Site.ID == 2008)
pm0sub <- subset(pm0, State.Code == 36 & County.Code == 63 & Site.ID == 2008)
# there are 30 observations from 2012, and 122 from 1999
dim(pm1sub)
dim(pm0sub)
#Plot Data for 2012
# capture the dates of the subset of data
dates1 <- pm1sub$Date
# capture measurements for the subset of data
x1sub <- pm1sub$Sample.Value
# convert dates to appropriate format
dates1 <- as.Date(as.character(dates1), "%Y%m%d")
# plot pm2.5 value vs time
plot(dates1, x1sub, main = "PM2.5 Polution Level in 2012")
#Plot data for 1999
# capture the dates of the subset of data
dates0 <- pm0sub$Date
# convert dates to appropriate format
dates0 <- as.Date(as.character(dates0), "%Y%m%d")
# capture measurements for the subset of data
x0sub <- pm0sub$Sample.Value
# plot pm2.5 value vs time
plot(dates0, x0sub, main = "PM2.5 Polution Level in 1999")
#Panel Plot for Both Years
# find max range for data
rng <- range(x0sub, x1sub, na.rm = T)
# create 1 x 2 panel plot
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
# plot time series plot for 1999
plot(dates0, x0sub, pch = 20, ylim = rng, main="Pollution in 1999")
# plot the median
abline(h = median(x0sub, na.rm = T))
# plot time series plot for 2012
plot(dates1, x1sub, pch = 20, ylim = rng, main="Pollution in 2012")
# plot the median
abline(h = median(x1sub, na.rm = T))
#Find State-wide Means and Trend
# divide data by state and find tne mean of pollution level for 1999
mn0 <- with(pm0, tapply(Sample.Value, State.Code, mean, na.rm = T))
# divide data by state and find tne mean of pollution level for 1999
mn1 <- with(pm1, tapply(Sample.Value, State.Code, mean, na.rm = T))
# convert to data frames while preserving state names
d0 <- data.frame(state = names(mn0), mean = mn0)
d1 <- data.frame(state = names(mn1), mean = mn1)
# merge the 1999 and 2012 means by state
mrg <- merge(d0, d1, by = "state")
# dimension of combined data frame
dim(mrg)
# plot the pollution levels data points for 1999
par(mfrow=c(1, 1))
with(mrg, plot(rep(1999, 52), mrg[, 2], xlim = c(1998,2013),
               main = "PM2.5 Pollution Level by State for 1999 & 2012",
               xlab = "", ylab = "State-wide Mean PM"))
# plot the pollution levels data points for 2012
with(mrg, points(rep(2012, 52), mrg[, 3]))
# connected the dots
segments(rep(1999, 52), mrg[, 2], rep(2012, 52), mrg[, 3])
