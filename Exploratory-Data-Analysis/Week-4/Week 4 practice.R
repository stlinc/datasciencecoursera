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
