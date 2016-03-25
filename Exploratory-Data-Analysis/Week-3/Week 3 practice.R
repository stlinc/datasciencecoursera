#Set working directory
setwd("D:/Coursera Data Science Specialization/Exploratory Data Analysis/Week 3")
set.seed(1234)
par(mar=c(0,0,0,0))
x <- rnorm(12,mean=rep(1:3,each=4),sd=0.2)
y <- rnorm(12,mean=rep(c(1,2,1),each=4),sd=0.2)
plot(x,y, col="blue", pch=19, cex=2)
text(x+0.05,y+0.05,labels=as.character(1:12))
#Calculate pairwise distance
dataFrame <- data.frame(x=x,y=y)
distxy <- dist(dataFrame)
hClustering <- hclust(distxy)
plot(hClustering)
names(hClustering)
#Enhanced plclust, plotting the tree.
myplclust <- function(hclust, lab = hclust$labels,
                      lab.col = rep(1, length(hclust$labels)), hang = 0.1, ...) {
  ## modifiction of plclust for plotting hclust objects *in colour*! Copyright
  ## Eva KF Chan 2009 Arguments: hclust: hclust object lab: a character vector
  ## of labels of the leaves of the tree lab.col: colour for the labels;
  ## NA=default device foreground colour hang: as in hclust & plclust Side
  ## effect: A display of hierarchical cluster with coloured leaf labels.
  y <- rep(hclust$height, 2)
  x <- as.numeric(hclust$merge)
  y <- y[which(x < 0)]
  x <- x[which(x < 0)]
  x <- abs(x)
  y <- y[order(x)]
  x <- x[order(x)]
  plot(hclust, labels = FALSE, hang = hang, ...)
  text(x = x, y = y[hclust$order] - (max(hclust$height) * hang), labels = lab[hclust$order],
       col = lab.col[hclust$order], srt = 90, adj = c(1, 0.5), xpd = NA, ...)
}
# example
myplclust(hClustering, lab = rep(1:3, each = 4), lab.col = rep(1:3, each = 4))

#Heatmap
dataFrame <- data.frame(x=x,y=y)
set.seed(1234)
dataMatrix <-as.matrix(dataFrame)[sample(1:12),]
heatmap(dataMatrix)

#K-means clustering
# specifies initial number of clusters to be 3
kmeansObj <- kmeans(dataFrame,centers=3)
names(kmeansObj)
kmeansObj$cluster
kmeansObj$centers
#Plot k-means clusters
par(mar=rep(0.2,4))
plot(x,y,col=kmeansObj$cluster,pch=19,cex=2)
points(kmeansObj$centers,col=1:3,pch=3,cex=3,lwd=3)
#Plot heatmap of k-means
par(mfrow = c(1,2), mar=c(2,4,0.1,0.1))
image(t(dataMatrix)[, nrow(dataMatrix):1], yaxt = "n")
image(t(dataMatrix)[, order(kmeansObj$cluster)], yaxt = "n")

#Dimension reduction
set.seed(1234)
par(mar=rep(0.2, 4))
dataMatrix <- matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])
#Heatmap doesn't help because there is no underlying patterns
heatmap(dataMatrix)
dim(dataMatrix)
#arbitrarily introduced pattern in data: we flip a coin and if the it is heads, we add the row with [0, 0, 0, 0, 0, 3, 3, 3, 3, 3]
for(i in 1:40){
  # flip a coin
  coinFlip <- rbinom(1,size=1,prob=0.5)
  # if coin is heads add a common pattern to that row
  if(coinFlip){
    dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,3),each=5)
  }
}
# hierarchical clustering
hh <- hclust(dist(dataMatrix))
dataOrdered <- dataMatrix[hh$order,]
# create 1 x 3 panel plot
par(mfrow=c(1,3))
# heat map (sorted)
image(t(dataOrdered)[,nrow(dataOrdered):1])
# row means (40 rows)
plot(rowMeans(dataOrdered),40:1,,xlab="Row Mean",ylab="Row",pch=19)
# column means (10 columns)
plot(colMeans(dataOrdered),xlab="Column",ylab="Column Mean",pch=19)

#Sigular Value Decomposition (SVD)
# running svd
svd1 <- svd(scale(dataOrdered))
# create 1 by 3 panel plot
par(mfrow=c(1,3))
# data heatmap (sorted)
image(t(dataOrdered)[,nrow(dataOrdered):1])
# U Matrix - first column
plot(svd1$u[,1],40:1,,xlab="Row",ylab="First left singular vector",pch=19)
# V vector - first column
plot(svd1$v[,1],xlab="Column",ylab="First right singular vector",pch=19)
# create 1 x 2 panel plot
par(mfrow=c(1,2))
#D  Matrix and Variance Explained
# plot singular values
plot(svd1$d,xlab="Column",ylab="Singular value",pch=19)
# plot proportion of variance explained
plot(svd1$d^2/sum(svd1$d^2),xlab="Column",ylab="Prop. of variance explained",pch=19)
#Relationship to PCA
# SVD
svd1 <- svd(scale(dataOrdered))
# PCA
pca1 <- prcomp(dataOrdered,scale=TRUE)
# Plot the rotation from PCA (Principal Components) vs v vector from SVD. They are the same thing.
plot(pca1$rotation[,1],svd1$v[,1],pch=19,xlab="Principal Component 1",
     ylab="Right Singular Vector 1")
abline(c(0,1))
#Components of SVD, variance explained
constantMatrix <- dataMatrix*0
for(i in 1:dim(dataMatrix)[1]){constantMatrix[i,] <- rep(c(0,1), each = 5)}
svd1 <- svd(constantMatrix)
par(mfrow=c(1,3))
# data heatmap
image(t(constantMatrix)[,nrow(constantMatrix):1])
# D Matrix - first column
plot(svd1$d,xlab="Column",ylab="Singular vector",pch=19)
# plot proportion of variance explained
plot(svd1$d^2/sum(svd1$d^2),xlab="Column",ylab="Prop. of variance explained",pch=19)
#What if we add more patterns?
set.seed(678910)
# setting pattern
data <- matrix(rnorm(400), nrow = 40)
for(i in 1:40){
  # flip a coin
  coinFlip1 <- rbinom(1,size=1,prob=0.5)
  coinFlip2 <- rbinom(1,size=1,prob=0.5)
  # if coin is heads add a common pattern to that row
  if(coinFlip1){
    data[i,] <- data[i,] + rep(c(0,5),each=5)
  }
  if(coinFlip2){
    data[i,] <- data[i,] + rep(c(0,5),5)
  }
}
hh <- hclust(dist(data)); dataOrdered <- data[hh$order,]
# perform SVD
svd2 <- svd(scale(dataOrdered))
par(mfrow=c(2,3))
image(t(dataOrdered)[,nrow(dataOrdered):1])
plot(rep(c(0,1),each=5),pch=19,xlab="Column", main="True Pattern 1")
plot(rep(c(0,1),5),pch=19,xlab="Column",main="True Pattern 2")
image(t(dataOrdered)[,nrow(dataOrdered):1])
plot(svd2$v[,1],pch=19,xlab="Column",ylab="First right singular vector",
     main="Detected Pattern 1")
plot(svd2$v[,2],pch=19,xlab="Column",ylab="Second right singular vector",
     main="Detected Pattern 2")
#d and variance explained
svd1 <- svd(scale(dataOrdered))
par(mfrow=c(1,2))
plot(svd1$d,pch=19,xlab="Column",ylab="Singular value")
plot(svd1$d^2/sum(svd1$d^2),xlab="Column",ylab="Prop. of variance explained",pch=19)
#Missing Values
dataMatrix2 <- dataOrdered
dataMatrix2[sample(1:100, size =40, replace = FALSE)] <- NA
svd1 <- svd(scale(dataMatrix2)) #Doesn't work!
#Error in svd(scale(dataMatrix2)) : infinite or missing values in 'x'
#Use impute library to interpolate missing values by computing the mean of the k-nearest-neighbors (knn).
## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite("impute")
library(impute)  ## Available from http://bioconductor.org
data2 <- dataOrdered
# set random samples = NA
data2[sample(1:100,size=40,replace=FALSE)] <- NA
data2 <- impute.knn(data2)$data
svd1 <- svd(scale(dataOrdered)); svd2 <- svd(scale(data2))
par(mfrow=c(1,2))
plot(svd1$v[,1],pch=19, main="Original")
plot(svd2$v[,1],pch=19, main="Imputed")
#Create Approximations/Data Compression
# load faceData
load("./data/face.rda")
image(t(faceData)[, nrow(faceData):1])
# perform SVD
svd3 <- svd(scale(faceData))
plot(svd3$d^2/sum(svd3$d^2),pch=19,xlab="Singular vector",ylab="Variance explained")
#Approximate images by first 1, 5 or 10 sigular vectors.
approx1 <- svd3$u[,1] %*% t(svd3$v[,1]) * svd3$d[1]
approx5 <- svd3$u[,1:5] %*% diag(svd3$d[1:5])%*% t(svd3$v[,1:5])
approx10 <- svd3$u[,1:10] %*% diag(svd3$d[1:10])%*% t(svd3$v[,1:10])
# create 1 x 4 panel plot
par(mfrow=c(1,4))
# plot original facedata
image(t(approx1)[,nrow(approx1):1], main = "1 Component")
image(t(approx5)[,nrow(approx5):1], main = "5 Component")
image(t(approx10)[,nrow(approx10):1], main = "10 Component")
image(t(faceData)[,nrow(faceData):1], main = "Original")

#Adjusting colors
colors() #check colors (with names) available
# define colorRamp function
pal <- colorRamp(c("red", "blue"))
# create a color
pal(0.67)
#[,1]       [,2]   [,3]
#[1,] 84.15    0 170.85
pal <- colorRamp(c("red", "green", "blue"))
# create a color
pal(0.5)
#[,1] [,2] [,3]
#[1,]    0  255    0
pal(0.25)
#      [,1]  [,2] [,3]
#[1,] 127.5 127.5    0
#Show range of color shemes in 10 increments
pal(seq(0,1,len=10))
# define colorRampPalette function
pal <- colorRampPalette(c("red", "yellow"))
# create 10 colors in between "red" ("#FF0000") and "yellow"("#FFFF00")
pal(10)
#[1] "#FF0000" "#FF1C00" "#FF3800" "#FF5500" "#FF7100" "#FF8D00" "#FFAA00"
#[8] "#FFC600" "#FFE200" "#FFFF00"

#RColorBrewer Package
library(RColorBrewer)
# generate 3 colors using brewer.pal function
cols <- brewer.pal(3, "YlOrRd")
pal <- colorRampPalette(cols)
par(mfrow=c(1,3))
# heat.colors/default
image(volcano, main = "Heat.colors/Default")
# topographical colors
image(volcano, col = topo.colors(20), main = "Topographical Colors")
# RColorBrewer colors
image(volcano, col = pal(20), main = "RColorBrewer Colors")
#smoothScatter function
x <- rnorm(10000); y <- rnorm(10000)
par(mfrow=c(1,2))
plot(x,y)
smoothScatter(x, y)
#rgb function
par(mfrow=c(1,2))
# normal scatter plot
plot(x, y, pch = 19, main = "Default")
# using transparency shows data much better
plot(x, y, col = rgb(0, 0, 0, 0.2), main = "With Transparency")
