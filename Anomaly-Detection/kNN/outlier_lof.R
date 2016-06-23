install.packages("DMwR")
library(DMwR)
# Find outliers in iris data
# remove "Species", which is a categorical column
iris2 <- iris[,1:4]
# Normalize the data
irisNorm <- scale(iris2[, 1:4])
outlier.scores <- lofactor(irisNorm, k=5)
# outlier.scores <- lofactor(iris2, k=5)
# Plot distribution outlier scores
hist(outlier.scores, breaks = 20)
# Top 3 have LOF about 2 or above
plot(density(outlier.scores))
LOF.threshold <- 2
nOutliers <- length(which(outlier.scores >= LOF.threshold)) # How many points whose LOF is greater than or equal to the LOF threshold?
nOutliers <- 3
# [1] 3
# Top 3 have LOF about 2 or above
plot(density(outlier.scores))
# pick top outliers
outliers <- order(outlier.scores, decreasing=T)[1:nOutliers]
# who are outliers
print(outliers)
# [1]  42 118 132
outlier.scores[outliers]
# [1] 3.007637 2.288424 2.102685
iris2[outliers,]
# Sepal.Length Sepal.Width Petal.Length Petal.Width
# 42           4.5         2.3          1.3         0.3
# 118          7.7         3.8          6.7         2.2
# 132          7.9         3.8          6.4         2.0

#Plot pairwise iris data points in lower left corner (colored by species label).
# pairs(iris[, 1:4], main = "Anderson's Iris Data -- 3 species", pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)], upper.panel = NULL)
#Plot pairwise iris data points in lower left corner.
pairs(iris[, 1:4], main = "Iris Data", pch = 21, upper.panel = NULL)

#Create outliersFlag for iris2, non-outliers <- 0; outliers <- 1
n <- nrow(iris2)
outliersFlag <- rep(0, n) # default: no outliers
iris2 <- cbind(iris2[,1:4], outliersFlag)
iris2[outliers, 5] <- 1 # Mark outliers with value 1
iris2[,5] <- as.factor(iris2[,5])
pairs(iris2[,1:4], main = "Outliers", pch = 21, bg = c("green3", "red")[unclass(iris2$outliersFlag)], upper.panel = NULL)

# Plot outliers on first 2 principal components
iris.pca <- prcomp(iris2[,1:4])
plot(iris.pca, type = "l") # plot variances of 4 PC
iris.pca.data <- cbind(iris.pca$x[,1],iris.pca$x[,2]) # PC1 and PC2
# names(iris.pca.data[,1:2]) <- c("PC1", "PC2")
iris.pca.data <- cbind(iris.pca.data[,1:2], outliersFlag) #add outlier flag column, default is 0
iris.pca.data[outliers, 3] <- 1
iris.pca.data <- as.data.frame(iris.pca.data)
iris.pca.data[,3] <- as.factor(iris.pca.data[,3])
plot(iris.pca.data[,1:2], main = "Top 3 Outliers by PCA, k = 5", pch = 21, bg = c("green3", "red")[unclass(iris.pca.data$outliersFlag)])

# show outliers with a biplot of the first two principal components.
n <- nrow(iris2)
labels <- 1:n
labels[-outliers] <- "."
biplot(prcomp(iris2[,1:4]), cex=.8, xlabs=labels)
iris.pca <- prcomp(iris2[,1:4])
plot(iris.pca, type = "l") # plot variances of 4 PC
summary(iris.pca) # Get summary statistices about 4 PCs
# Importance of components:
#     PC1     PC2    PC3     PC4
# Standard deviation     2.0563 0.49262 0.2797 0.15439
# Proportion of Variance 0.9246 0.05307 0.0171 0.00521
# Cumulative Proportion  0.9246 0.97769 0.9948 1.00000

# show outliers with a pairs plot as below, where outliers are labeled with "+" in red.
pch <- rep(".", n)
pch[outliers] <- "+"
col <- rep("black", n)
col[outliers] <- "red"
pairs(iris2[1:4], pch=pch, col=col)

# Rlof provides function lof(), a parallel implementation. lof() has two additional features of supporting multiple values of k and choices of distance metrics. 
install.packages("Rlof")
library(Rlof)
outlier.scores <- lof(iris2, k=5)
# try with different number of neighbors (k = 5,6,7,8,9 and 10)
outlier.scores <- lof(iris2, k=c(5:10))
plot(density(outlier.scores[,1]))
outliers <- order(outlier.scores[,1], decreasing=T)[1:5]
# Get first 5 outliers for k=10
outliers.10 <- order(outlier.scores[,6], decreasing=T)[1:5]

pch <- rep(".", n)
pch[outliers.10] <- "+"
col <- rep("black", n)
col[outliers.10] <- "red"
pairs(iris2, pch=pch, col=col)

# faithful                Old Faithful Geyser Data
names(faithful) <- c("eruptions (min.)", "waiting (min.)")
# Normalize the data
faithfulNorm <- scale(faithful[, 1:2])
outlier.scores <- lofactor(faithfulNorm, k=5)
# Plot distribution outlier scores
hist(outlier.scores, breaks = 20)
LOF.threshold <- 2
nOutliers <- length(which(outlier.scores >= LOF.threshold)) # How many points whose LOF is greater than or equal to the LOF threshold?
# [1] 3
# Top 3 have LOF about 2 or above
plot(density(outlier.scores))
# pick top outliers
outliers <- order(outlier.scores, decreasing=T)[1:nOutliers]
# who are outliers
print(outliers)
outlier.scores[outliers]
faithful[outliers,]

#Plot faithful data points.
plot(faithful[,1:2], main = "Old Faithful Geyser")

#Create outliersFlag for faithful, non-outliers <- 0; outliers <- 1
n <- nrow(faithful)
outliersFlag <- rep(0, n) # default: no outliers
faithful <- cbind(faithful[,1:2], outliersFlag)
faithful[outliers, 3] <- 1 # Mark outliers with value 1
faithful[,3] <- as.factor(faithful[,3])
plot(faithful[,1:2], main = "Outliers", pch = 21, bg = c("green3", "red")[unclass(faithful$outliersFlag)])

# Plot outliers on first 2 principal components
faithful.pca <- prcomp(faithful[,1:2])
plot(faithful.pca, type = "l") # plot variances of PC
faithful.pca.data <- cbind(faithful.pca$x[,1],faithful.pca$x[,2]) # PC1 and PC2
faithful.pca.data <- cbind(faithful.pca.data[,1:2], outliersFlag) #add outlier flag column, default is 0
faithful.pca.data[outliers, 3] <- 1
faithful.pca.data <- as.data.frame(faithful.pca.data)
faithful.pca.data[,3] <- as.factor(faithful.pca.data[,3])
plot(faithful.pca.data[,1:2], main = "Top Outliers by PCA, k = 5", pch = 21, bg = c("green3", "red")[unclass(faithful.pca.data$outliersFlag)])

# Pollution Data
pollution <- read.csv("D:/Coursera Data Science Specialization/Exploratory Data Analysis/Week 1/data/avgpm25.csv", colClasses=c("numeric", "character", "factor", "numeric", "numeric"))
head(pollution)
#Use colors() to check colors available for plotting.
#Scatter plot
with(pollution, plot(latitude, pm25))
pollution.east <- pollution[pollution$region == "east",]
pollution.east <- pollution.east[, -(2:3)] # Remove columns "fips" and "region"
pollution.east.pca <- prcomp(pollution.east)
pollution.east.pca
plot(pollution.east.pca, type = "l") # plot variances of 3 PC
summary(pollution.east.pca) # Get summary statistices about 3 PCs

pollution.west <- pollution[pollution$region == "west",]
with(pollution.east, plot(latitude, pm25))
with(pollution.west, plot(latitude, pm25))
#Color by regions
with(pollution, plot(latitude, pm25, col = region))

#Airquality data
data(airquality)
