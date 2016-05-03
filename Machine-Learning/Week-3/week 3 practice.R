#Example: Iris Data
data(iris); library(ggplot2)
names(iris)
table(iris$Species)

# Create training and test sets
install.packages("caret", dependencies = TRUE)
library(caret)
inTrain <- createDataPartition(y=iris$Species,p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)

# Iris petal widths/sepal width
qplot(Petal.Width,Sepal.Width,colour=Species,data=training)

# Iris petal widths/sepal width
library(caret)
modFit <- train(Species ~ .,method="rpart",data=training)
print(modFit$finalModel)

# Plot tree
plot(modFit$finalModel, uniform=TRUE, main="Classification Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

# Prettier plots
install.packages("rattle", dependencies = TRUE)
library(rattle)
fancyRpartPlot(modFit$finalModel)
