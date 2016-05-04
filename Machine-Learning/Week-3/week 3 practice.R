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
install.packages("RGtk2", depen=T)
library(RGtk2)
install.packages("rattle", dependencies = TRUE)
library(rattle)
fancyRpartPlot(modFit$finalModel)

# Predicting new values
predict(modFit,newdata=testing)

# Bootstrap aggregating (bagging)
# Ozone data
install.packages("ElemStatLearn")
library(ElemStatLearn); data(ozone,package="ElemStatLearn")
ozone <- ozone[order(ozone$ozone),]
head(ozone)

# Bagged loess
ll <- matrix(NA,nrow=10,ncol=155)
for(i in 1:10){
    ss <- sample(1:dim(ozone)[1],replace=T)
    ozone0 <- ozone[ss,]; ozone0 <- ozone0[order(ozone0$ozone),]
    loess0 <- loess(temperature ~ ozone,data=ozone0,span=0.2)
    ll[i,] <- predict(loess0,newdata=data.frame(ozone=1:155))
}

# Bagged loess
plot(ozone$ozone,ozone$temperature,pch=19,cex=0.5)
for(i in 1:10){lines(1:155,ll[i,],col="grey",lwd=2)}
lines(1:155,apply(ll,2,mean),col="red",lwd=2) # 2 in apply function indicates taking mean over each column

# More bagging in caret
predictors = data.frame(ozone=ozone$ozone)
temperature = ozone$temperature
treebag <- bag(predictors, temperature, B = 10,
               bagControl = bagControl(fit = ctreeBag$fit,
                                       predict = ctreeBag$pred,
                                       aggregate = ctreeBag$aggregate))

# Example of custom bagging (continued)
plot(ozone$ozone,temperature,col='lightgrey',pch=19)
points(ozone$ozone,predict(treebag$fits[[1]]$fit,predictors),pch=19,col="red")
points(ozone$ozone,predict(treebag,predictors),pch=19,col="blue")
