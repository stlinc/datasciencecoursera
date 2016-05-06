# Prostate cancer
install.packages("ElemStatLearn")
library(ElemStatLearn); data(prostate)
str(prostate)

# Regularized linear regression models in caret.(ridge, lasso, relaxo)
library(caret)

# Ensemble models
# Example with Wage data
# Create training, test and validation sets
install.packages("ISLR"); install.packages("caret", dependencies = TRUE)
library(ISLR); data(Wage); library(ggplot2); library(caret);
Wage <- subset(Wage,select=-c(logwage))

# Create a building data set and validation set
inBuild <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
validation <- Wage[-inBuild,]; buildData <- Wage[inBuild,]

inTrain <- createDataPartition(y=buildData$wage, p=0.7, list=FALSE)
training <- buildData[inTrain,]; testing <- buildData[-inTrain,]
dim(training)
dim(testing)
dim(validation)

# Build two different models
mod1 <- train(wage ~.,method="glm",data=training)
mod2 <- train(wage ~.,method="rf", data=training, trControl = trainControl(method="cv"),number=3)

# Predict on the testing set
pred1 <- predict(mod1,testing); pred2 <- predict(mod2,testing)
qplot(pred1,pred2,colour=wage,data=testing)

# Fit a model that combines predictors
predDF <- data.frame(pred1,pred2,wage=testing$wage)
combModFit <- train(wage ~.,method="gam",data=predDF) #gam: Generalized Additive Model
combPred <- predict(combModFit,predDF)

# Testing errors
sqrt(sum((pred1-testing$wage)^2))
sqrt(sum((pred2-testing$wage)^2))
sqrt(sum((combPred-testing$wage)^2))

# Predict on validation data set
pred1V <- predict(mod1,validation); pred2V <- predict(mod2,validation)
predVDF <- data.frame(pred1=pred1V,pred2=pred2V)
combPredV <- predict(combModFit,predVDF)

# Evaluate on validation
sqrt(sum((pred1V-validation$wage)^2))
sqrt(sum((pred2V-validation$wage)^2))
sqrt(sum((combPredV-validation$wage)^2))

# Forecasting
# Google data
library(quantmod)
from.dat <- as.Date("01/01/08", format="%m/%d/%y")
to.dat <- as.Date("12/31/13", format="%m/%d/%y")
getSymbols("GOOG", src="google", from = from.dat, to = to.dat)
head(GOOG)

# Summarize monthly and store as time series
mGoog <- to.monthly(GOOG[,1:4]) # Do not include 5th column, which is NA
googOpen <- Op(mGoog)
ts1 <- ts(googOpen,frequency=12)
plot(ts1,xlab="Years+1", ylab="GOOG")

# Decompose a time series into parts
plot(decompose(ts1),xlab="Years+1")

# Training and test sets
ts1Train <- window(ts1,start=1,end=5)
ts1Test <- window(ts1,start=5,end=(7-0.01))
ts1Train

# Simple moving average
install.packages("forecast")
library(forecast)
plot(ts1Train)
lines(ma(ts1Train,order=3),col="red")

# Exponential smoothing
ets1 <- ets(ts1Train,model="MMM")
fcast <- forecast(ets1)
plot(fcast); lines(ts1Test,col="red")

# Get the accuracy
accuracy(fcast,ts1Test)

# Unsupervised prediction
#Iris example ignoring species labels
data(iris); library(ggplot2)
inTrain <- createDataPartition(y=iris$Species, p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)

# Cluster with k-means
kMeans1 <- kmeans(subset(training,select=-c(Species)),centers=3)
training$clusters <- as.factor(kMeans1$cluster)
qplot(Petal.Width,Petal.Length,colour=clusters,data=training)


# Compare to real labels
table(kMeans1$cluster,training$Species)

# Build predictor
modFit <- train(clusters ~.,data=subset(training,select=-c(Species)),method="rpart")
table(predict(modFit,training),training$Species)

# Apply on test
testClusterPred <- predict(modFit,testing) 
table(testClusterPred ,testing$Species)

# Quiz
# Q 1. Fit 
# (1) a random forest predictor relating the factor variable y to the remaining variables
# (2) a boosted predictor using the "gbm" method. 
# Fit these both with the train() command in the caret package.
# What are the accuracies for the two approaches on the test data set? 
# What is the accuracy among the test set samples where the two methods agree?

# Load the vowel.train and vowel.test data sets:
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
head(vowel.train); head(vowel.test)
str(vowel.train); str(vowel.test)

# Set the variable y to be a factor variable in both the training and test set. 
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

# Run a random forest predictor
library(caret)
set.seed(33833)
rfFit <- train(y ~ .,data=vowel.train,method="rf",prox=TRUE)

# Predicting new values
predRF <- predict(rfFit, vowel.test)
confusionMatrix(vowel.test$y, predRF)$overall['Accuracy']
# Accuracy is 0.6147186. Correct answer is 0.6082251. But if I ran other people's codes, I still got 0.6147186. Don't know why.

# Run a boosted predictor using the "gbm" method.
set.seed(33833)
bFit <- train(y ~ ., method="gbm",data=vowel.train,verbose=FALSE)

# Predicting new values
predB <- predict(bFit, vowel.test)
confusionMatrix(vowel.test$y, predB)$overall['Accuracy']
# Accuracy is 0.5108225.

# What is the accuracy among the test set samples where the two methods agree?

idx_agreed <- (predRF == predB)
confusionMatrix(vowel.test$y[idx_agreed], predRF[idx_agreed])$overall['Accuracy']
# Accuracy is 0.6656051.

# Accuracy numbers are not exact, but close.

# RF Accuracy = 0.6082 (What I got: 0.6147186)
# GBM Accuracy = 0.5152 (What I got: 0.5108225)
# Agreement Accuracy = 0.6361 (What I got: 0.6656051)

# Q 2. Set the seed to 62433 and predict diagnosis with all the other variables using
library(gbm)
set.seed(62433)
install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# run a random forest (“rf”)
rfFit <- train(diagnosis ~ .,data=training,method="rf",prox=TRUE)
predRF <- predict(rfFit, testing)

# boosted trees (“gbm”)
bFit <- train(diagnosis ~ ., method="gbm",data=training,verbose=FALSE)
predB <- predict(bFit, testing)

# linear discriminant analysis (“lda”) model.
ldaFit = train(diagnosis ~ .,data=training,method="lda")
predLDA <- predict(ldaFit, testing)

# Stack the predictions together using random forests (“rf”). 
# Fit a model that combines predictors
predDF <- data.frame(predRF,predB, predLDA, testing$diagnosis)
combModFit <- train(testing.diagnosis ~.,method="rf",data=predDF)
combPred <- predict(combModFit,predDF)

# What is the resulting accuracy on the test set?
confusionMatrix(testing$diagnosis, predRF)$overall['Accuracy'] # 0.8658537
confusionMatrix(testing$diagnosis, predB)$overall['Accuracy'] # 0.597561
confusionMatrix(testing$diagnosis, predLDA)$overall['Accuracy'] # 0.7804878
confusionMatrix(testing$diagnosis, combPred)$overall['Accuracy'] # 0.8780488

# Is it better or worse than each of the individual predictions?
# A: Stacked Accuracy: 0.88 is better than all three other methods
# 
# set.seed(62433)
# rfmodel <- suppressMessages(train(diagnosis~., data=training, method="rf"))
# gbmmodel <- suppressMessages(train(diagnosis~., data=training, method="gbm"))
# ldamodel <- suppressMessages(train(diagnosis~., data=training, method="lda"))
# rfresult <- predict(rfmodel, testing)
# gbmresult <- predict(gbmmodel, testing)
# ldaresult <- predict(ldamodel, testing)
# combined.data <- data.frame(rfresult, gbmresult, ldaresult, diagnosis=testing$diagnosis)
# combined.model <- train(diagnosis~., data=combined.data, method="rf")
# combined.result <- predict(combined.model, testing)
# confusionMatrix(testing$diagnosis, rfresult)$overall['Accuracy']
# confusionMatrix(testing$diagnosis, gbmresult)$overall['Accuracy']
# confusionMatrix(testing$diagnosis, ldaresult)$overall['Accuracy']
# confusionMatrix(testing$diagnosis, combined.result)$overall['Accuracy']

# Q 3.fit a lasso model to predict Compressive Strength. Which variable is the last coefficient to be set to zero as the penalty increases? (Hint: it may be useful to look up ?plot.enet).
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

# Set the seed to 233
set.seed(233)

# Applying caret package
lassoFit <- train(CompressiveStrength~., data=training, method="lasso")
plot.enet(lassoFit$finalModel, xvar="penalty", use.color=TRUE)
# A: Cement
