# If it isn't installed, install the kernlab package with install.packages()
install.packages("kernlab")
library(kernlab)
data(spam)
names(spam)
str(spam[,1:5])
set.seed(3435)
trainIndicator = rbinom(4601, size = 1, prob = 0.5)
table(trainIndicator)
trainSpam = spam[trainIndicator == 1, ]
testSpam = spam[trainIndicator == 0, ]
table(trainSpam$type)
table(testSpam$type)
#Exploratory analysis
plot(trainSpam$capitalAve ~ trainSpam$type)
plot(log10(trainSpam$capitalAve + 1) ~ trainSpam$type)
#Pairwise comparison
plot(log10(trainSpam[, 1:4] + 1))
#hierarchical cluster analysis (Dendrogram) = what words/characteristics tend to cluster together
hCluster = hclust(dist(t(trainSpam[, 1:57])))
plot(hCluster)
#resclae the distance
hClusterUpdated = hclust(dist(t(log10(trainSpam[, 1:55] + 1))))
plot(hClusterUpdated)

#Run logistic regression to find which feature has the greatest predicting power.
trainSpam$numType = as.numeric(trainSpam$type) - 1
costFunction = function(x, y) sum(x != (y > 0.5))
cvError = rep(NA, 55)
library(boot)
for (i in 1:55) {
  # creates formula with one variable and the result
  lmFormula = reformulate(names(trainSpam)[i], response = "numType")
  glmFit = glm(lmFormula, family = "binomial", data = trainSpam)
  # cross validated error
  cvError[i] = cv.glm(trainSpam, glmFit, costFunction, 2)$delta[2]
}
# Which predictor has minimum cross-validated error?
names(trainSpam)[which.min(cvError)]
# Use the best model from the group
predictionModel = glm(numType ~ charDollar,family="binomial",data=trainSpam)
# Get predictions on the test set
predictionTest = predict(predictionModel,testSpam)
predictedSpam = rep("nonspam",dim(testSpam)[1])
# Classify as 'spam' for those with prob > 0.5
predictedSpam[predictionModel$fitted > 0.5] = "spam"
# Classification table
table(predictedSpam, testSpam$type)
