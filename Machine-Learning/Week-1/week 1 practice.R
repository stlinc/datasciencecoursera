# SPAM Example
library(kernlab)
data(spam)
head(spam)
plot(density(spam$your[spam$type=="nonspam"]),
     col="blue",main="",xlab="Frequency of 'your'")
lines(density(spam$your[spam$type=="spam"]),col="red")
abline(v=0.5,col="black")
prediction <- ifelse(spam$your > 0.5,"spam","nonspam")
table(prediction,spam$type)/length(spam$type)
# Accuracy: 0.459 + 0.292 = 0.751

# In sample versus out of sample errors
library(kernlab); data(spam); set.seed(333)
smallSpam <- spam[sample(dim(spam)[1],size=10),]
spamLabel <- (smallSpam$type=="spam")*1 + 1
plot(smallSpam$capitalAve,col=spamLabel)

# Prediction rule 1
# capitalAve $>$ 2.7 = "spam"
# capitalAve $<$ 2.40 = "nonspam"
# capitalAve between 2.40 and 2.45 = "spam"
# capitalAve between 2.45 and 2.7 = "nonspam"
# Apply Rule 1 to smallSpam
rule1 <- function(x){
    prediction <- rep(NA,length(x))
    prediction[x > 2.7] <- "spam"
    prediction[x < 2.40] <- "nonspam"
    prediction[(x >= 2.40 & x <= 2.45)] <- "spam"
    prediction[(x > 2.45 & x <= 2.70)] <- "nonspam"
    return(prediction)
}
table(rule1(smallSpam$capitalAve),smallSpam$type)

# Prediction rule 2
# capitalAve $>$ 2.40 = "spam"
# capitalAve $\leq$ 2.40 = "nonspam"
# Apply Rule 2 to smallSpam
rule2 <- function(x){
    prediction <- rep(NA,length(x))
    prediction[x > 2.8] <- "spam"
    prediction[x <= 2.8] <- "nonspam"
    return(prediction)
}
table(rule2(smallSpam$capitalAve),smallSpam$type)

# Apply to complete spam data
table(rule1(spam$capitalAve),spam$type)
table(rule2(spam$capitalAve),spam$type)
mean(rule1(spam$capitalAve)==spam$type)
mean(rule2(spam$capitalAve)==spam$type)

# Look at accuracy
sum(rule1(spam$capitalAve)==spam$type)
sum(rule2(spam$capitalAve)==spam$type)
