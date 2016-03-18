#Subsetting
set.seed(12345)
X <- data.frame("var1"=sample(1:5), "var2"=sample(6:10), "var3"=sample(11:15))
X <- X[sample(1:5),]
X$var2[c(1,3)] <- NA
X
X[,1]
X[,"var1"]
X[1:2,"var2"]

#Logical ands and ors
X[X$var1 <= 3 & X$var3 > 11,]
X[X$var1 <= 3 | X$var3 > 15,]

#Dealing with missing values
X[which(X$var2>8),]

#Sorting
sort(X$var1)
sort(X$var1, decreasing=TRUE)
sort(X$var2, na.last=TRUE)

#Ordering
X[order(X$var1),]
X[order(X$var2, X$var1),]

#Ordering with plyr
install.packages("plyr")
library(plyr)
arrange(X,var1)
arrange(X, desc(var1))
arrange(X, var2, var1)
arrange(X, var2, desc(var1))

#Adding rows and columns
X$var4 <- rnorm(5)
X
Y <- cbind(X, var5 = rnorm(5))
Z <- rbind(rnorm(4),X)

#Summarizing data
#Getting the dataset
getwd()
setwd("D:/../../..")
if(!file.exists("./data")){dir.create("./data/")}
fileUrl <- "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl, destfile="./data/restaurants.csv", method="curl")
restData <- read.csv("./data/restaurants.csv")
head(restData, n=3)
tail(restData, n=3)
#Summary statistics
summary(restData)
str(restData)
quantile(restData$councilDistrict, na.rm=TRUE)
quantile(restData$councilDistrict, probs = c(0.5, 0.75,0.9))
#Make table
table(restData$zipCode, useNA="ifany")
table(restData$councilDistrict, restData$zipCode, useNA="ifany")
table(restData$policeDistrict, restData$zipCode, useNA="ifany")
#Checking for missing values
sum(is.na(restData$councilDistrict))
any(is.na(restData$councilDistrict))
all(restData$zipCode>0)
any(restData$zipCode<0)
sum(restData$zipCode<0)
#Row and column sums
colSums(is.na(restData))
all(colSums(is.na(restData))==0)
#Values with specific characteristics
table(restData$zipCode %in% "21212")
table(restData$zipCode == "21212")
table(restData$zipCode %in% c("21212", "21213"))
#table(restData$zipCode == c("21212", "21213"))   #Wrong expression
#subsetting data
restData[restData$zipCode %in% c("21212", "21213"),]

#Cross tabs
data(UCBAdmissions)
DF <- as.data.frame(UCBAdmissions)
summary(DF)
xt <- xtabs(Freq ~ Gender + Admit, DF)
xt
summary(xtabs(Freq ~ ., DF))

#Flat tables
head(warpbreaks, n=3)
nrow(warpbreaks)
ncol(warpbreaks)
warpbreaks$replicate <- rep(1:9, len=54)
xt <- xtabs(breaks ~., warpbreaks)
xt
ftable(xt)

#Size of data set
fakeData = rnorm(1e5)
object.size(fakeData)
print(object.size(fakeData), units="Mb")
object.size(restData)
print(object.size(restData), units="Mb")
object.size(UCBAdmissions)
print(object.size(UCBAdmissions), units="Mb")
object.size(warpbreaks)
print(object.size(warpbreaks), units="Mb")

#Creating sequences
s1 <- seq(1,10, by=2); s1
s2 <- seq(1,10, length=3); s2
x <- c(1, 3, 8, 25, 100); seq(along=x)
#Subsetting variables
restData$nearMe <- restData$neighborhood %in% c("Roland Park", "Homeland")
table(restData$nearMe)
restData$zipWrong <- ifelse(restData$zipCode < 0, TRUE, FALSE)
table(restData$zipWrong, restData$zipCode <0)
table(restData$zipWrong)
#Cutting categoricalvariables
restData$zipGroups <- cut(restData$zipCode, breaks = quantile(restData$zipCode))
table(restData$zipGroups)
table(restData$zipGroups, restData$zipCode)
#Easier cutting
install.packages("Hmisc")
library(Hmisc)
restData$zipGroups <- cut2(restData$zipCode, g= 4)
table(restData$zipGroups)
#Creating factor variables
restData$zcf <- factor(restData$zipCode)
restData$zcf[1:10]
class(restData$zcf)
class(restData$zipCode)
restData$zipCode[1:10]
#Levels of factor variables
yesno <- sample(c("yes", "no"),size=10, replace=TRUE)
yesnofac <- factor(yesno,levels=c("yes","no"))
relevel(yesnofac,ref="yes")
as.numeric(yesnofac)
#Using the mutate function
library(plyr)
restData2 <- mutate(restData, zipGroups <- cut2(zipCode, g=4))
table(restData2$zipGroups)

#Start with reshaping
install.packages("reshape")
library(reshape)
head(mtcars)
mtcars$carname <- rownames(mtcars)
carMelt <- melt(mtcars, id <- c("carname", "gear", "cyl"), measure.vars <- c("mpg", "hp"))
head(carMelt, n=3)
tail(carMelt, n=3)
#Casting data frames
install.packages("reshape2")
library(reshape2)
cylData <- dcast(carMelt, cyl ~ variable)
cylData
cylData <- dcast(carMelt, cyl ~ variable, mean)
cylData
#Averaging values
head(InsectSprays)
tapply(InsectSprays$count, InsectSprays$spray, sum)
#Another way, split
spIns = split(InsectSprays$count, InsectSprays$spray)
spIns
sprCount <- lapply(spIns, sum)
sprCount
#Another way-combine
unlist(sprCount)
sapply(spIns, sum)
#Another way - plyr package
ddply(InsectSprays,.(spray), summarize, sum=sum(count))
#Creating anew variable
spraySums <- ddply(InsectSprays,.(spray),summarize, sum=ave(count, FUN=sum))
dim(spraySums)
head(spraySums)
