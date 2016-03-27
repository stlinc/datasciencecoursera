# 1. Merges the training and the test sets to create one data set.
# 1.1: Load activity
activityLabels <- read.table("/Users/apple/Documents/Big Data/Coursera Data Science Specialization/Cleaning Data/UCI HAR Dataset/activity_labels.txt", header = FALSE, sep = " ")
names(activityLabels)
head(activityLabels)
colnames(activityLabels) <- c("ActivityID","ActivityName")

# 1.2: Load Features
features <- read.table("/Users/apple/Documents/Big Data/Coursera Data Science Specialization/Cleaning Data/UCI HAR Dataset/features.txt", header = FALSE, sep = " ")
names(features)
head(features)
colnames(features) <- c("FeatureID","FeatureName")
features[,2]

# 1.3 Data loading for training set
# 1.3.1 Load training subject
trainSubject <- read.table("/Users/apple/Documents/Big Data/Coursera Data Science Specialization/Cleaning Data/UCI HAR Dataset/train/subject_train.txt", header = FALSE, sep = " ")
names(trainSubject)
head(trainSubject)
names(trainSubject) <- c("Subject")

# 1.3.2 Load training label
trainLabels <- read.table("/Users/apple/Documents/Big Data/Coursera Data Science Specialization/Cleaning Data/UCI HAR Dataset/train/y_train.txt", header = FALSE, sep = " ")
names(trainLabels)
head(trainLabels)
names(trainLabels) <- c("ActivityClass")

# 1.3.3 Load training data
trainData <- read.table("/Users/apple/Documents/Big Data/Coursera Data Science Specialization/Cleaning Data/UCI HAR Dataset/train/x_train.txt", header = FALSE, sep = "", na.strings ="")
names(trainData)
head(trainData)
colnames(trainData) <- features[,2]

# 1.3.4 Bind label + data
trainData <- cbind(trainSubject, trainLabels,trainData)
names(trainData)
head(trainData[, 1:10], 3)
dim(trainData)

# 1.4 Data loading for test set 
# 1.4.1 Load testing subject
testSubject <- read.table("/Users/apple/Documents/Big Data/Coursera Data Science Specialization/Cleaning Data/UCI HAR Dataset/test/subject_test.txt", header = FALSE, sep = " ")
names(testSubject)
head(testSubject)
names(testSubject) <- c("Subject")

# 1.4.2 Load testing label
testLabels <- read.table("/Users/apple/Documents/Big Data/Coursera Data Science Specialization/Cleaning Data/UCI HAR Dataset/test/y_test.txt", header = FALSE, sep = " ")
names(testLabels)
head(testLabels)
names(testLabels) <- c("ActivityClass")

# 1.4.3 Load testing data
testData <- read.table("/Users/apple/Documents/Big Data/Coursera Data Science Specialization/Cleaning Data/UCI HAR Dataset/test/x_test.txt", header = FALSE, sep = "", na.strings ="")
names(testData)
head(testData)
colnames(testData) <- features[,2]

# 1.4.4 Bind label + data
testData <- cbind(testSubject, testLabels,testData)
names(testData)
head(testData[, 1:10], 3)
dim(testData)

# 1.5 merge test + training dataset into one
dataCombined <- rbind(trainData, testData)
names(dataCombined)
dim(dataCombined)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
dataSubset_mean_std <- dataCombined[, grepl("Subject|Activity|[Mm]ean|[Ss]td", names(dataCombined))]

# 3. Uses descriptive activity names to name the activities in the data set
dataSubset_mean_std <- merge(activityLabels, dataSubset_mean_std, by.x = "ActivityID", by.y = "ActivityClass")

# 4. Appropriately labels the data set with descriptive variable names.
names(dataSubset_mean_std)<-sub("^t", "time", names(dataSubset_mean_std))
names(dataSubset_mean_std)<-sub("^f", "frequency", names(dataSubset_mean_std))
names(dataSubset_mean_std)<-sub("Acc", "Accelerometer", names(dataSubset_mean_std))
names(dataSubset_mean_std)<-sub("Gyro", "Gyroscope", names(dataSubset_mean_std))
names(dataSubset_mean_std)<-sub("Mag", "Magnitude", names(dataSubset_mean_std))
names(dataSubset_mean_std)<-sub("BodyBody", "Body", names(dataSubset_mean_std))

# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
dataSubset_mean_stdByActSub <- aggregate(.~dataSubset_mean_std$'ActivityName'+dataSubset_mean_std$Subject, dataSubset_mean_std, mean)

# 6 write to text file
write.table(dataSubset_mean_stdByActSub, file = "tidy_data.txt", sep = " ", col.names = FALSE)
tidyData <- read.table("./tidy_data.txt", header = FALSE, sep = " ")
names(tidyData)
head(tidyData)
