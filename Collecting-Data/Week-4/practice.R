### Load Library

library(doBy)
library(dplyr)
library(stringr)

### 1. Data loading for field name
###### 1.1: Load activity
con <- file("C:/Users/user/Documents/datasciencecouresa/UCI HAR Dataset/activity_labels.txt","r")
labels <- read.csv(con, header = FALSE, sep = " ")
close(con)
labels <- rename(labels, Activity = V1, Activity_name = V2 )

###### 1.2: Load Features
con <- file("C:/Users/user/Documents/datasciencecouresa/UCI HAR Dataset/features.txt","r")
features <- read.csv(con, header = FALSE, sep = " ")
close(con)
features <- rename(features, feature_code = V1, feature_name = V2 )


### 2. Data loading for training set
###### 2.0: Load training subject
con <- file("C:/Users/user/Documents/datasciencecouresa/UCI HAR Dataset/train/subject_train.txt","r")
train_subject <- read.csv(con, header = F)
train_subject <- rename(train_subject, subject = V1)   ## rename field name
close(con)

###### 2.1: Load training label
con <- file("C:/Users/user/Documents/datasciencecouresa/UCI HAR Dataset/train/y_train.txt","r")
train_labels <- read.csv(con, header = F)
train_labels <- rename(train_labels, Activity = V1)   ## rename field name
close(con)

###### 2.2 Load training data
con <- file("C:/Users/user/Documents/datasciencecouresa/UCI HAR Dataset/train/X_train.txt","r")
train_data <- read.table(con, sep = "" , header = F , 
                         na.strings ="", stringsAsFactors= F)
close(con)
###### 2.3 Bind label + data
train_data <- cbind(train_labels,train_data)
train_data <- cbind(train_subject,train_data)


### 3. data loading for test set 
###### 3.0: Load testing subject
con <- file("C:/Users/user/Documents/datasciencecouresa/UCI HAR Dataset/test/subject_test.txt","r")
test_subject <- read.csv(con, header = F)
test_subject <- rename(test_subject, subject = V1)   ## rename field name
close(con)
###### 3.1: Load testing label
con <- file("C:/Users/user/Documents/datasciencecouresa/UCI HAR Dataset/test/y_test.txt","r")
test_labels <- read.csv(con,header = F)
test_labels <- rename(test_labels, Activity = V1)   ## rename field name
close(con)

###### 3.2 Load testing data
con <- file("C:/Users/user/Documents/datasciencecouresa/UCI HAR Dataset/test/X_test.txt","r")
test_data <- read.table(con, sep = "" , header = F ,
                         na.strings ="", stringsAsFactors= F)
close(con)

###### 3.3 Bind label + data
test_data <- cbind(test_labels,test_data)
test_data <- cbind(test_subject,test_data)


### 4. merge test + training dataset into one
total <- rbind(train_data, test_data)

### 4.1 assign field name
id1 <- 3: 563
i=0
for (i in id1) {
  colnames(total)[i] <- as.character(features[i,2])
  i = i+1
}

### 4.2 select mean, std wording columns into a new data frame
col_vector <- grep("subject|Activity|mean|Mean|std", names(total))
selected_columns <- total[,col_vector]

### 4.3 merge activity description
selected_columns <- merge(labels, selected_columns, by.x = "Activity", by.y = "Activity")

### 4.4 prepare the final summary 
aggr_data <- aggregate(selected_columns[, 4:ncol(selected_columns)],
                       by=list(subject = selected_columns$subject, 
                               Activity_name = selected_columns$Activity_name),
                       mean)

### 4.5 write to text file
write.table(aggr_data, file = "class4_tidy_data_set.txt", sep = " ", col.names = FALSE)
