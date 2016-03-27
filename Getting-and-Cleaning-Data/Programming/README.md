# Coursera -- Getting and Cleaning Data 
Purpose : Creating a tidy data set of wearable computing experiments data 
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones


## Contents
(1) README.MD 
(2) CodeBook.MD  -- Field definition
(3) run_analysis.R  -- Code written by R


## Main steps in run_analysis.R
1. Merges the training and the test sets to create one data set.
 * 1.1: Load Activities
 * 1.2: Load Features
 * 1.3 Data loading for training set
1.3.1 Load Training Subjects
1.3.2 Load Training Labels
1.3.3 Load Training Data
1.3.4 Bind Traing Labels + Data
 * 1.4 Data loading for testing set
1.4.1 Load Testing Subjects
1.4.2 Load Testing Labels
1.4.3 Load Testing Data
1.4.4 Bind Testing Labels + Data
 * 1.5 Merge Traingin and Testing Data into one set

2. Extracts only the measurements on the mean and standard deviation for each measurement.

3. Uses descriptive activity names to name the activities in the data set. 

4. Appropriately labels the data set with descriptive variable names.

5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

6. Write to text file
