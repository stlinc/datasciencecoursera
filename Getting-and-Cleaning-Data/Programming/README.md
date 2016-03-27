# Coursera -- Getting and Cleaning Data 
Purpose :¡@Creating a tidy data set of wearable computing experiments data 
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones


## Contents
(1) README.MD 
(2) CodeBook.MD  -- Field definition
(3) run_analysis.R  -- Code written by R


## Main steps in run_analysis.R

1. Data loading for field name
1.1: Load activity
1.2: Load Features

2. Data loading for training set
2.0: Load training subject
2.1: Load training label
2.2 Load training data
2.3 Bind label + data

3. data loading for test set 
3.0: Load testing subject
3.1: Load testing label
3.2 Load testing data
3.3 Bind label + data

4. merge test + training dataset into one
4.1 assign field name
4.2 select mean, std wording columns into a new data frame
4.3 merge activity description
4.4 prepare the final summary 
4.5 write to text file
