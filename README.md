# DataScience-GCD-Project
Project for Getting and Cleaning Data course

# Steps Taken
Outside of R, downloaded and unzipped the data
In the R script:
-read in the test data
-add names to test data
-combine all test data columns
-read training data
-add names to training data
-combine all training data columns
-combine test and training data rows
-add descriptive activity names
-pull out only the mean, std, subject, and activityName columns
-load into a dplyr dataframe
-summarize data set with the average of each variable for each activity and each subject
-write to a file called `projectFinalData.txt`
