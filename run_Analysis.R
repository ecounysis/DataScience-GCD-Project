library(dplyr)

setwd("UCI HAR Dataset")
loadDate <- date()

# read test data
Xtest <- read.table("test/X_test.txt")
Ytest <- read.table("test/y_test.txt")

subjectTest <- read.table("test/subject_test.txt")
names(subjectTest) <- "subject"
activityLabels <- read.table("activity_labels.txt")
names(activityLabels) <- c("activity","activityName")

# label test data
features <- read.table("features.txt")
names(Xtest) <- features[[2]]
names(Ytest) <- "activity"

# combine test data columns
testData <- cbind(Ytest,Xtest,subjectTest)

# read training data
Xtrain <- read.table("train/X_train.txt")
Ytrain <- read.table("train/y_train.txt")
subjecttrain <- read.table("train/subject_train.txt")
names(subjecttrain) <- "subject"

# label training data
names(Xtrain) <- features[[2]]
names(Ytrain) <- "activity"

# combine training data columns
trainData <- cbind(Ytrain,Xtrain,subjecttrain)


# combine test and training data
allData <- rbind(trainData, testData)


# add descriptive activity names
mergedData <- merge(activityLabels, allData)


# grab only the mean, std, subject, and activityName
extractedData <- mergedData[grep("(-(mean|std)\\(|subject|activityName)", names(mergedData))]

#load into a dplyr dataframe
data2 <- tbl_df(extractedData)
rm(extractedData)

# summarize data set with the average of each variable for each activity and each subject
bySubject <- group_by(data2, subject)
byActivity <- group_by(data2, activityName)
byBoth <- data2 %>% group_by(activityName, subject)

finalData <- summarize(byBoth, 
                       tBodyAcc_mean_X = mean(`tBodyAcc-mean()-X`),
                       tBodyAcc_mean_Y = mean(`tBodyAcc-mean()-Y`),
                       tBodyAcc_mean_Z = mean(`tBodyAcc-mean()-Z`),
                       tBodyAcc_std_X = mean(`tBodyAcc-std()-X`),
                       tBodyAcc_std_Y = mean(`tBodyAcc-std()-Y`),
                       tBodyAcc_std_Z = mean(`tBodyAcc-std()-Z`),
                       tGravityAcc_mean_X = mean(`tGravityAcc-mean()-X`),
                       tGravityAcc_mean_Y = mean(`tGravityAcc-mean()-Y`),
                       tGravityAcc_mean_Z = mean(`tGravityAcc-mean()-Z`),
                       tGravityAcc_std_X = mean(`tGravityAcc-std()-X`),
                       tGravityAcc_std_Y = mean(`tGravityAcc-std()-Y`),
                       tGravityAcc_std_Z = mean(`tGravityAcc-std()-Z`),
                       tBodyAccJerk_mean_X = mean(`tBodyAccJerk-mean()-X`),
                       tBodyAccJerk_mean_Y = mean(`tBodyAccJerk-mean()-Y`),
                       tBodyAccJerk_mean_Z = mean(`tBodyAccJerk-mean()-Z`),
                       tBodyAccJerk_std_X = mean(`tBodyAccJerk-std()-X`),
                       tBodyAccJerk_std_Y = mean(`tBodyAccJerk-std()-Y`),
                       tBodyAccJerk_std_Z = mean(`tBodyAccJerk-std()-Z`),
                       tBodyGyro_mean_X = mean(`tBodyGyro-mean()-X`),
                       tBodyGyro_mean_Y = mean(`tBodyGyro-mean()-Y`),
                       tBodyGyro_mean_Z = mean(`tBodyGyro-mean()-Z`),
                       tBodyGyro_std_X = mean(`tBodyGyro-std()-X`),
                       tBodyGyro_std_Y = mean(`tBodyGyro-std()-Y`),
                       tBodyGyro_std_Z = mean(`tBodyGyro-std()-Z`),
                       tBodyGyroJerk_mean_X = mean(`tBodyGyroJerk-mean()-X`),
                       tBodyGyroJerk_mean_Y = mean(`tBodyGyroJerk-mean()-Y`),
                       tBodyGyroJerk_mean_Z = mean(`tBodyGyroJerk-mean()-Z`),
                       tBodyGyroJerk_std_X = mean(`tBodyGyroJerk-std()-X`),
                       tBodyGyroJerk_std_Y = mean(`tBodyGyroJerk-std()-Y`),
                       tBodyGyroJerk_std_Z = mean(`tBodyGyroJerk-std()-Z`),
                       tBodyAccMag_mean_ = mean(`tBodyAccMag-mean()`),
                       tBodyAccMag_std_ = mean(`tBodyAccMag-std()`),
                       tGravityAccMag_mean_ = mean(`tGravityAccMag-mean()`),
                       tGravityAccMag_std_ = mean(`tGravityAccMag-std()`),
                       tBodyAccJerkMag_mean_ = mean(`tBodyAccJerkMag-mean()`),
                       tBodyAccJerkMag_std_ = mean(`tBodyAccJerkMag-std()`),
                       tBodyGyroMag_mean_ = mean(`tBodyGyroMag-mean()`),
                       tBodyGyroMag_std_ = mean(`tBodyGyroMag-std()`),
                       tBodyGyroJerkMag_mean_ = mean(`tBodyGyroJerkMag-mean()`),
                       tBodyGyroJerkMag_std_ = mean(`tBodyGyroJerkMag-std()`),
                       fBodyAcc_mean_X = mean(`fBodyAcc-mean()-X`),
                       fBodyAcc_mean_Y = mean(`fBodyAcc-mean()-Y`),
                       fBodyAcc_mean_Z = mean(`fBodyAcc-mean()-Z`),
                       fBodyAcc_std_X = mean(`fBodyAcc-std()-X`),
                       fBodyAcc_std_Y = mean(`fBodyAcc-std()-Y`),
                       fBodyAcc_std_Z = mean(`fBodyAcc-std()-Z`),
                       fBodyAccJerk_mean_X = mean(`fBodyAccJerk-mean()-X`),
                       fBodyAccJerk_mean_Y = mean(`fBodyAccJerk-mean()-Y`),
                       fBodyAccJerk_mean_Z = mean(`fBodyAccJerk-mean()-Z`),
                       fBodyAccJerk_std_X = mean(`fBodyAccJerk-std()-X`),
                       fBodyAccJerk_std_Y = mean(`fBodyAccJerk-std()-Y`),
                       fBodyAccJerk_std_Z = mean(`fBodyAccJerk-std()-Z`),
                       fBodyGyro_mean_X = mean(`fBodyGyro-mean()-X`),
                       fBodyGyro_mean_Y = mean(`fBodyGyro-mean()-Y`),
                       fBodyGyro_mean_Z = mean(`fBodyGyro-mean()-Z`),
                       fBodyGyro_std_X = mean(`fBodyGyro-std()-X`),
                       fBodyGyro_std_Y = mean(`fBodyGyro-std()-Y`),
                       fBodyGyro_std_Z = mean(`fBodyGyro-std()-Z`),
                       fBodyAccMag_mean_ = mean(`fBodyAccMag-mean()`),
                       fBodyAccMag_std_ = mean(`fBodyAccMag-std()`),
                       fBodyBodyAccJerkMag_mean_ = mean(`fBodyBodyAccJerkMag-mean()`),
                       fBodyBodyAccJerkMag_std_ = mean(`fBodyBodyAccJerkMag-std()`),
                       fBodyBodyGyroMag_mean_ = mean(`fBodyBodyGyroMag-mean()`),
                       fBodyBodyGyroMag_std_ = mean(`fBodyBodyGyroMag-std()`),
                       fBodyBodyGyroJerkMag_mean_ = mean(`fBodyBodyGyroJerkMag-mean()`),
                       fBodyBodyGyroJerkMag_std_ = mean(`fBodyBodyGyroJerkMag-std()`))

write.table(finalData, file="projectFinalData.txt", row.name=FALSE)