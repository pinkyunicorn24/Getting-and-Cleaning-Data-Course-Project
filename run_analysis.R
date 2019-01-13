##Description#
#Here are the data for the project:

#https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

#You should create one R script called run_analysis.R that does the following.

#1. Merges the training and the test sets to create one data set.
#2. Extracts only the measurements on the mean and standard deviation for each measurement.
#3. Uses descriptive activity names to name the activities in the data set.
#4. Appropriately labels the data set with descriptive variable names.
#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Set working Directory
setwd("~/Desktop/R")

#Download and unzip data
if(!file.exists("./UCI Data")){dir.create("./UCI Data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile="./UCI Data/Dataset.zip" )
unzip(zipfile = "./UCI Data/Dataset.zip",exdir="./UCI Data")

#Reading Data

#Training Data
train_x <- read.table("./UCI Data/UCI HAR Dataset/train/X_train.txt")
train_y <- read.table("./UCI Data/UCI HAR Dataset/train/Y_train.txt")
subject_train <- read.table("./UCI Data/UCI HAR Dataset/train/subject_train.txt")

#Feature
features <- read.table('./UCI Data/UCI HAR Dataset/features.txt')

#Testing data
test_x <- read.table("./UCI Data/UCI HAR Dataset/test/X_test.txt")
test_y <- read.table("./UCI Data/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./UCI Data/UCI HAR Dataset/test/subject_test.txt")

#Activity label
activityLabel <- read.table('./UCI Data/UCI HAR Dataset/activity_labels.txt')
subject <- rbind(subject_train, subject_test)
activity <- rbind(train_y, test_y)

#1. Merges the training and the test sets to create one data set.

#Assign column names first

#for training data
colnames(subject_train) <- "subjectId"
colnames(train_x) <- features[,2] 
colnames(train_y) <-"activityId"

#for testing data
colnames(test_x) <- features[,2] 
colnames(test_y) <- "activityId"
colnames(subject_test) <- "subjectId"

# for activityLabels
colnames(activityLabel) <- c('activityId','activityType')
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
#Merging the train and the test sets 
merging_train <- cbind(train_x, train_y, subject_train)
merging_test <- cbind(test_x, test_y, subject_test)

#Merging all data sets into one set
AllData <- rbind(merging_train, merging_test)

#2. Extracts only the measurements on the mean and standard deviation for each measurement.
Names <- colnames(AllData)
MeanAndSTD <-grepl("mean|std|subject|activityId",Names)
OnlyMeanAndSTD <- AllData[, MeanAndSTD]

#3. Uses descriptive activity names to name the activities in the data set
activitylabel <- as.character(activityLabel[,2])
OnlyMeanAndSTD$activityId<- activitylabel[OnlyMeanAndSTD$activityId]

#4. Appropriately labels the data set with descriptive variable names.

names(OnlyMeanAndSTD) <- gsub("^t", "Time",names(OnlyMeanAndSTD ))
names(OnlyMeanAndSTD) <- gsub("^f", "Frequency",names(OnlyMeanAndSTD))
names(OnlyMeanAndSTD) <- gsub("-std()", "STD",names(OnlyMeanAndSTD))
names(OnlyMeanAndSTD) <- gsub("-mean()", "Mean",names(OnlyMeanAndSTD))
names(OnlyMeanAndSTD) <- gsub("BodyBody", "Body",names(OnlyMeanAndSTD))
names(OnlyMeanAndSTD) <- gsub("Acc", "Acceleration",names(OnlyMeanAndSTD))
names(OnlyMeanAndSTD) <- gsub("Mag", "Magnitude",names(OnlyMeanAndSTD))
names(OnlyMeanAndSTD) <- gsub("tBody", "TimeBody",names(OnlyMeanAndSTD))
names(OnlyMeanAndSTD) <- gsub("Gyro", "Gyroscope",names(OnlyMeanAndSTD))

##5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
OnlyMeanAndSTD$subjectId <- as.factor(OnlyMeanAndSTD$subjectId)
OnlyMeanAndSTD<- data.table(OnlyMeanAndSTD)
tidyData <- aggregate(. ~subjectId + activityId, OnlyMeanAndSTD, mean)
tidyData <- tidyData[order(tidyData$subjectId,tidyData$activityId),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)