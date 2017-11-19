#--------------------------------------------------------------------
# CSR3_Getting_and_Cleaning_Data
#
# run_analysis.R 
#--------------------------------------------------------------------
library(dplyr)
#--------------------------------------------------------------------
# Download and unzip source data if it doesn't exist
filename <- "getdata_dataset.zip"
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
  download.file(fileURL, filename, method="curl")}  
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) }

#--------------------------------------------------------------------
# Read data
trsu <- read.table(file.path("UCI HAR Dataset", "train", "subject_train.txt"))
trval <- read.table(file.path("UCI HAR Dataset", "train", "X_train.txt"))
tract <- read.table(file.path("UCI HAR Dataset", "train", "y_train.txt"))
tssub <- read.table(file.path("UCI HAR Dataset", "test", "subject_test.txt"))
tsval <- read.table(file.path("UCI HAR Dataset", "test", "X_test.txt"))
tsact <- read.table(file.path("UCI HAR Dataset", "test", "y_test.txt"))
features <- read.table(file.path("UCI HAR Dataset", "features.txt"), as.is = TRUE)
activity <- read.table(file.path("UCI HAR Dataset", "activity_labels.txt"))
colnames(activity) <- c("activityid", "activitylabel")

#--------------------------------------------------------------------
# Merge the training and the test sets to create one data set
hact <- rbind(
  cbind(trsub, trval, tract),
  cbind(tssub, tsval, tsact))

rm(trsub, trval, tract, tssub, tsval, tsact)
colnames(hact) <- c("subject", features[, 2], "activity")

#--------------------------------------------------------------------
# Extract only the measurements on the mean and standard deviation for each measurement
colstk <- grepl("subject|activity|mean|std", colnames(hact))
hact <- hact[, coltk]

#--------------------------------------------------------------------
# Use descriptive activity names to name the activities in the data set
hact$activity <- factor(hact$activity, levels = activities[, 1], labels = activities[, 2])

#--------------------------------------------------------------------
# Appropriately label the data set with descriptive variable names
hactcols <- colnames(hact)
hactcols <- gsub("[\\(\\)-]", "", hactcols)
hactcols <- gsub("FD", "frequencyDomain", hactcols)
hactcols <- gsub("TD", "timeDomain", hactcols)
hactcols <- gsub("Acc", "Accelerometer", hactcols)
hactcols <- gsub("Gyro", "Gyroscope", hactcols)
hactcols <- gsub("Mag", "Magnitude", hactcols)
hactcols <- gsub("Freq", "Frequency", hactcols)
hactcols <- gsub("Mean", "Mean", hactcols)
hactcols <- gsub("Std", "StandardDeviation", hactcols)
hactcols <- gsub("BodyBody", "Body", hactcols)
colnames(hact) <- hactcols

#--------------------------------------------------------------------
# Create a second, independent tidy set with the average of each variable for each activity and each subject
hactmeans <- hact %>% 
group_by(subject, activity) %>%
summarise_each(funs(Mean))

#--------------------------------------------------------------------
# Write the data set to the tidy_data.txt file
write.table(hactmeans, "tidy_data.txt", row.names = FALSE, quote = FALSE)

#--------------------------------------------------------------------





