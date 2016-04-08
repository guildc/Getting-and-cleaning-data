#######################################################################
## Assignment: Getting and Cleaning Data - Project
## author: Camelia Guild
## date: 4/1/2016

## runAnalysis.R File Description:
## The following script performs data extraction, cleaning, and creates an 
## analysis data set (tidyData) from the UCI HAR Dataset downloaded from:
## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

## The following steps are accomplished:
## 1. Merge the training and the test sets to create one data set.
## 2. Extract only the measurements on the mean and standard deviation for each measurement.
## 3. Use descriptive activity names to name the activities in the data set.
## 4. Appropriately label the data set with descriptive activity names.
## 5. Create a second, independent tidy data set with the average of each variable for each
##    activity and each subject.

#########################################################################

#set working directory
setwd("/Users/cameliaguild/Projects/DataScientistsToolbox/Getting-and-cleaning-data")

filename <- "getdata_dataset.zip"

# Download and unzip data files:
if (!file.exists(filename)) {
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}
if (!file.exists("UCI HAR Dataset")) {
  unzip(filename)
}

# Preliminary steps:
# Read in the training data from files
activityType <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE)
features <- read.table("./UCI HAR Dataset/features.txt", header = FALSE)
subjectTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)
xTrain <- read.table("./UCI HAR Dataset/train/x_train.txt", header = FALSE)
yTrain <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE)

# Cleaning the data: Assign column names to the data
colnames(activityType) <- c("activityId", "activityType")
colnames(subjectTrain) <- c("subjectId")
colnames(xTrain) <- features[,2]
colnames(yTrain) <- c("activityId")

# Create the final training set: merge yTrain, subjectTrain, and xTrain
trainingData <-cbind(subjectTrain, yTrain, xTrain)

# Read in the test data files
subjectTest <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)
xTest <- read.table("./UCI HAR Dataset/test/x_test.txt", header = FALSE)
yTest <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE)

# Assign column names to the test data
colnames(subjectTest) <- c("subjectId")
colnames(xTest) <- features[,2]
colnames(yTest) <- c("activityId")

# Create the final test set: merge subjectTest, xTest, and yTest
testData <- cbind(subjectTest, yTest, xTest)

####################################################################
# Step 1:
# Merge training and test data sets to create a final data set
finalData <- rbind(trainingData, testData)

# Create a vector for the column names from the finalData, which will be
# used to select the desired mean() and stddev() columns
colNames <- colnames(finalData)

# Step 2:
# Extract only the measurements on the mean and standard deviation for each measurement.
# Create a vector of logical values that contains TRUE values for the ID, mean() & stddev()
# columns and FALSE otherwise

logicalVector <- (grepl("activity..", colNames) | grepl("subject..", colNames) 
                          | grepl("-mean..", colNames)  & !grepl("-meanFreq..", colNames)
                          | grepl("-std..", colNames))

# Subset finalData table based on the logicalVector to keep only desired columns
finalData = finalData[logicalVector == TRUE]

# Step 3:
# Use descriptive activity names to name the activities in the data set

# Merge the finalData set with the activityType table to include descriptive activity names
finalData <- merge(finalData, activityType, by= 'activityId', all.x = TRUE)

# Update the colNames vector to include the new column names
colNames <- colnames(finalData)

# Step 4: 
# Appropriately label the data set with descriptive activity names.

# Clean up the variable names, use the gsub function. 

for (i in 1:length(colNames))
{
  colNames[i] <- gsub("\\()","", colNames[i])
  colNames[i] <- gsub("-std$", "StdDev", colNames[i])
  colNames[i] <- gsub("-mean", "Mean", colNames[i])
  colNames[i] <- gsub("^(t)", "time", colNames[i])
  colNames[i] <- gsub("^(f)", "freq", colNames[i])
  colNames[i] <- gsub("(BodyBody|Body)", "Body", colNames[i])
  colNames[i] <- gsub("Gyro","Gyroscope", colNames[i])
  colNames[i] <- gsub("Acc", "Accelerometer", colNames[i])
  colNames[i] <- gsub("Mag","Magnitude", colNames[i])
}

# Assign the descriptive column names to the finalData set
colnames(finalData) <- colNames

# Step 5:
# Create a second, independent tidy data set with the average of each variable for each activity and each subject.
# Create a new table that does not include the activityType column.
finalDataNoActivityType <- finalData[,names(finalData) != 'activityType']

# Summarize the finalDataNoActivityType to include only the mean of each variable for each activity and each subject.
tidyData <- aggregate(finalDataNoActivityType[,names(finalDataNoActivityType)
              != c('activityId','subjectId')], by=list(activityId=finalDataNoActivityType$activityId,
                                                       subjectId = finalDataNoActivityType$subjectId),mean)

# Merge the tidyData with activityType to include the descriptive activity names.
tidyData <- merge(tidyData, activityType, by = 'activityId', all.x = TRUE)

# Export the tidyData set
write.table(tidyData, './UCI HAR Dataset/tidyData.txt', row.names = FALSE, sep='\t')


