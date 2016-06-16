if (!require("stringr")){
  install.packages("stringr", dependencies=TRUE)
}

library("stringr")

if (!require("reshape")){
  install.packages("reshape", dependencies=TRUE)
}

library("reshape")

if (!require("plyr")){
  install.packages("plyr", dependencies=TRUE)
}

library("plyr")

if(!file.exists("./dataset")){
  dir.create("./dataset")
}

setwd("~/Coursera/cleaning_data/dataset")

file <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

if(!file.exists("./dataset/data.zip")){
  download.file(file, destfile = "data.zip")
  unzip("data.zip")
}

setwd("./UCI HAR Dataset")

features <- read.table("features.txt", header=FALSE)

#read tables and set column names
trainset_x <- read.table("./train/X_train.txt", header=FALSE, col.names = features$V2)
trainset_y <- read.table("./train/y_train.txt", header=FALSE, col.names = "Activity")
trainset_subject <- read.table("./train/subject_train.txt", header=FALSE, col.names=c("Subject"))



test_x <- read.table("./test/X_test.txt", header=FALSE, col.names = features$V2)
test_y <- read.table("./test/y_test.txt", header=FALSE, col.names = "Activity")
test_subject <- read.table("./test/subject_test.txt", header=FALSE, col.names=c("Subject"))

#concatenate data tables
datasubject <- rbind(trainset_subject, test_subject)
dataactivity <- rbind(trainset_y, test_y)
datafeatures <- rbind(trainset_x,test_x)

completeData <- cbind(datafeatures,dataactivity,datasubject)

activityLabels <- read.table("./activity_labels.txt", header = FALSE)

#Extracts only the measurements on the mean and standard deviation for each measurement. 
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)
requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)

extractedData <- completeData[,requiredColumns]
dim(extractedData)

#Uses descriptive activity names to name the activities in the data set
extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}

extractedData$Activity <- as.factor(extractedData$Activity)

#Appropriately labels the data set with descriptive variable names. 
names(extractedData)<-gsub("Acc", "Acc", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyro", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Freq", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Freq", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))


#From the data set in step 4, creates a second, independent tidy data set 
#with the average of each variable for each activity and each subject.

extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

tidyData<-aggregate(. ~Subject + Activity, extractedData, mean)
tidyData<-tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "tidydata.txt",row.name=FALSE)
