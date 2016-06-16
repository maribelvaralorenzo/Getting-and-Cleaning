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




#Extracts only the measurements on the mean and standard deviation for each measurement
datafeatures <- datafeatures[, grep(".*\\.(mean|std)\\.\\..*", names(datafeatures), value=T)]

#Uses descriptive activity names to name the activities in the data set

activity_labels <- read.table("./activity_labels.txt", header=FALSE, col.names=c("Activity", "ActivityName"))
dataactivity <- merge(dataactivity,activity_labels, by="Activity")
#merge columns to get the data "data"
dataactivities <- cbind(datasubject, data.frame(activity = dataactivity$ActivityName))

data <- cbind(datafeatures, dataactivities)


#Appropriately labels the data set with descriptive variable names. 
colnames(data) <- tolower(str_replace_all(colnames(data), "([A-Z]{1})", ".\\1"))
colnames(data) <- str_replace_all(colnames(data), "[\\.]+", ".")
colnames(data) <- str_replace_all(colnames(data), "[\\.]+$", "")


tidy <- data[0,]
tidy[1,] <- rep(NA, 68)
 
melted <- melt(data, id=c(".subject","activity"))

#From the data set in step 4, creates a second, independent tidy data set 
#with the average of each variable for each activity and each subject.

data2<-aggregate(. ~.subject + activity, data, mean)
data2<-data2[order(data2$.subject,data2$activity),]
write.table(data2, file = "tidydata.txt",row.name=FALSE)
