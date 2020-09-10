## Final Project Coursera SEP 20
## 0 - Cleaning (tyding) data
## 0.1 Read libraries
library(data.table)
library(zip)
library(plyr)
## 0.2 Check if data set is already downloaded and download it, if necessary.

if (dir.exists("UCI HAR Dataset")){print("Already exists")
      } else {
        file1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        print("error")
        ##download.file(file1, destfile = "x.zip", method = "curl")
        unzip("x.zip") 
        }
## 0.3 Obtaining all the txt files 
list1 <- list.files(pattern = "*.txt" , recursive = TRUE)
x <- grep("X_|/y_test|/y_train|subject|features?|_labels" ,list1)
x<- list1[x]
## 0.4 Remove "features.info.txt" from the list
x<- x[-2]

## 0.5 Reading the txt files as data frames
y<- sapply(x, FUN=read.table, simplify = T)

## 0.6 Binding test, training & ID data
data_x  <- rbind(y$`UCI HAR Dataset/test/X_test.txt`, y$`UCI HAR Dataset/train/X_train.txt`)
data_y  <- rbind(y$`UCI HAR Dataset/test/y_test.txt`, y$`UCI HAR Dataset/train/y_train.txt`)
subject <- rbind(y$`UCI HAR Dataset/test/subject_test.txt`, y$`UCI HAR Dataset/train/subject_train.txt`)

## 0.7 Labeling test, training & ID data

features_names <-as.character(y$`UCI HAR Dataset/features.txt`[,2])
colnames(data_x)  <- features_names
colnames(data_y)  <- ("Activity")
colnames(subject) <- ("IDsubject")

## 1.0 Merges the training and the test sets to create one data set. 
merged <- cbind(subject, data_y, data_x)


## 2.0 Extracts only the measurements on the mean and standard deviation for each measurement. 
g1 <- grep("std\\(\\)|mean\\(\\)" ,features_names)
col_filter <- c("Activity", "IDsubject", features_names [g1])
data <- subset(merged, select = col_filter)

## 3.0 Uses descriptive activity names to name the activities in the data set
## Obtaining the labels from the "activity_labels.txt" file
labels <- (y$`UCI HAR Dataset/activity_labels.txt`)
labels <- as.character(labels [,2])
key <- c(1:6)
sapply(key,FUN =function(i){data$Activity[data$Activity == key[i]] <<- labels[i]})

##4 .0 Appropriately labels the data set with descriptive variable names.
## For many features there are several abbreviations that need to be expanded to clarify the meaning

names(data)<-gsub("Acc", "Accelerometer", names(data))
names(data)<-gsub("Gyro", "Gyroscope", names(data))
names(data)<-gsub("BodyBody", "Body", names(data))
names(data)<-gsub("Mag", "Magnitude", names(data))
names(data)<-gsub("^t", "Time", names(data))
names(data)<-gsub("^f", "Frequency", names(data))
names(data)<-gsub("tBody", "TimeBody", names(data))
names(data)<-gsub("-mean()", "Mean", names(data), ignore.case = TRUE)
names(data)<-gsub("-std()", "STD", names(data), ignore.case = TRUE)
names(data)<-gsub("-freq()", "Frequency", names(data), ignore.case = TRUE)
names(data)<-gsub("angle", "Angle", names(data))
names(data)<-gsub("gravity", "Gravity", names(data))

## 5.0 From the data set in step 4, creates a second, independent tidy data set with the average
## of each variable for each activity and each subject.

data2<-aggregate(. ~IDsubject + Activity, data, mean)
write.table(data2, file = "tidy-data.txt",row.name=FALSE)
