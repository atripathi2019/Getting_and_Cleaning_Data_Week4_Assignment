## Download the file
library(dplyr)
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, "Course3week4Assign.zip")
unzip("Course3week4Assign.zip", exdir = "Course3week4Assign")
file.remove("Course3week4Assign.zip")

## Read the files in the folder
features_file <- read.table("Course3week4Assign/UCI HAR Dataset/features.txt", col.names = c("feature_id","features"))
activities_file <- read.table("Course3week4Assign/UCI HAR Dataset/activity_labels.txt", col.names = c("activity_id", "activity"))
subject_test_file <- read.table ("Course3week4Assign/UCI HAR Dataset/test/subject_test.txt", col.names = "subject_id")
x_test_file <- read.table ("Course3week4Assign/UCI HAR Dataset/test/X_test.txt", col.names = features_file$features)
y_test_file <-read.table ("Course3week4Assign/UCI HAR Dataset/test/y_test.txt", col.names = "activity_id")
subject_train_file <- read.table ("Course3week4Assign/UCI HAR Dataset/train/subject_train.txt", col.names = "subject_id")
x_train_file <- read.table ("Course3week4Assign/UCI HAR Dataset/train/X_train.txt", col.names = features_file$features)
y_train_file <-read.table ("Course3week4Assign/UCI HAR Dataset/train/y_train.txt", col.names = "activity_id")

## Merge the test and train files in ONE file (currently 4 files)
x_all_file <- rbind(x_test_file, x_train_file)
y_all_file <- rbind(y_test_file, y_train_file)
subject_all_file <- rbind(subject_test_file, subject_train_file)
main_file <- cbind(subject_all_file, x_all_file, y_all_file)

## Extract mean and Standard Deviation of each variable
columns <-names(main_file)
mean_std_columns <- grep("activity_id|subject_id|.mean.|.std.", columns, value = TRUE)
desired_file <- main_file [,mean_std_columns]

## Labeling activity_id with activity 
labeled_file_test <- inner_join(desired_file, activities_file, by = "activity_id")
labeled_file <- select (labeled_file_test,1:80,82)

##Labeling other columns with meaningful names
names(labeled_file)<-gsub("Acc", "Accelerometer", names(labeled_file))
names(labeled_file)<-gsub("Gyro", "Gyroscope", names(labeled_file))
names(labeled_file)<-gsub("BodyBody", "Body", names(labeled_file))
names(labeled_file)<-gsub("Mag", "Magnitude", names(labeled_file))
names(labeled_file)<-gsub("^t", "Time", names(labeled_file))
names(labeled_file)<-gsub("^f", "Frequency", names(labeled_file))
names(labeled_file)<-gsub("tBody", "TimeBody", names(labeled_file))
names(labeled_file)<-gsub("-mean()", "Mean", names(labeled_file), ignore.case = TRUE)
names(labeled_file)<-gsub("-std()", "STD", names(labeled_file), ignore.case = TRUE)
names(labeled_file)<-gsub("-freq()", "Frequency", names(labeled_file), ignore.case = TRUE)
names(labeled_file)<-gsub("angle", "Angle", names(labeled_file))
names(labeled_file)<-gsub("gravity", "Gravity", names(labeled_file))


## Creating another data set with mean  
## for each variable for each activity for each subject
by_sub_act <- labeled_file %>%
  group_by(subject_id, activity) %>%
  summarise_all(mean)
write.table(by_sub_act, "Summarized by subject and activity.txt", row.names = FALSE)