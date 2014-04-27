## Required Packages: plyer, reshape
library(plyr)
library(reshape)
## Gather column labels and find valid features
features <- read.table("./UCI HAR Dataset/features.txt")
colnames(features) <- c("feature_id", "feature_name")
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

## Merge data together
## Gather and clean train data
train_directory <- "./UCI HAR Dataset/train"
data_train <- read.table(paste(train_directory, "X_train.txt", sep = "/"))
colnames(data_train) <- features[,2]
activity_train <- read.table(paste(train_directory, "y_train.txt", sep = "/"))
activity_train <- join(activity_train,activity_labels)
subject_train <- read.table(paste(train_directory, "subject_train.txt", sep = "/"))
train_final <- cbind(subject = subject_train[,1], activity = activity_train[,2], data_train)

## Gather and clean test data
test_directory <- "./UCI HAR Dataset/test"
data_test <- read.table(paste(test_directory, "X_test.txt", sep = "/"))
colnames(data_test) <- features[,2]
activity_test <- read.table(paste(test_directory, "y_test.txt", sep = "/"))
activity_test <- join(activity_test,activity_labels)
subject_test <- read.table(paste(test_directory, "subject_test.txt", sep = "/"))
test_final <- cbind(subject = subject_test[,1], activity = activity_test[,2], data_test)

## Combine datasets
complete_set <- rbind(train_final,test_final)

## Reduce to the valid features where standard deviation and mean columns
std_cols <- grep("-std\\(\\)", colnames(complete_set))
mean_cols <- grep("-mean\\(\\)", colnames(complete_set))
valid_cols <- c(1:2, sort(c(std_cols,mean_cols)))
valid_features <- features[valid_cols,]
complete_set_valid <- subset(complete_set[,valid_features[,1]])

## Create tidy or long format based on subject and activity
complete_set_tidy <- melt(complete_set_valid, id=c("subject","activity"))

## Creates a 2nd data set where the subjects' activity features are averaged
complete_cast <- cast(complete_set_tidy,subject + activity ~ variable,mean)

## TEST CASE: mean(subset(complete_set_tidy, subject==1 & activity == "LAYING" & variable == "tBodyAcc-mean()-X")$value)

## Generates a text file with the 2nd tidy data set
write.table(complete_cast, file = "getdata-002_tidy_dataset.txt")