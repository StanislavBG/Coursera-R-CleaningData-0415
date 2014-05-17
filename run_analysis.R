### Clear Path and Set Working Directories
rm(list=ls())

### Load Required libraries

### Set working directory
setwd("/Users/sgeorgiev/Desktop/Analytics/201405_CleaningData/1405_GettingCleaningData")

### Load and Combine the Data sets
tmp1 <- read.table("data/train/X_train.txt", header=FALSE, stringsAsFactors=FALSE)
tmp2 <- read.table("data/test/X_test.txt", header=FALSE, stringsAsFactors=FALSE)
X <- rbind(tmp1,tmp2)

tmp1 <- read.table("data/train/Y_train.txt", header=FALSE, stringsAsFactors=FALSE)  
tmp2 <- read.table("data/test/Y_test.txt", header=FALSE, stringsAsFactors=FALSE)  
Y <- rbind(tmp1,tmp2)

tmp1 <- read.table("data/train/subject_train.txt", header=FALSE, stringsAsFactors=FALSE)  
tmp2 <- read.table("data/test/subject_test.txt", header=FALSE, stringsAsFactors=FALSE)  
S <- rbind(tmp1,tmp2)


### Extracts only the measurements on the mean and standard deviation for each measurement. 

features <- read.table("data/features.txt", header=FALSE, stringsAsFactors=FALSE)  
indices_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, indices_of_good_features]
names(X) <- features[indices_of_good_features, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X)) # see last slide of the lecture Editing Text Variables (week 4)

### Uses descriptive activity names to name the activities in the data set
activities <- read.table("data/activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity"

### Appropriately labels the data set with descriptive activity names. 
names(S) <- "subject"
cleaned <- cbind(S, Y, X)
write.table(cleaned, "data/merged_clean_data.txt")


### Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
uniqueSubjects = unique(S)[,1]
numSubjects = length(unique(S)[,1])
numActivities = length(activities[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    result[row, 1] = uniqueSubjects[s]
    result[row, 2] = activities[a, 2]
    tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
    result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}
write.table(result, "data/data_set_with_the_averages.txt")

