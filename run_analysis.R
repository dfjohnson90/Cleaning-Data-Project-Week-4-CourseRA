#load all the data
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
features <- read.table("UCI HAR Dataset/features.txt")

#merge all tables into one giant data table
test <- cbind(subject_test,y_test,X_test)
train <- cbind(subject_train,y_train, X_train)
data <- rbind(test,train)

#creates the descriptive column names for the data set
colnames(data) = c("subject","activity",features$V2)

#pulls out only columns with mean or std in the title
Stat_data <- data[,grep("subject|activity|mean|std", colnames(data))]

#labels activity names by actual activities
for(i in 1:10299){
  if(Stat_data[i,2] == 1) {Stat_data[i,2] = "walking"}
  if(Stat_data[i,2] == 2) {Stat_data[i,2] = "walking upstairs"}
  if(Stat_data[i,2] == 3) {Stat_data[i,2] = "walking downstairs"}
  if(Stat_data[i,2] == 4) {Stat_data[i,2] = "sitting"}
  if(Stat_data[i,2] == 5) {Stat_data[i,2] = "standing"}
  if(Stat_data[i,2] == 6) {Stat_data[i,2] = "laying"}
}

#cleans up variable names
colnames(Stat_data) <- gsub("-","", colnames(Stat_data))
colnames(Stat_data) <- gsub("\\()","", colnames(Stat_data))
colnames(Stat_data) <- gsub("m","M", colnames(Stat_data))
colnames(Stat_data) <- gsub("std","Std", colnames(Stat_data))
colnames(Stat_data) <- gsub("^t","Time", colnames(Stat_data))
colnames(Stat_data) <- gsub("^f","Freq", colnames(Stat_data))
tidyData <- data.frame()

#returns the average of each variable for each subject and activity
for(i in 1:30){
  subject <- Stat_data[Stat_data$subject == i,]
  activity <- subject[subject$activity=="walking",]
  activity <- cbind(activity[1,1:2],data.frame(t(colMeans(activity[,3:81]))))
  tidyData <- rbind(tidyData,activity)
  
  subject <- Stat_data[Stat_data$subject == i,]
  activity <- subject[subject$activity=="walking upstairs",]
  activity <- cbind(activity[1,1:2],data.frame(t(colMeans(activity[,3:81]))))
  tidyData <- rbind(tidyData,activity)
  
  subject <- Stat_data[Stat_data$subject == i,]
  activity <- subject[subject$activity=="walking downstairs",]
  activity <- cbind(activity[1,1:2],data.frame(t(colMeans(activity[,3:81]))))
  tidyData <- rbind(tidyData,activity)
  
  subject <- Stat_data[Stat_data$subject == i,]
  activity <- subject[subject$activity=="sitting",]
  activity <- cbind(activity[1,1:2],data.frame(t(colMeans(activity[,3:81]))))
  tidyData <- rbind(tidyData,activity)
  
  subject <- Stat_data[Stat_data$subject == i,]
  activity <- subject[subject$activity=="standing",]
  activity <- cbind(activity[1,1:2],data.frame(t(colMeans(activity[,3:81]))))
  tidyData <- rbind(tidyData,activity)
  
  subject <- Stat_data[Stat_data$subject == i,]
  activity <- subject[subject$activity=="laying",]
  activity <- cbind(activity[1,1:2],data.frame(t(colMeans(activity[,3:81]))))
  tidyData <- rbind(tidyData,activity)
}
print(tidyData)