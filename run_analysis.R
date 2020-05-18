
#######Raw data needs to be downloaded and Unzipped in the working directory before executing the code

##Import data
  
#Import Train data
  subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
  X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
  y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
  
 
#Import Test data
  subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
  X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
  y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
  
  #Import other data
  activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
  features <- read.table("UCI HAR Dataset/features.txt")  
  
 # Step 1. Merges the training and the test sets to create one data set.
 data <- rbind(X_train,X_test)
 names(data)<-features[,2]
 subject <- rbind(subject_train, subject_test)
 activity <- rbind(y_train, y_test)
  
 # Step 2. Extracts only the measurements on the mean and standard deviation for each measurement.
 #Positions of features which correspond to mean or standard deviation
 Mean_Std_posn <- grep("std()|mean()", features[, 2]) 
 #Subset the data to include only those columns related to mean or standard deviation
 data <- data[,Mean_Std_posn]


 # Step 3. Uses descriptive activity names to name the activities in the data set
 #Replace activity levels in the activity dataset with the corresponding activity names
 activity_level <- factor(activity$V1)
 levels(activity_level) <- activity_labels[,2]
 activity$V1 <- activity_level
 
 #Create consolidated dataset
 data <- cbind(subject,activity, data)
 
 # Step 4. Appropriately label the data set with descriptive activity names.
 names(data)[1:2] <- c("subject","activity")
 names(data)<-gsub("[()]","", names(data))
 names(data)<-gsub("angle", "Angle", names(data), ignore.case = TRUE)
 names(data)<-gsub("gravity", "Gravity", names(data), ignore.case = TRUE)
 names(data)<-gsub("Acc", "Accelerometer", names(data), ignore.case = TRUE)
 names(data)<-gsub("Gyro", "Gyroscope", names(data), ignore.case = TRUE)
 names(data)<-gsub("BodyBody", "Body", names(data), ignore.case = TRUE)
 names(data)<-gsub("Mag", "Magnitude", names(data), ignore.case = TRUE)
 names(data)<-gsub("^t", "Time", names(data), ignore.case = TRUE)
 names(data)<-gsub("^f", "Frequency", names(data), ignore.case = TRUE)
 names(data)<-gsub("tBody", "TimeBody", names(data), ignore.case = TRUE)
 names(data)<-gsub("-mean", "_Mean", names(data), ignore.case = TRUE)
 names(data)<-gsub("-std", "_STD", names(data), ignore.case = TRUE)
 names(data)<-gsub("-freq", "_Frequency", names(data), ignore.case = TRUE)

 
 # Step 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
 
 #Load reshape2 package
 
 if (!"reshape2" %in% installed.packages())
 {
   install.packages("reshape2")
 }
 library(reshape2)
 
#Data set with average of each variable for each activity
 intermediate_data <- melt(data,(id.vars=c("subject","activity")))
 tidy_data <- dcast(intermediate_data, subject + activity ~ variable, mean)
 names(tidy_data)[c(3:length(names(tidy_data)))] <- paste("average(" , names(tidy_data)[c(3:length(names(tidy_data)))],")" )
 
 #Export the final data to working directory as a text file
 write.table(tidy_data, "final_data.txt", sep = ",",row.names = FALSE)
