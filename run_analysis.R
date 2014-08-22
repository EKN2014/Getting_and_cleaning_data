#create working directory
if (!file.exists("C:/Users/Elizabeth/Desktop/Datascience/Coursera/Cleaning_Data_3/Project")) {
    dir.create("C:/Users/Elizabeth/Desktop/Datascience/Coursera/Cleaning_Data_3/Project")
}

#confirm working directory
getwd()

#download and unzip data to a directory named 'project'
url <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
file <- "project.zip"
data <- download.file(url, destfile=file)
unzip(file, exdir="project")

#read and merge data

merge_data = function() {
    x_train <- read.table("project/UCI HAR Dataset/train/X_train.txt")
    y_train <- read.table("project/UCI HAR Dataset/train/y_train.txt")
    subject_train <- read.table("project/UCI HAR Dataset/train/subject_train.txt")
    x_test <- read.table("project/UCI HAR Dataset/test/X_test.txt")
    y_test <- read.table("project/UCI HAR Dataset/test/y_test.txt")
    subject_test <- read.table("project/UCI HAR Dataset/test/subject_test.txt")

    #Merge the training and the test sets.

    merged.x <- rbind(x_train, x_test) 
    merged.y <- rbind(y_train, y_test) 
    merged.subject <- rbind(subject_train, subject_test)
    list(x=merged.x,y=merged.y,subject=merged.subject)
}    

#Extract only the measurements on the mean and standard deviation for each measurement

mean_std = function(mstd){

    #read the features dataset
    features <- read.table("project/UCI HAR Dataset/features.txt")

    #Find mean and standard deviation columns
    mean_col <- sapply(features[,2], function(x) grepl("mean()", x, fixed=T))
    std_col <- sapply(features[,2], function(x) grepl("std()", x, fixed=T))

    #get the mean and std deviation columns from the data
    cols <- mstd[,(mean_col | std_col)]
    colnames(cols) <- features[(mean_col | std_col), 2]
    cols
}

# Use descriptive activity names to name the activities in the dataset 
# and label the data set with descriptive variable names.

activities = function(act) { 

    colnames(act) <- "activity" 
    act$activity[act$activity == 1] = "WALKING" 
    act$activity[act$activity == 2] = "WALKING_UPSTAIRS" 
    act$activity[act$activity == 3] = "WALKING_DOWNSTAIRS" 
    act$activity[act$activity == 4] = "SITTING" 
    act$activity[act$activity == 5] = "STANDING" 
    act$activity[act$activity == 6] = "LAYING" 
    act 
} 
 
# Merge the mean and standard deviation values, subjects and activities
 
combine <- function(x, y, subjects) { 
    cbind(x, y, subjects) 
}  

# Create an independent tidy dataset with the average of each variable for each activity and each subject.
  
tidy_dataset = function(tidy) { 
    tidy_data <- ddply(tidy, .(subject, activity), function(x) colMeans(x[,1:60])) 
    tidy_data 
}  

#Now call the functions defined above and combine the mean and standard deviation, subjects and activities dataset
# to create the final clean data

clean_data = function() { 

    # Merged dataset     
    merged <- merge_data() 
     
    # Mean and standard deviation dataset
     mean_std_deviation <- mean_std(merged$x) 
      
    # Activities dataset
     
    activity <- activities(merged$y) 
     
    # Descriptive column name for subjects 
    colnames(merged$subject) <- c("subject") 
     
    # Now combine the mean and standard deviation, subject and activity datasets
    combined <- combine(mean_std_deviation, activity, merged$subject)
     
    # Create tidy dataset 
    final_tidy_data <- tidy_dataset(combined) 
     
    # Write the final tidy dataset as csv 
     write.table(final_tidy_data, "tidy.txt", row.names=FALSE) 
 }