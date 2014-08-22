Getting and Cleaning Data Project

Data:  https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

Original description: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

The script (run_analysis.R) does the following:

1.Merges the training and the test sets to create one data set.

The script uses download.file to download the zip data, unzip the data and read them using read.table()
The script merges the train and test dataset


2.Extracts only the measurements on the mean and standard deviation for each measurement. 
The scripts reads the features data and extracts the mean and standard deviation for each measurment

   features <- read.table("project/UCI HAR Dataset/features.txt")
 
   #Find mean and standard deviation columns
    mean_col <- sapply(features[,2], function(x) grepl("mean()", x, fixed=T))
    std_col <- sapply(features[,2], function(x) grepl("std()", x, fixed=T))

    #get the mean and std deviation columns from the data
     cols <- mstd[,(mean_col | std_col)]
     colnames(cols) <- features[(mean_col | std_col), 2]


3.Uses descriptive activity names to name the activities in the data set

These activities include WALKING, WALKINGUPSTAIRS, WALKINGDOWNSTAIRS, SITTING, STANDING AND LAYING

4.Appropriately labels the data set with descriptive variable names. 

The first column contains subject IDS, the second column activity names and the other columns contains the measurements

5.Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
The final dataset as outputted as "write.table(final_tidy_data, "tidy.txt", row.names=FALSE)
