Getting and Cleaning Data Project

1. Download the project data from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

2. Create and set your working directory

3. Unzip the data to the working directory in your local drive, eg, C:\Users\~\Desktop\Datascience\Coursera\Cleaning_Data_3\Project

4. Write a function that reads and merges the specific test and train datasets, eg, x_train and x_test. Use read.table() to read the data and rbind to merge the dataset.
   rbind merges the dataset rowwise

5. Write a function that reads the features dataset, extracts the mean and standard deviation 

6. Write a function that uses descriptive activity names to name the activities in the dataset and labels the data set with descriptive variable names

7. Write a function to merge the datasets created in the steps above

8. Finally call the functions defined above and create the tidy dataset

9. The script run_analysis.R script should be save in the same working directory created at step 2

10. Use data <- write.table(your tidy dataset.txt) to write the tidy dataset in your working directory