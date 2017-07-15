# This file is for the Geatting and Cleaning data course project

#download data from the URL listed for the project
  if(!file.exists("./data")){dir.create("./data")}
  fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileUrl,destfile="./data/Dataset.zip")

# Unzip dataSet to /data directory
  unzip(zipfile="./data/Dataset.zip",exdir="./data")

#loading all data used for the analysis
  # Read in the data from files
  features     = read.table('./data/UCI HAR Dataset/features.txt',header=FALSE);
  activity_Type = read.table('./data/UCI HAR Dataset/activity_labels.txt',header=FALSE);
  subject_Train = read.table('./data/UCI HAR Dataset/train/subject_train.txt',header=FALSE);
  x_Train       = read.table('./data/UCI HAR Dataset/train/x_train.txt',header=FALSE);
  y_Train       = read.table('./data/UCI HAR Dataset/train/y_train.txt',header=FALSE);
  
  
  # Read in the test data
  subject_Test = read.table('./data/UCI HAR Dataset/test/subject_test.txt',header=FALSE);
  x_Test       = read.table('./data/UCI HAR Dataset/test/x_test.txt',header=FALSE);
  y_Test       = read.table('./data/UCI HAR Dataset/test/y_test.txt',header=FALSE);
  
#1 Merging the training and test data sets into one data set  
  # Assigining column names to the data loaded
  colnames(activity_Type)  = c('activity_Id','activity_Type');
  colnames(subject_Train)  = "subject_Id";
  colnames(x_Train)        = features[,2]; 
  colnames(y_Train)        = "activity_Id";
  
  # Assigining column names to the test data loaded
  colnames(subject_Test) = "subject_Id";
  colnames(x_Test)       = features[,2]; 
  colnames(y_Test)       = "activity_Id";
  
  # Merging x_Train, y_Train, subject_Train for the final Training_Data set
  training_Data = cbind(y_Train,subject_Train,x_Train);
  
  # Merging x_Test, y_Test, subject_Train for the final test_Data set 
  test_Data = cbind(y_Test,subject_Test,x_Test);
  
  
  # Merging training_Data and test_Data into the final_data set
  final_Data = rbind(training_Data,test_Data);
  
  # Creating col_Names from the final_Data, to calculate and store stdev and mean, for tidy data creation
  col_Names  = colnames(final_Data); 
  
#2 Extract only the measurements on the mean and standard deviation for each measurement 
   
  # Creating test_Vector for data testing, TRUE  for values of the ID, mean() & stddev() columns, FALSE for all other columns
  test_Vector = (grepl("activity..",col_Names) | grepl("subject..",col_Names) | grepl("-mean..",col_Names) & !grepl("-meanFreq..",col_Names) & !grepl("mean..-",col_Names) | grepl("-std..",col_Names) & !grepl("-std()..-",col_Names));
  
  # updating final_Data to only keep desirec columns, found in test_Vector
  final_Data = final_Data[test_Vector==TRUE];
  
 #3 Use descriptive activity names to name the activities in the data set
  
  # pulling in activity_Type for the descriptive names for each column
  final_Data = merge(final_Data,activity_Type,by='activity_Id',all.x=TRUE);
  
  # Pulling updated columns into col_Names
  col_Names  = colnames(final_Data); 
  
 #4 Appropriately label the data set with descriptive activity names. 
  
  #clean up for finalization
  for (i in 1:length(col_Names)) 
  {
    col_Names[i] = gsub("\\()","",col_Names[i])
    col_Names[i] = gsub("-std$","StdDev",col_Names[i])
    col_Names[i] = gsub("-mean","Mean",col_Names[i])
    col_Names[i] = gsub("^(t)","time",col_Names[i])
    col_Names[i] = gsub("^(f)","freq",col_Names[i])
    col_Names[i] = gsub("([Gg]ravity)","Gravity",col_Names[i])
    col_Names[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",col_Names[i])
    col_Names[i] = gsub("[Gg]yro","Gyro",col_Names[i])
    col_Names[i] = gsub("AccMag","Acc_Magnitude",col_Names[i])
    col_Names[i] = gsub("([Bb]odyaccjerkmag)","Body_Acc_Jerk_Magnitude",col_Names[i])
    col_Names[i] = gsub("JerkMag","Jer_kMagnitude",col_Names[i])
    col_Names[i] = gsub("GyroMag","Gyro_Magnitude",col_Names[i])
  };
  
  # Updating the final column names
  colnames(final_Data) = col_Names;

  
#5 Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
  
  # Create final data table, without activity_type
  preliminary_Final_Data  = final_Data[,names(final_Data) != 'activity_Type'];
  
  # pulling in the mean, stdev and name for each activtivty and subject
  tidy_Data    = aggregate(preliminary_Final_Data[,names(preliminary_Final_Data) != c('activity_Id','subject_Id')],by=list(activity_Id=preliminary_Final_Data$activity_Id,subject_Id = preliminary_Final_Data$subject_Id),mean);
  
  # Merging the tidyData with activityType to include descriptive acitvity names
  tidy_Data    = merge(tidy_Data,activity_Type,by='activity_Id',all.x=TRUE);
  
  # Export the tidyData set 
  write.table(tidy_Data, './tidy_Data.txt',row.names=TRUE,sep='\t');
