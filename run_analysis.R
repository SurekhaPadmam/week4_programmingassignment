# Programming assignment week 4
# Use UCI data set
# write a script which does the following:



##1. Merge the training and test sets to create one dataset

setwd('C:/Users/mikeb/Documents/week4_programmingassignment/UCI HAR Dataset');

features <- read.table('./features.txt',header=FALSE);
activity_labels <- read.table('./activity_labels.txt',header=FALSE); 
  colnames(activity_labels) <- c("activityId","activityType");
subject_train <- read.table('./train/subject_train.txt',header=FALSE); 
  colnames(subject_train) <- "subjectId";
subject_test <- read.table('./test/subject_test.txt',header=FALSE); 
  colnames(subject_test) <- "subjectId";
x_train <- read.table('./train/x_train.txt',header=FALSE); colnames(x_train) <- 
  features[,2];
y_train <- read.table('./train/y_train.txt',header=FALSE); colnames(y_train) <- 
  "activityId";
x_test <- read.table('./test/x_test.txt',header=FALSE); colnames(x_test) <- 
  features[,2];
y_test <- read.table('./test/y_test.txt',header=FALSE); colnames(y_test) <- 
  "activityId";
training_dataset = cbind(y_train,subject_train,x_train);
test_dataset = cbind(y_test,subject_test,x_test);
merged_data = rbind(training_dataset,test_dataset);
column_names <- colnames(merged_data);


## 2. Extracts only measurements on mean and standard deviation for each measurement

extract <- (grepl("activity..",column_names) | grepl("subject..",column_names) | grepl("-mean..",column_names) &
             !grepl("-meanFreq..",column_names) & !grepl("mean..-",column_names) | 
             grepl("-std..",column_names) & !grepl("-std()..-",column_names));
merged_data <- merged_data[extract==TRUE];


## 3. Use descriptive activity names 

merged_data <- merge(merged_data,activity_labels,by='activityId',all.x=TRUE);
merged_data$activityId <-activity_labels[,2][match(merged_data$activityId, activity_labels[,1])] 
column_names <- colnames(merged_data);

## 4. Appropriately label the data set with descriptive activity names.

for (i in 1:length(column_names)) 
{
  column_names[i] <- gsub("\\()","",column_names[i])
  column_names[i] <- gsub("-std$","Std_Deviation",column_names[i])
  column_names[i] <- gsub("-mean","Mean",column_names[i])
  column_names[i] <- gsub("^(t)","Time",column_names[i])
  column_names[i] <- gsub("^(f)","Freq",column_names[i])
  column_names[i] <- gsub("([Gg]ravity)","Gravity",column_names[i])
  column_names[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body_Acceleration",column_names[i])
  column_names[i] <- gsub("[Gg]yro","Gyro_Measure",column_names[i])
  column_names[i] <- gsub("AccMag","Acceleration_Magnitude",column_names[i])
  column_names[i] <- gsub("([Bb]odyaccjerkmag)","Body_Acceleration_Jerk",column_names[i])
  column_names[i] <- gsub("JerkMag","Jerk_Magnitude",column_names[i])
  column_names[i] <- gsub("GyroMag","Gyro_Magnitude",column_names[i])
};

colnames(merged_data) <- column_names;
merged_data <- merged_data[,names(merged_data) != 'activityType'];

## 5. From this data set, create a second tidy data set with the aveage of each variable and subject

tidy_data <- aggregate(merged_data[,names(merged_data) 
                                    != c('activityId','subjectId')],by=list
                      (activityId=merged_data$activityId,
                        subjectId=merged_data$subjectId),mean);

write.table(tidy_data, './Final_Tidy_Data.txt',row.names=FALSE,sep='\t')