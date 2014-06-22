#read the names of the features.
feature_names<-read.table("UCI HAR Dataset/features.txt")
#extract the required column names 
columns_for_mean_values<-grep("mean|std", feature_names$V2)
#find how many columns are there..
all_columns<-read.table("UCI HAR Dataset/test/X_test.txt", na.strings="N/A", nrows=1)
#populate colClass which would be used to read required columns later
colClasses <- rep('NULL', ncol(all_columns))
colClasses[columns_for_mean_values]<-NA

activity_names<-read.table("UCI HAR Dataset/activity_labels.txt")
#read the training set data
train_set_mean_columns<-read.table("UCI HAR Dataset/train/X_train.txt", na.strings="N/A", colClasses=colClasses)
#read subject ids and activity ids
subject_train<-read.table("UCI HAR Dataset/train/subject_train.txt", na.strings="N/A")
y_train<-read.table("UCI HAR Dataset/train/y_train.txt", na.strings="N/A")
#merge the subject id and activity ids.
train_set_mean_columns<-cbind(y_train, train_set_mean_columns)
train_set_mean_columns<-cbind(subject_train, train_set_mean_columns)


#read test set data
test_set_mean_columns<-read.table("UCI HAR Dataset/test/X_test.txt", na.strings="N/A", colClasses=colClasses)
#read subject ids and activity ids
y_test<-read.table("UCI HAR Dataset/test/y_test.txt", na.strings="N/A")
subject_test<-read.table("UCI HAR Dataset/test/subject_test.txt", na.strings="N/A")
#merge the subject id and activity ids.
test_set_mean_columns<-cbind(y_test, test_set_mean_columns)
test_set_mean_columns<-cbind(subject_test, test_set_mean_columns)

#merge test and training sets.

combined_test_train_mean<-rbind(train_set_mean_columns, test_set_mean_columns)
# replace activity ids with activity names
combined_test_train_mean[,2]<-activity_names[combined_test_train_mean[,2],2]


# change column names to more human readable form

column_names<-feature_names[columns_for_mean_values,2]
column_names<-c("Subject", "Activity", as.character(column_names))
column_names<-gsub("^t","Time of ", column_names)
column_names<-gsub("^f","Frequency of ", column_names)
column_names<-gsub("(.*)-mean(.*)","Mean \\1\\2", column_names)
column_names<-gsub("(.*)-std(.*)","Standard Deviation \\1\\2", column_names)
column_names<-gsub("()-X"," in X direction", column_names)
column_names<-gsub("()-Y"," in Y direction", column_names)
column_names<-gsub("()-Z"," in Z direction", column_names)
column_names<-gsub("Mag"," Magnitude ", column_names)
column_names<-gsub("Acc"," Acceleration ", column_names)
column_names<-gsub("\\()","",column_names)
column_names<-gsub("  "," ", column_names)
column_names<-gsub(" ","_",column_names)
#assign column names to data frame.
names(combined_test_train_mean)<-column_names


# create new table for average of all the columns and create a new data frame.

# find the ordered subject ids and activity ids.
subjects<-sort(unique(combined_test_train_mean[,1]))
activities<-sort(unique(as.character(combined_test_train_mean[,2])))

# new to find average per column, find the column count.
num_col<- ncol(combined_test_train_mean)

#cerate empty data frame.
averaged_test_train<-data.frame(matrix(nrow=0, ncol=length(column_names)))

# Loop over each subject id and then, on each activity id. For each combination of subject and activity 
# create one row. And merge this row to the data frame.
row_number<-1
for(sub in subjects) {
   for( act in activities) {
       row<-c(sub,act) # first two columns of each row is object id and activity name.
       averaged_test_train[row_number,1]=sub
       averaged_test_train[row_number,2]=act
       # extract from combined table rows which corresponds to a object and activity combination.
       one_frame<-combined_test_train_mean[combined_test_train_mean$Subject==sub&combined_test_train_mean$Activity==act, ]
       # go through each of the columns with numeric values and find the average (mean and average would be same).
       # and keep on adding the mean to the row.
       for(c in 3:num_col) {
          #row<-c(row, mean(one_frame[,c]))
          averaged_test_train[row_number,c]=mean(one_frame[,c])
       }

       row_number<-row_number+1
   }
}
names(averaged_test_train)<-column_names
library(MASS)
write.matrix(averaged_test_train, "averaged_test_train.txt",sep=", ")

