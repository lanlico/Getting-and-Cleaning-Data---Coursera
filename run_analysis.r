## Alan Guilherme de Oliveira
## 21/12/2015
## runAnalysis.r 

# This script will perform the following steps on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

#First steps

#Install packages data.table and reshape2

  install.packages("data.table")
  install.packages("reshape2")
  
#Load the installed packages
  
  require("data.table")
  require("reshape2")

#set working directory to the location where the UCI HAR Dataset was unzipped (I use Windows)
 
setwd("C:\\Users\\C140954\\Downloads\\UCI HAR Dataset")

# Read all the data
 
activityLabels <- read.table("./activity_labels.txt")[,2] 
features <- read.table("./features.txt")[,2]
  
# Load X_train and Y_train data
	
X_train <- read.table("./train/X_train.txt")
Y_train <- read.table("./train/Y_train.txt")
subjectTrain <- read.table("./train/subject_train.txt")
  
# Load X_test and Y_test data.
	
X_test <- read.table("./test/X_test.txt")
Y_test <- read.table("./test/Y_test.txt")
subjectTest <- read.table("./test/subject_test.txt")
	
# Assign column names to the test data imported above

Y_test[,2] = activityLabels[Y_test[,1]]
names(Y_test) = c("ActivityID", "ActivityLabel")
names(subjectTest) = "subject"
	
Y_train[,2] = activityLabels[Y_train[,1]]
names(Y_train) = c("ActivityID", "ActivityLabel")
names(subjectTrain) = "subject"

#TRUE values for the ID, mean() & stddev() columns and FALSE for others
		
extractFeatures <- grepl("mean|std", features)
		
names(X_test) = features
X_test = X_test[,extractFeatures]
		
names(X_train) = features
X_train = X_train[,extractFeatures]

# Create the final training set by merging Y_train, X_train and subjectTrain

trainData <- cbind(as.data.table(subjectTrain), Y_train, X_train)

# Create the final test set by merging the X_test, Y_test and subjectTest

testData <- cbind(as.data.table(subjectTest), Y_test, X_test)
		
# Merge training and test data to create a final data set

finalData = rbind(testData, trainData)
	
# Create a vector for the column names from the finalData
		
colNames  = colnames(finalData); 

# Appropriately label the data set with descriptive activity names
idLabels   = c("subject", "ActivityID", "ActivityLabel")
dataLabels = setdiff(colNames, idLabels)
meltData  = melt(finalData, id = idLabels, measure.vars = dataLabels)

# Apply mean function to dataset

tidy_data   = dcast(meltData, subject + ActivityLabel ~ variable, mean)
write.table(tidy_data, file = "./tidy_data.txt",row.names=FALSE, sep='\t')

	
		
		


		


  
  

  
