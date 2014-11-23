run_analysis <- function(){
        #This script takes the Samsung data and processes it by doing the following:
        # 1. Merges the training and the test sets to create one data set.
        # 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
        # 3. Uses descriptive activity names to name the activities in the data set
        # 4. Appropriately labels the data set with descriptive variable names. 
        # 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
        #
        # The output is the summary data from step 5
        
        
        #reads in the X data for the test and train X data, and combines it into one
        testX <- read.table("./UCI HAR Dataset/test/X_test.txt")
        trainX <- read.table("./UCI HAR Dataset/train/X_train.txt")
        combinedX <- rbind(testX, trainX)
        
        #reads in the descriptive variable names and applies it to the data
        varLabels <- read.table("./UCI HAR Dataset/features.txt")
        colnames(combinedX) <- varLabels[,2]
        
        
        #reads in the descriptive activity labels
        activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt")
        
        #reads in the Y data for both the test and train Y data, and combines it into one
        testY <- read.table("./UCI HAR Dataset/test/y_test.txt")
        trainY <- read.table("./UCI HAR Dataset/train/y_train.txt")
        combinedY <- rbind(testY, trainY)
        
        #applies the activity labels as a factor to the Y data. Binds the Y and X data into one
        colnames(combinedY) <- c("activity")
        combinedData <- cbind(combinedY, combinedX)
        combinedData$activity <- factor(combinedData$activity, levels = activityLabels[,1], labels = activityLabels[,2])
        
        #reads in the Subjects label for both the test and train data and combines them
        testSubjects <- read.table("./UCI HAR Dataset/test/subject_test.txt")
        trainSubjects <- read.table("./UCI HAR Dataset/train/subject_train.txt")
        combinedSubjects <- rbind(testSubjects,trainSubjects)
        
        #adds to the main data table the Subject IDs
        colnames(combinedSubjects) <- c("Subject_ID")
        combinedData <- cbind(combinedSubjects, combinedData)
        
        #filters out the data to only include mean() and std() measurements
        includeCols <- grep("mean\\(\\)|std\\(\\)", colnames(combinedData))
        includeCols <- c(1,2, includeCols)
        combinedData <- combinedData[,includeCols]      
        
        #uses reshape2 package to melt and dcast the data to summarize showing the
        #mean of each variable by Subject ID and activity.
        #Returns this summary data as the output of the function.
        library(reshape2)
        meltTidy <- melt(combinedData, id=c("Subject_ID", "activity"), measure.vars = colnames(combinedData[,3:68]))
        processedData <- dcast(meltTidy, Subject_ID + activity ~ variable, mean)
        
}


