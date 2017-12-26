library(data.table)
library(dplyr)

### Creation of the Tidy data.
## combining the training and test data

    fileURL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    
    download.file(fileURL,destfile="./dataset.zip",method="curl")
    unzip(zipfile = "./dataset.zip")
    
    sub_train<- fread('./UCI HAR Dataset/train/subject_train.txt',sep="\n",header=F)
    sub_test<- fread('./UCI HAR Dataset/test/subject_test.txt',sep="\n",header=F)
    sub_data<-rbind(sub_train, sub_test)
    colnames(sub_data)<-"subjectID"
    
    
    y_train<- fread('./UCI HAR Dataset/train/y_train.txt')
    y_test<- fread('./UCI HAR Dataset/test/y_test.txt')
    y_data<-rbind(y_train, y_test)
    colnames(y_data)<-"activityNum"
    
    X_train<- fread('./UCI HAR Dataset/train/X_train.txt')
    X_test<- fread('./UCI HAR Dataset/test/X_test.txt')
    X_data<-rbind(X_train, X_test)
    
    X_names<-fread('./UCI HAR Dataset/features.txt')
    colnames(X_names)<-c("number", "feature")
    
    c(dim(X_data), dim(X_names))
    colnames(X_data)<-X_names[, feature]
    
    final_data <- cbind(sub_data, y_data, X_data)
    
    
    final_data <- as.data.frame(final_data)
    final_data <- final_data[, !duplicated(colnames(final_data))]
    final_data<- tbl_df(arrange(final_data, subjectID))

### Completion of the merging data & Tidy Data

##Extracts only the measurements on the mean and standard deviation to different data frame

    Avg_data<-select(final_data,subjectID,activityNum,matches(".mean().",ignore.case = TRUE),
                     matches(".std().",ignore.case = TRUE)) 

##END of Extracts only the measurements on the mean and standard deviation to different data frame

##Uses descriptive activity names to name the activities in the data set

    activity_names<-fread('./UCI HAR Dataset/activity_labels.txt')
    setnames(activity_names, c("V1", "V2"), c("activityNum", "activity"))
    activity_names$activity<-tolower(activity_names$activity)
    activity_names$activity<-gsub(pattern = "_", replacement = "", x = activity_names$activity)
    
## End of descriptive activity names to name the activities in the data set
    
##Appropriately labels the data set with descriptive variable names.    
    tidy_data<-merge(y = Avg_data,x = activity_names, by = "activityNum")
    tidy_data<-arrange(tidy_data, subjectID, activityNum)
    
##creates a second, independent tidy data set with the average of 
##each variable for each activity and each subject


    tidy_data1<-melt(tidy_data, id=c("activity", "subjectID"))
    final_result<-dcast(data = tidy_data1,formula = subjectID+activity~variable,
                         fun.aggregate = mean)
    final_result<-arrange(final_result, subjectID, activityNum)
    new_colNames<-paste("ave", colnames(final_result), sep=".")
    setnames(x = final_result,old = colnames(final_result[4:ncol(final_result)]), 
             new = new_colNames[4:length(new_colNames)])
    
    
    write.table(x = final_result, file = "./averagedData.txt", row.names = F)

## End of Program    
