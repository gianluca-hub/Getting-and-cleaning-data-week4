### download and unzip files ###

urlpath <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(urlpath, destfile = "Dataset.zip")
unzip("Dataset.zip", exdir = "Dataset")
list.files("Dataset")

### read general data ###

dirpath <- "/Users/gianluca/Desktop/Getting-and-cleaning-data-week4/Dataset/UCI HAR DATASET/"
activity_labels <- read.table(paste(dirpath, "activity_labels.txt", sep = ""), col.names = 
                                      c("activity_labels", "activity"))
features <- read.table(paste(dirpath, "features.txt", sep = ""), col.names = 
                               c("features_labels", "features"))

### Preapre a vector of descriptive variable names for Set_train and Set_value###

Set_value <- features[, 2]
Set_valuenames <- gsub("-mean", "Mean", Set_value)
Set_valuenames <- gsub("-std", "Std", Set_value)
Set_valuenames <- gsub("[-()]", "", Set_valuenames)


### extract test data ###

dirpathtest <- "/Users/gianluca/Desktop/Getting-and-cleaning-data-week4/Dataset/UCI HAR DATASET/test/"
Subject_test <- read.table(paste(dirpathtest, "subject_test.txt", sep = ""), col.names = 
                                   "subject")
Set_test <- read.table(paste(dirpathtest, "X_test.txt", sep = ""), col.names = 
                               Set_valuenames)
Labels_test <- read.table(paste(dirpathtest, "y_test.txt", sep = ""), col.names = 
                                  "activity_labels_test")


#### extract train data ###


dirpathtrain <- "/Users/gianluca/Desktop/Getting-and-cleaning-data-week4/Dataset/UCI HAR DATASET/train/"
Subject_train <- read.table(paste(dirpathtrain, "subject_train.txt", sep = ""), col.names = 
                                    "subject")
Set_train <- read.table(paste(dirpathtrain, "X_train.txt", sep = ""), col.names = 
                                Set_valuenames)
Labels_train <- read.table(paste(dirpathtrain, "y_train.txt", sep = ""),
                           col.names = "activity_labels_train")


### ad id column ###
Labels_test <- Labels_test %>% mutate(id = row_number())
Labels_train <- Labels_train %>% mutate(id = row_number())
Set_test <- Set_test %>% mutate(id = row_number())
Set_train <- Set_train %>% mutate(id = row_number())
Subject_test <- Subject_test %>% mutate(id = row_number())
Subject_train <- Subject_train %>% mutate(id = row_number())

### subset mean and std columns of Set_test and Set_train ###

Set_test <- Set_test[, grepl("mean|std|id", names(Set_test))]
Set_train <- Set_train[, grepl("mean|std|id", names(Set_train))]


### merge Labels database with activity_labels ###

Labels_test <- merge.data.frame(activity_labels, Labels_test, by.x = "activity_labels", by.y = 
                                        "activity_labels_test")
Labels_train <- merge.data.frame(activity_labels, Labels_train, by.x = "activity_labels", by.y = 
                                         "activity_labels_train")


### merge Labels database with subject id ###

Labels_test <- merge.data.frame(Labels_test, Subject_test, by.x = "id", by.y = "id")
Labels_train <- merge.data.frame(Labels_train, Subject_train, by.x = "id", by.y = "id")

### merge test database ###

Testdb <- merge.data.frame(Labels_test, Set_test, by.x = "id", by.y = "id")

### merge train databases ###

Traindb <- merge.data.frame(Labels_train, Set_train, by.x = "id", by.y = "id")

### add group column to test and train databases ###

Testdb["group"] = "Test"
Traindb["group"] = "Train"

### merge test and train databases to create one dataset ###

Completedb <- rbind(Testdb, Traindb)

### reorder Complete db to make it more readble (first I use moveMe function, ###
### than i put activity_labels to NULL) ###
### Recall moveMe function ###

moveMe <- function(data, tomove, where = "last", ba = NULL) {
        temp <- setdiff(names(data), tomove)
        x <- switch(
                where,
                first = data[c(tomove, temp)],
                last = data[c(temp, tomove)],
                before = {
                        if (is.null(ba)) stop("must specify ba column")
                        if (length(ba) > 1) stop("ba must be a single character string")
                        data[append(temp, values = tomove, after = (match(ba, temp)-1))]
                },
                after = {
                        if (is.null(ba)) stop("must specify ba column")
                        if (length(ba) > 1) stop("ba must be a single character string")
                        data[append(temp, values = tomove, after = (match(ba, temp)))]
                })
        x
}

### reorder Completedb ###

Completedb <- moveMe(Completedb, c("subject", "group"), "after", "id")
Completedb$activity_labels <- NULL

### create Tidy dataset with mean of each variable for each activity and each subject ###

Averagedb <- Completedb %>% group_by(subject, activity) %>% summarise_at(vars
                (tBodyAccmeanX:fBodyBodyGyroJerkMagmeanFreq), mean , na.rm = TRUE)







