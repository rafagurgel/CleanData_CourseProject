library(dplyr)
library(data.table)
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", "UCI HAR Dataset.zip", method = "curl")
unzip("UCI HAR Dataset.zip")
fields  <- list(Subjects  = "subject", Activities = "y", Features = "X")

# Loading train set
str <- "train"
tmp.data<- lapply(file.path("UCI HAR Dataset",str,paste0(fields,"_",str,".txt")),fread)
train.data <- as_tibble(data.frame(Subjects = tmp.data[[1]], Activities = tmp.data[[2]], Features = tmp.data[[3]]))
# Loading test set
str <- "test"
tmp.data<- lapply(file.path("UCI HAR Dataset",str,paste0(fields,"_",str,".txt")),fread)
test.data <- as_tibble(data.frame(Subjects = tmp.data[[1]], Activities = tmp.data[[2]], Features = tmp.data[[3]]))
# Removing tmp.data
rm(list=c("tmp.data","fields","str"))

# Load labels
features.labels <- fread("UCI HAR Dataset/features.txt", sep=" ", data.table = FALSE)[,2]
activity.labels <- fread("UCI HAR Dataset/activity_labels.txt", sep=" ", data.table = FALSE)[,2]


# 1. Merges the training and the test sets to create one data set.
full.data <- rbind(train.data,test.data)
rm(list=c("train.data","test.data"))


# 4. Appropriately labels the data set with descriptive variable names.
names(full.data)<-make.names(c("Subject",
                    "Activity",
                    features.labels))
# There are some columns with duplicated names
dups<- which(duplicated(names(full.data)))
dups.names <- names(full.data)[dups]
# First I'll check if their contents are equal. I've noticed that's one one repetition
ident <- vector(mode="logical", length=0)
for (i in unique(dups.names)){
    idx<-which(names(full.data)==i)
    ident<- cbind(ident,identical(full.data[,idx[1]],full.data[,idx[2]]))
    features.labels[idx[1]] <- paste0(features.labels[idx[1]],"_1")
    features.labels[idx[2]] <- paste0(features.labels[idx[2]],"_2")
}
any(ident)
rm(list=c("dups","dups.names","i","idx","ident"))
# As they have different data, I can't remove, so I'll add something in their name. Actually I did this in the last loop, so
names(full.data)<-c("Subject",
                    "Activity",
                    features.labels)
rm(list=c("features.labels"))
print(names(full.data))


# 3. Uses descriptive activity names to name the activities in the data set
full.data<-full.data%>% 
    mutate(Activity = as.factor(Activity))
levels(full.data$Activity)<-activity.labels      
rm(list=c("activity.labels"))
table(full.data$Activity)


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
mean.std.data<-full.data%>%
    select(c(Subject,
             Activity,
             contains("mean()"),contains("std()")))


# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
summary.data <- full.data%>%
    mutate(Subject = as.factor(Subject))%>%
    group_by(Activity,Subject)%>%
    summarise_all(funs(mean))
print(summary.data)
write.table(summary.data, file = "tidydata.txt", row.name=FALSE)
