library(dplyr)
library(tidyr)
library(downloader)

rm(list = ls(all = TRUE))

dir <- ""

#-------------------------------------------------------------------------
# Get and Extract Data

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip" 

setwd(paste(dir,"data", sep="\\"))

download(url,"dataset.zip")
unzip("dataset.zip")

x_tst <- read.table(".\\UCI HAR Dataset\\test\\X_test.txt", comment.char="", colClasses="numeric")
y_tst <- read.table(".\\UCI HAR Dataset\\test\\y_test.txt", comment.char="", colClasses="numeric")

x_tr <- read.table('.\\UCI HAR Dataset\\train\\X_train.txt', comment.char="", colClasses="numeric")
y_tr <- read.table('.\\UCI HAR Dataset\\train\\y_train.txt', comment.char="", colClasses="numeric")

sb_tr <- read.table(".\\UCI HAR Dataset\\train\\subject_train.txt")
sb_tst <- read.table(".\\UCI HAR Dataset\\test\\subject_test.txt")

feat <- read.table(".\\UCI HAR Dataset\\features.txt", stringsAsFactors=FALSE)

actv <- read.table(".\\UCI HAR Dataset\\activity_labels.txt", stringsAsFactors=FALSE)

#-------------------------------------------------------------------------
# combine files int to dataframes
#Merges the training and the test sets to create one data set.

train <- data.frame(subject = sb_tr$V1, activity = y_tr$V1, x_tr)
test <- data.frame(subject = sb_tst$V1, activity = y_tst$V1, x_tst)
rm(sb_tr, x_tr, y_tr, sb_tst, y_tst, x_tst)

dat <- rbind(train, test)
dim(dat) ; dim(train) ;dim(test)
class(dat)

#-------------------------------------------------------------------------
#Uses descriptive activity names to name the activities in the data set
actv
names(actv) <- c("activity","actv_name")
dat <- right_join(actv, dat)
table(dat$activity)

#-------------------------------------------------------------------------
#Appropriately labels the data set with descriptive variable names. 
names(feat) 
dim(feat)
colnames(dat)[ !colnames(dat) %in% c("activity","actv_name","subject") ] <- feat[ ,2]
names(dat)

#-------------------------------------------------------------------------
#Extracts only the measurements on the mean and standard deviation for each measurement. 

want <- c("subject","activity", "actv_name","mean", "Mean","std")
vars <- grep(paste(want, collapse="|"), colnames(dat), value=T)

dat2 <- dat[ , names(dat) %in% vars]

rm(train, test) ; rm(dat, actv, feat, want, vars)

#-------------------------------------------------------------------------
#From the data set in step 4, creates a second, independent tidy data set with the 
#average of each variable for each activity and each subject.

# tidy data princples - transposing with obs and labels
d_agg = dat2 %>% 
  select( -(activity)) %>%
   group_by(subject, actv_name) %>%
  summarise_each(funs(mean)) %>%
  gather(Measure, Values, -c(subject, actv_name)) %>%
  arrange(subject)

# cleaning up variable names
rmv <- c("\\(\\)","\\(\\)","-","\\(",",", ")")

d_agg$Measure <- tolower(sapply(d_agg$Measure, function(x) 
  gsub(paste(rmv, collapse = '|'), '',x)))

head(d_agg,10)
