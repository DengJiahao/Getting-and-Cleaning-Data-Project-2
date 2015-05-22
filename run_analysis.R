##load the fundamental library
library(dplyr)
library(reshape2)

##set the url and the direction
url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%2OHAR%20Dataset.zip"
zip <- "getdata-projectfiles-UCI HAR Dataset.zip"

direction <- "UCI HAR Dataset"

## check if the file exists
if (!file.exists(direction))
{
  if (!file.exists(zip))
  {
    
    download.file(url, destfile=zip)
  }
  unzip(zip)
}

## Read the data into R

raw.labels <- read.table("UCI HAR Dataset/activity_labels.txt",sep="")
raw.features <- read.table("UCI HAR Dataset/test/subject_test.txt",sep="",col.names=c("Subject"))

raw.test.subject <- read.table("UCI HAR Dataset/test/subject_test.txt",sep="", col.names=c("Subject"))
raw.test.x <- read.table("UCI HAR Dataset/test/X_test.txt",sep="")
raw.test.y <- read.table("UCI HAR Dataset/test/Y_test.txt",sep="",col.names=c("Activity"))

raw.train.subject <- read.table("UCI HAR Dataset/train/subject_train.txt",sep="", col.names=c("Subject"))
raw.train.x <- read.table("UCI HAR Dataset/train/X_train.txt",sep="")
raw.train.y <- read.table("UCI HAR Dataset/train/Y_train.txt",sep="",col.names=c("Activity"))

features <- read.table("UCI HAR Dataset/features.txt",sep="")




##adjust features' names to make a tidy name set
features$V1 <- NULL
features$V2 <- gsub(",|-", "", features$V2)
features$V2 <- gsub("\\(", "", features$V2)
features$V2 <- gsub("\\)", "", features$V2)


##initialize the names of raw. files

for (i in 1:nrow(features)) {
  names(raw.test.x)[i]<-features[i, ]
}

for (i in 1:nrow(features)) {
  names(raw.train.x)[i]<-features[i, ]
}


## conbine and merge the data
raw.test <- cbind(raw.test.subject,raw.test.y,raw.test.x)
raw.train <- cbind(raw.train.subject,raw.train.y,raw.train.x)
raw.data <- rbind(raw.test, raw.train)

##clean the data/ get rid of the useless ones

w.mean <- glob2rx("*mean*")
w.std <- glob2rx("*std*")
w.meanFreq <- glob2rx("*Freq*")

##addint and removing the some variables
mean.variables <- with(features, features[grep(w.mean, V2), ])
std.variables <- with(features, features[grep(w.std, V2), ])

meanFreq.variables <- with(features, features[grep(w.meanFreq, V2), ])

needed.variables <- c("Subject", "Activity", mean.variables, std.variables)

working.data <- raw.data[ , which(names(raw.data) %in% needed.variables)]
working.data <- working.data[ , -which(names(working.data) %in% meanFreq.variables)]

working.data <- within(working.data, {
  Activity <- gsub(1, "WALKING", Activity)
  Activity <- gsub(2, "WALKING_UPSTAIRS", Activity)
  Activity <- gsub(3, "WALKING_DOWNSTAIRS", Activity)
  Activity <- gsub(4, "SITTING", Activity)
  Activity <- gsub(5, "STANDING", Activity)
  Activity <- gsub(6, "LAYING", Activity)
})

data.melt <- melt(working.data, id = c("Subject", "Activity"))


tidy.table <- dcast(data.melt, Subject + Activity ~ variable)
tidy.table.mean <- dcast(data.melt, Subject + Activity ~ variable, mean)

##write the data into a document
format.tidy.data <- format(tidy.table.mean, digits=10, scientific=FALSE, justify='right')
write.table(format.tidy.data, row.name = FALSE, sep ='\t', file = "tidy.data.txt", quote=FALSE)