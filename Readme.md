This funtion gets the tidy data with the average of each variable for each activity and each subject.
=====================================================================================================

tidy\_data \<- function() {

library(plyr)

\# Download the zip file \#if (!dir.exists("./acc\_data"))
{dir.create("./acc\_data")} \#fileurl \<-
"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
\#download.file(fileurl, "./acc\_data/acc\_data.zip")

\# Get the list of file names fnames \<-
unzip("./acc\_data/acc\_data.zip", list = TRUE) test\_fnames \<-
fnames[grepl("\_test.txt", fnames\$Name),] train\_fnames \<-
fnames[grepl("\_train.txt", fnames\$Name),]

\# Copy the test files to working directory for (i in
1:nrow(test\_fnames)) { unzip("./acc\_data/acc\_data.zip", files =
test\_fnames$Name[i], exdir = "./acc_data", overwrite = TRUE)     fpath <- paste("./acc_data", test_fnames$Name[i],
sep = "/") posn \<- (regexpr("\\/[\^\\\\/]\*$", test_fnames$Name[i])) +
1 fnam \<-
substr(test\_fnames$Name[i], posn, nchar(test_fnames$Name[i]))
file.copy(fpath, fnam) }

\# Copy the training files to working directory for (i in
1:nrow(train\_fnames)) { unzip("./acc\_data/acc\_data.zip", files =
train\_fnames$Name[i], exdir = "./acc_data", overwrite = TRUE)     fpath <- paste("./acc_data", train_fnames$Name[i],
sep = "/") posn \<- (regexpr("\\/[\^\\\\/]\*$", train_fnames$Name[i])) +
1 fnam \<-
substr(train\_fnames$Name[i], posn, nchar(train_fnames$Name[i]))
file.copy(fpath, fnam) }

\# Get the consolidated Accelerometer readings (w/o gravity) per Subject
per activity for test environment data\_body\_test \<-
merge\_files(file\_x = "body\_acc\_x\_test.txt", file\_y =
"body\_acc\_y\_test.txt", file\_z = "body\_acc\_z\_test.txt", file\_fact
= "y\_test.txt", file\_subject = "subject\_test.txt", file\_labels =
"activity\_labels.txt", variable = "Body")

\# Get the consolidated Gyroscope readings per Subject per activity for
test environment data\_gyro\_test \<- merge\_files(file\_x =
"body\_gyro\_x\_test.txt", file\_y = "body\_gyro\_y\_test.txt", file\_z
= "body\_gyro\_z\_test.txt", file\_fact = "y\_test.txt", file\_subject =
"subject\_test.txt", file\_labels = "activity\_labels.txt", variable =
"Gyro")

\# Get the consolidated Accelerometer (w gravity) readings per Subject
per activity for test environment data\_total\_test \<-
merge\_files(file\_x = "total\_acc\_x\_test.txt", file\_y =
"total\_acc\_y\_test.txt", file\_z = "total\_acc\_z\_test.txt",
file\_fact = "y\_test.txt", file\_subject = "subject\_test.txt",
file\_labels = "activity\_labels.txt", variable = "Total")

\# Get the consolidated Accelerometer readings (w/o gravity) per Subject
per activity for training environment data\_body\_train \<-
merge\_files(file\_x = "body\_acc\_x\_train.txt", file\_y =
"body\_acc\_y\_train.txt", file\_z = "body\_acc\_z\_train.txt",
file\_fact = "y\_train.txt", file\_subject = "subject\_train.txt",
file\_labels = "activity\_labels.txt", variable = "Body")

\# Get the consolidated Gyroscope readings per Subject per activity for
training environment data\_gyro\_train \<- merge\_files(file\_x =
"body\_gyro\_x\_train.txt", file\_y = "body\_gyro\_y\_train.txt",
file\_z = "body\_gyro\_z\_train.txt", file\_fact = "y\_train.txt",
file\_subject = "subject\_train.txt", file\_labels =
"activity\_labels.txt", variable = "Gyro")

\# Get the consolidated Accelerometer (w gravity) readings per Subject
per activity for training environment data\_total\_train \<-
merge\_files(file\_x = "total\_acc\_x\_train.txt", file\_y =
"total\_acc\_y\_train.txt", file\_z = "total\_acc\_z\_train.txt",
file\_fact = "y\_train.txt", file\_subject = "subject\_train.txt",
file\_labels = "activity\_labels.txt", variable = "Total")

\# Final Consolidation body\_final \<- rbind(data\_body\_test,
data\_body\_train) gyro\_final \<- rbind(data\_gyro\_test,
data\_gyro\_train) total\_final \<- rbind(data\_total\_test,
data\_total\_train)

summary\_body \<- ddply(body\_final, c("Subject", "Measure"), summarize,
body\_x\_mean = mean(x), body\_y\_mean = mean(y), body\_z\_mean =
mean(z))

summary\_gyro \<- ddply(gyro\_final, c("Subject", "Measure"), summarize,
gyro\_x\_mean = mean(x), gyro\_y\_mean = mean(y), gyro\_z\_mean =
mean(z))

summary\_total \<- ddply(total\_final, c("Subject", "Measure"),
summarize, total\_x\_mean = mean(x), total\_y\_mean = mean(y),
total\_z\_mean = mean(z))

final\_data \<- merge(summary\_body, summary\_gyro, by = c("Subject",
"Measure")) final\_data \<- merge(final\_data, summary\_total, by =
c("Subject", "Measure"))

final\_data$Subject <- as.numeric(as.character(final_data$Subject))
final\_data[order(final\_data$Subject, final_data$Measure), ]

write.table(x = final\_data, file = "Tidy\_Data.txt", row.names = FALSE,
sep = "\t", eol = "\n")

}

This function merges the files
==============================

created this as a separate R code to modularize and re-use this repeating piece of code
=======================================================================================

merge\_files \<- function(file\_x, file\_y, file\_z, file\_fact,
file\_subject, file\_labels, variable) {

library(reshape2) library(plyr)

\# Check for existance of supplied files if(!file.exists(file\_x) |
!file.exists(file\_y) | !file.exists(file\_z) | !file.exists(file\_fact)
| !file.exists(file\_subject)) { print("Mandatory Files for activity
reading for body, do not exist") return }

if(!file.exists(file\_labels)) { print("Mandatory file with descriptive
test for list of activities, do not exist") return }

\# Read the files data\_x \<- read.table(file\_x) data\_y \<-
read.table(file\_y) data\_z \<- read.table(file\_z) data\_fact \<-
read.table(file\_fact) data\_labels \<- read.table(file\_labels)
data\_subject \<- read.table(file\_subject)

\# Bind all the X's, Y's and Z's data\_x \<-
cbind(data\_subject$V1, data_fact$V1, data\_x) data\_y \<-
cbind(data\_subject$V1, data_fact$V1, data\_y) data\_z \<-
cbind(data\_subject$V1, data_fact$V1, data\_z)

\# Melt the data so that each row contains an X / Y / Z value for each
Subject and Measure data\_x\_melt \<- melt(data\_x, id =
c("data\_subject$V1", "data_fact$V1"), value.name = "x") data\_y\_melt
\<- melt(data\_y, id = c("data\_subject$V1", "data_fact$V1"), value.name
= "y") data\_z\_melt \<- melt(data\_z, id =
c("data\_subject$V1", "data_fact$V1"), value.name = "z")

\# Combine the X, Y and Z values for each reading data \<-
cbind(data\_x\_melt, data\_y\_melt$y, data_z_melt$z) colnames(data) \<-
c("Subject", "Measure", "Variable", "x", "y", "z") data\$Variable \<-
variable

for (i in 1:nrow(data\_labels)) {
data[data$Measure == data_labels$V1[i], "Measure"] \<-
as.vector(data\_labels\$V2[i]) }

\#Return the formatted data return(data)

}
