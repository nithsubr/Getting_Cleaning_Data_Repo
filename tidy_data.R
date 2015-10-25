# This funtion gets the tidy data with the average of each variable for each activity and each subject.

tidy_data <- function()
{
  
  library(plyr)
  
  # Download the zip file
  if (!dir.exists("./acc_data")) {dir.create("./acc_data")}
  fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileurl, "./acc_data/acc_data.zip")
  
  # Get the list of file names
  fnames <- unzip("./acc_data/acc_data.zip", list = TRUE)
  test_fnames <- fnames[grepl("_test.txt", fnames$Name),]
  train_fnames <- fnames[grepl("_train.txt", fnames$Name),]
  
  # Copy the test files to working directory
  for (i in 1:nrow(test_fnames))
  {
    unzip("./acc_data/acc_data.zip", files = test_fnames$Name[i], exdir = "./acc_data", overwrite = TRUE)
    fpath <- paste("./acc_data", test_fnames$Name[i], sep = "/")
    posn <- (regexpr("\\/[^\\/]*$", test_fnames$Name[i])) + 1
    fnam <- substr(test_fnames$Name[i], posn, nchar(test_fnames$Name[i]))
    file.copy(fpath, fnam)
  }
  
  # Copy the training files to working directory
  for (i in 1:nrow(train_fnames))
  {
    unzip("./acc_data/acc_data.zip", files = train_fnames$Name[i], exdir = "./acc_data", overwrite = TRUE)
    fpath <- paste("./acc_data", train_fnames$Name[i], sep = "/")
    posn <- (regexpr("\\/[^\\/]*$", train_fnames$Name[i])) + 1
    fnam <- substr(train_fnames$Name[i], posn, nchar(train_fnames$Name[i]))
    file.copy(fpath, fnam)
  }
  
  # Get the consolidated Accelerometer readings (w/o gravity) per Subject per activity for test environment
  data_body_test <- merge_files(file_x = "body_acc_x_test.txt", 
                                file_y = "body_acc_y_test.txt",
                                file_z = "body_acc_z_test.txt",
                                file_fact = "y_test.txt",
                                file_subject = "subject_test.txt",
                                file_labels = "activity_labels.txt",
                                variable = "Body")
  
  # Get the consolidated Gyroscope readings per Subject per activity for test environment
  data_gyro_test <- merge_files(file_x = "body_gyro_x_test.txt", 
                                file_y = "body_gyro_y_test.txt",
                                file_z = "body_gyro_z_test.txt",
                                file_fact = "y_test.txt",
                                file_subject = "subject_test.txt",
                                file_labels = "activity_labels.txt",
                                variable = "Gyro")
  
  # Get the consolidated Accelerometer (w gravity) readings per Subject per activity for test environment
  data_total_test <- merge_files(file_x = "total_acc_x_test.txt", 
                                 file_y = "total_acc_y_test.txt",
                                 file_z = "total_acc_z_test.txt",
                                 file_fact = "y_test.txt",
                                 file_subject = "subject_test.txt",
                                 file_labels = "activity_labels.txt",
                                 variable = "Total")
  
  # Get the consolidated Accelerometer readings (w/o gravity) per Subject per activity for training environment
  data_body_train <- merge_files(file_x = "body_acc_x_train.txt",
                                 file_y = "body_acc_y_train.txt",
                                 file_z = "body_acc_z_train.txt",
                                 file_fact = "y_train.txt",
                                 file_subject = "subject_train.txt",
                                 file_labels = "activity_labels.txt",
                                 variable = "Body")
  
  # Get the consolidated Gyroscope readings per Subject per activity for training environment
  data_gyro_train <- merge_files(file_x = "body_gyro_x_train.txt",
                                 file_y = "body_gyro_y_train.txt",
                                 file_z = "body_gyro_z_train.txt",
                                 file_fact = "y_train.txt",
                                 file_subject = "subject_train.txt",
                                 file_labels = "activity_labels.txt",
                                 variable = "Gyro")
  
  # Get the consolidated Accelerometer (w gravity) readings per Subject per activity for training environment
  data_total_train <- merge_files(file_x = "total_acc_x_train.txt",
                                  file_y = "total_acc_y_train.txt",
                                  file_z = "total_acc_z_train.txt",
                                  file_fact = "y_train.txt",
                                  file_subject = "subject_train.txt",
                                  file_labels = "activity_labels.txt",
                                  variable = "Total")
  
  # Final Consolidation
  body_final <- rbind(data_body_test, data_body_train)
  gyro_final <- rbind(data_gyro_test, data_gyro_train)
  total_final <- rbind(data_total_test, data_total_train)
  
  
  summary_body <- ddply(body_final, c("Subject", "Measure"), summarize, body_x_mean = mean(x), 
                                                                        body_y_mean = mean(y), 
                                                                        body_z_mean = mean(z))

  
  summary_gyro <- ddply(gyro_final, c("Subject", "Measure"), summarize, gyro_x_mean = mean(x), 
                                                                        gyro_y_mean = mean(y), 
                                                                        gyro_z_mean = mean(z))

                        
  summary_total <- ddply(total_final, c("Subject", "Measure"), summarize, total_x_mean = mean(x), 
                                                                          total_y_mean = mean(y), 
                                                                          total_z_mean = mean(z))

  final_data <- merge(summary_body, summary_gyro, by = c("Subject", "Measure"))
  final_data <- merge(final_data, summary_total, by = c("Subject", "Measure"))
  
  final_data$Subject <- as.numeric(as.character(final_data$Subject))
  final_data[order(final_data$Subject, final_data$Measure), ]
  
  write.table(x = final_data, file = "Tidy_Data.txt", row.names = FALSE, sep = "\t", eol = "\n")
 
}



# This function merges the files
# created this as a separate R code to modularize and re-use this repeating piece of code

merge_files <- function(file_x, file_y, file_z, file_fact, file_subject, file_labels, variable)
{
  
  library(reshape2)
  library(plyr)
  
  # Check for existance of supplied files
  if(!file.exists(file_x) | 
     !file.exists(file_y) |
     !file.exists(file_z) |
     !file.exists(file_fact) |
     !file.exists(file_subject)) 
  {
    print("Mandatory Files for activity reading for body, do not exist")
    return
  }
  
  if(!file.exists(file_labels))
  {
    print("Mandatory file with descriptive test for list of activities, do not exist")
    return
  }
  
  # Read the files
  data_x <- read.table(file_x)
  data_y <- read.table(file_y)
  data_z <- read.table(file_z)
  data_fact <- read.table(file_fact)
  data_labels <- read.table(file_labels)
  data_subject <- read.table(file_subject)
  
  # Bind all the X's, Y's and Z's
  data_x <- cbind(data_subject$V1, data_fact$V1, data_x)
  data_y <- cbind(data_subject$V1, data_fact$V1, data_y)
  data_z <- cbind(data_subject$V1, data_fact$V1, data_z)
  
  # Melt the data so that each row contains an X / Y / Z value for each Subject and Measure
  data_x_melt <- melt(data_x, id = c("data_subject$V1", "data_fact$V1"), value.name = "x")
  data_y_melt <- melt(data_y, id = c("data_subject$V1", "data_fact$V1"), value.name = "y")
  data_z_melt <- melt(data_z, id = c("data_subject$V1", "data_fact$V1"), value.name = "z")
  
  # Combine the X, Y and Z values for each reading
  data <- cbind(data_x_melt, data_y_melt$y, data_z_melt$z)
  colnames(data) <- c("Subject", "Measure", "Variable", "x", "y", "z")
  data$Variable <- variable
  
  for (i in 1:nrow(data_labels))
  {
    data[data$Measure == data_labels$V1[i], "Measure"] <- as.vector(data_labels$V2[i])
  }
  
  #Return the formatted data
  return(data)
  
}