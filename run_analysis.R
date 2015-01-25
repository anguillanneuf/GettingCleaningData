# Load the dplyr library
library(dplyr)

# Read the training subjects and testing subjects from their respective folders
trainSubject <- read.table("./train/subject_train.txt", header=FALSE)
testSubject <- read.table("./test/subject_test.txt", header=FALSE)

# Create the subject column by rowbinding training and test subjects
subject <- rbind (trainSubject, testSubject)

# Read the activities and signal data from their respective folders
X_train <- read.table("./train/X_train.txt", header=FALSE)
y_train <- read.table("./train/y_train.txt", header=FALSE)
X_test <- read.table("./test/X_test.txt", header=FALSE)
y_test <- read.table("./test/y_test.txt", header=FALSE)

# Combine the signal data
set <- rbind(X_train, X_test)

# Combine the activities
activity <- rbind(y_train, y_test)

# Put subject, activity, and signal data into one file
data_step1 <- cbind(subject, activity, set)

# Read all the signal or variable names
features <- read.table("./features.txt", header=FALSE)

# Isolate the signal or variable names that denote mean and standard deviation
meanIndex <- grep("mean()", features[,2], fixed=TRUE)
stdIndex <- grep("std()", features[,2], fixed=TRUE)

# Create an index for signal data that denote mean and standard deviation
usefulIndex <- sort(c(meanIndex, stdIndex))

# Subset the dataset from step 1 to include mean and standard deviation only
data_step2 <- data_step1[, c(1:2, usefulIndex+2)]

# Create a vector of signal names
signalNames <- as.vector(features[,2][usefulIndex])

# Read all the activity labels
activityNums <- read.table("./activity_labels.txt", header=FALSE)

# Replace activity numbers with activity names
activityNames <- sapply(as.character(data_step2[,2]), switch,
			"1" = "walking",
			"2" = "walkingupstairs",
			"3" = "walkingdownstairs",
			"4" = "sitting",
			"5" = "standing",
			"6" = "laying")

# Update the dataste in step 2 with activity names
data_step3 <- data_step2
data_step3[,2] <- activityNames

# Edit signal or variable names
n <- 1
for(i in seq(signalNames)){
# Replace dashes with nothing
	signalNames[n] <- gsub("-", "", signalNames[n])
# Replace mean()
	signalNames[n] <- gsub("mean()", "Mean", signalNames[n], fixed=TRUE)
# Replace std()
	signalNames[n] <- gsub("std()", "Std", signalNames[n], fixed=TRUE)
# Replace "BobyBody"
	signalNames[n] <- gsub("BodyBody", "Body", signalNames[n], fixed=TRUE)
# Replace first letter t
	signalNames[n] <- paste(gsub("t", "TimeSignal", substr(signalNames[n],1,1)), 
			substr(signalNames[n], 2, nchar(signalNames[n])), sep="")
# Replace first letter f
	signalNames[n] <- paste0(gsub("f", "FrequencySignal", substr(signalNames[n],1,1)), 
			substr(signalNames[n], 2, nchar(signalNames[n])), sep="")
	n <- n + 1
}

# Update the dataset in step 3 with new signal or variable names
data_step4 <- data_step3
names(data_step4) <- c("Subject", "Activity", signalNames)

# Group the dataset in step 4 by subject and activity and summarize the variables by mean
data_step5 <- data_step4 %>%
	group_by(Subject, Activity) %>%
	summarise_each(funs(mean))
