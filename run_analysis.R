run_analysis <- function() {
## 
	library(plyr)
	activity_labels <- read.table("~/activity_labels.txt", quote="\"", comment.char="")
	features <- read.table("~/features.txt", quote="\"", comment.char="")
	subject_test <- read.table("~/test/subject_test.txt", quote="\"", comment.char="")
	x_test <- read.table("~/test/X_test.txt", quote="\"", comment.char="")
	y_test <- read.table("~/test/y_test.txt", quote="\"", comment.char="")
	x_train <- read.table("~/train/X_train.txt", quote="\"", comment.char="")
	y_train <- read.table("~/train/y_train.txt", quote="\"", comment.char="")
	subject_train <- read.table("~/train/subject_train.txt", quote="\"", comment.char="")
	names(y_test) <- "Activity"
	names(y_train) <- "Activity"
	names(subject_test) <- "Subject"
	names(subject_train) <- "Subject"
	names(x_test) <- features$V2
	names(x_train) <- features$V2
	test_frame <- cbind(subject_test,y_test,x_test)
	train_frame <- cbind(subject_train,y_train,x_train)
	combined_set <- rbind(test_frame,train_frame)
	means_stds <- combined_set[, grep("mean|std", names(combined_set))]
	relevant_set <- cbind(combined_set$Subject,combined_set$Activity,means_stds)
	means_set <- aggregate(relevant_set[,3:81],list(relevant_set$`combined_set$Subject`,relevant_set$`combined_set$Activity`),mean)
	means_set<-rename(means_set,c("Group.1"="Subject"))
	means_set<-rename(means_set,c("Group.2"="Activity"))
	means_set$Activity<-mapvalues(means_set$Activity,c(1,2,3,4,5,6),c("walking","walking upstairs","walking downstairs","sitting","standing","laying"))
	names(means_set) <- gsub("-","",names(means_set))
	names(means_set) <- gsub("mean\\(\\)","mean",names(means_set))
	names(means_set) <- gsub("std\\(\\)","standarddeviation",names(means_set))
	names(means_set) <- gsub("^t","time",names(means_set))
	names(means_set) <- gsub("^f","frequency",names(means_set))
 	names(means_set) <- gsub("BodyBody","body",names(means_set))
	names(means_set) <- tolower(names(means_set))
	means_set
}