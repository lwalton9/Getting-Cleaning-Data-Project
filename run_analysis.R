Getting and Cleaning Data Course Project
##READ IN DATA FROM PERSONAL DIRECTORY WHERE FILE WAS STORED
getwd()
setwd("G:/LORI/2016 Role/Analytics & Data Division/Miscellaneous/Coursera R/UCI HAR Dataset/")
##READ ACTIVITY 
activityType<-read.table('./activity_labels.txt')
##READ FEATURES
features<-read.table('./features.txt')
##READ TRAIN
xtrain<-read.table('./train/X_train.txt')
ytrain<-read.table('./train/Y_train.txt')
##READ TRAIN SUBJECT
trainsubject<-read.table('./train/subject_train.txt')
###ASSIGN COLUMN NAMES TO TRAIN
colnames(xtrain)<-features[,2]
colnames(ytrain)<-"activityID"
colnames(trainsubject)<-"subjectID"
##READ TEST
xtest<-read.table('./test/X_test.txt')
ytest<-read.table('./test/y_test.txt')
##READ TEST SUBJECT
testsubject<-read.table('./test/subject_test.txt')
###ASSIGN COLUMN NAMES TO TRAIN
colnames(xtest)<-features[,2]
colnames(ytest)<-"activityID"
colnames(testsubject)<-"subjectID"
###ASSIGN COLUMN NAMES To ACTIVITY
colnames(activityType)<-c("activityID","activityType")
####MERGE THE TRAINING AND THE TEST SETS TO CREATE ONE DATA SET
train<-cbind(ytrain,trainsubject,xtrain)
test<-cbind(ytest,testsubject,xtest)
traintest<-rbind(train,test)
#####CREATE COLUMN NAMES OF FINAL SET
colNames<-colnames(traintest)
View(traintest)
#####GET MEAN AND STANDARD DEVIATIONS ONLY
meanstd <- (grepl("activity..",colNames)|grepl("subject..",colNames)|grepl("-mean..",colNames) 
           & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames)
           & !grepl("-std..-",colNames))
finaltraintest=traintest[ ,meanstd==TRUE]
View(finaltraintest)
dim(finaltraintest)
######ADD ACTIVITY LABELS
finaltraintest_wact <- merge(finaltraintest, activityType, by='activityID',all.x=TRUE)
##REVIEW DATA
dim(finaltraintest_wact)
summary(trainsubject)
summary(testsubject)
######CLEAN UP NAMES############
names(finaltraintest_wact) <- make.names(names(finaltraintest_wact))
names(finaltraintest_wact) <- gsub('Acc',"Acceleration",names(finaltraintest_wact))
names(finaltraintest_wact) <- gsub('GyroJerk',"AngularAcceleration",names(finaltraintest_wact))
names(finaltraintest_wact) <- gsub('Gyro',"AngularSpeed",names(finaltraintest_wact))
names(finaltraintest_wact) <- gsub('Mag',"Magnitude",names(finaltraintest_wact))
names(finaltraintest_wact) <- gsub('^t',"TimeDomain.",names(finaltraintest_wact))
names(finaltraintest_wact) <- gsub('^f',"FrequencyDomain.",names(finaltraintest_wact))
names(finaltraintest_wact) <- gsub('\\.mean',".Mean",names(finaltraintest_wact))
names(finaltraintest_wact) <- gsub('\\.std',".StandardDeviation",names(finaltraintest_wact))
names(finaltraintest_wact) <- gsub('Freq\\.',"Frequency.",names(finaltraintest_wact))
names(finaltraintest_wact) <- gsub('Freq$',"Frequency",names(finaltraintest_wact))



#####SECOND TIDY DATASET WITh AVERAGE OF EACH VARIABLE FOR EACH ACTIVITY ANd SUBJECT

FinalTidySet <- aggregate(. ~subjectID + activityType, finaltraintest_wact, mean)
head(FinalTidySet,n=12)
FinalTidySet <- FinalTidySet[order(FinalTidySet$subjectID, FinalTidySet$activityID),]
head(FinalTidySet,n=12)
write.table(FinalTidySet, "./FinalTidySet.txt", row.names=TRUE)




