# run_analysis
# Merges the training and the test sets to create one data set. Extracts only
# the measurements on the mean and standard deviation for each measurement. Uses
# descriptive activity names to name the activities in the data set 
# Appropriately labels the data set with descriptive variable names. From the
# data set in step 4, creates a second, independent tidy data set with the
# average of each variable for each activity and each subject

# raw data is downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 
# Working directory is the folder above "UCI HAR DataSet" obtained by
# unzipping the data
topdir <- 'UCI HAR Dataset'

#
# FEATURES
# The features.txt data file has the labels for the data. Load the features into
# a data frame
features <- read.table( paste( topdir, 'features.txt', sep='/'))
# Perform some pattern substitution on the features to create readable names
#    tBodyAcc-mean()-X becomes tBodyAcc.mean.X
# The array fnames will be used as column names when loading the measurements
# data set
fnames <- gsub( '\\-', '.', sub( '\\(\\)', '', features$V2))

# ACTIVITIES
# Read the activity labels
# The first column is activity ID, the second column is activity label
# This will be used to replace activity ID by activity label in forming the
# tidy data set
activity <- read.table( paste( topdir, 'activity_labels.txt', sep='/'), 
                        col.names = c("actId", "actLabel") )

# DATA
# The data come in three files: 
#   subject_test.txt: the subject (integer from 1 to 30)
#         y_test.txt: the activity (integer from 1 to 6)
#         x_test.txt: the measurements for the test set
#
# Load the subject data for the test set. Give it name "subject"
test_subj <- read.table( paste( topdir, 'test', 'subject_test.txt', sep='/'),
                         col.names="subject")
# Load the activity data for the test set.  Give it name "actId"
test_act <- read.table( paste( topdir, 'test', 'y_test.txt', sep='/'), 
                        col.names="actId")
# Load the test set used fnames as column names
test_data <- read.table( paste( topdir, 'test', 'X_test.txt', sep='/'),
                         col.names = fnames)

# Reduce the data set to those quantities that are representative of mean or
# standard deviation of a quantity. Start by identifying the desired data names,
# then proceed only with those.
# Search for pattern ".mean." or pattern ".std." in feature name (col 2)
# for the time measurements (prefix t)
# Return the row indices in variable k_ms
k_ms <- grep( '(t.*\\.mean\\.)|(t.*\\.std\\.)', fnames, value=FALSE)

# Create activity label data from activity id and corresponding label
test_actLabels <- activity$actLabel[ test_act$actId]

# Create a data frame using only desired columns. Bind with the
# subject and activity label 
test_df <- cbind(test_subj, test_actLabels, test_data[,k_ms])
# rename the activity
names(test_df)[names(test_df)=="test_actLabels"] <- "activity"

# Delete the big data set
rm( "test_data", "test_act", "test_subj")

# Prepare a data frame for combining the train and test data sets where
# the meausrements are averaged by subject abd by activity

nb_rows = 30*6 # 30 subjects and 6 activities
nb_meas = length(k_ms) # the number of measurements

df <- data.frame( 
        subject = rep(0, nb_rows),
        activity = factor( vector( mode="character", length=nb_rows), levels=activity$actLabel ), 
        status = factor( vector( mode="character", length=nb_rows),levels=c("test", "train"))
                )
# append data columns palce holder
df <- cbind( df, matrix( 0, nrow=nb_rows, ncol=nb_meas) )
# copy over the names to the data columns
names(df)[4:(nb_meas+3)]<-fnames[k_ms]

# loop over subjects and activity and calculate mean data
for (s in unique(test_df$subject) ) {
        for (a in 1:length(activity$actLabel) ) {
                # find matching subject and activity
                ind <- which(  test_df$subject==s
                      & test_df$activity==activity$actLabel[a])
                # calculate data mean and store in data frame
#                mat[6*(s-1)+a, ] = colMeans( test_df[ind , 3:50])
                df[6*(s-1)+a, 4:(nb_meas+3)] <-
                                colMeans( test_df[ind , 3:(nb_meas+2)])
                # record whether test or train subject
                #status[6*(s-1)+a] = "test"
                df$status[6*(s-1)+a] = "test"
                df$activity[6*(s-1)+a] = activity$actLabel[a]
                df$subject[6*(s-1)+a] = s
        }
}

# reapet for the train set
# DATA
# The data come in three files: 
#   subject_train.txt: the subject (integer from 1 to 30)
#         y_train.txt: the activity (integer from 1 to 6)
#         x_train.txt: the measurements for the train set
#
# Load the subject data for the train set. Give it name "subject"
train_subj <- read.table( paste( topdir, 'train', 'subject_train.txt', sep='/'),
                         col.names="subject")
# Load the activity data for the train set.  Give it name "actId"
train_act <- read.table( paste( topdir, 'train', 'y_train.txt', sep='/'), 
                        col.names="actId")
# Load the train set used fnames as column names
train_data <- read.table( paste( topdir, 'train', 'X_train.txt', sep='/'),
                         col.names = fnames)

# Create activity label data from activity id and corresponding label
train_actLabels <- activity$actLabel[ train_act$actId]

# Create a data frame using only desired columns. Bind with the
# subject and activity label 
train_df <- cbind(train_subj, train_actLabels, train_data[,k_ms])
# rename the activity
names(train_df)[names(train_df)=="train_actLabels"] <- "activity"

# Delete the big data set
rm( "train_data", "train_act", "train_subj")

# loop over subjects and activity and calculate mean data
for (s in unique(train_df$subject) ) {
        for (a in 1:length(activity$actLabel) ) {
                # find matching subject and activity
                ind <- which(  train_df$subject==s
                               & train_df$activity==activity$actLabel[a])
                # calculate data mean and store in data frame
                 df[6*(s-1)+a, 4:(nb_meas+3)] <-
                        colMeans( train_df[ind , 3:(nb_meas+2)])
                # record whether test or train subject
                #status[6*(s-1)+a] = "test"
                df$status[6*(s-1)+a] = "train"
                df$activity[6*(s-1)+a] = activity$actLabel[a]
                df$subject[6*(s-1)+a] = s
        }
}
