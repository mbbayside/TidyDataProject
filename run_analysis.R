# run_analysis
# Merges the training and the test sets to create one data set. Extracts only
# the measurements on the mean and standard deviation for each measurement. Uses
# descriptive activity names to name the activities in the data set 
# Appropriately labels the data set with descriptive variable names. From the
# data set in step 4, creates a second, independent tidy data set with the
# average of each variable for each activity and each subject

# The raw data was downloaded (April 11 2015) from:
#
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# 
# The data is unzipped into folder "UCI HAR Dataset". This script (run_analysis.R)
# is to be invoked in parent folder of "UCI HAR Dataset"
# 
# ------------------------------
# DEFINE PATH TO RAW DATA FOLDER
# ------------------------------
# The variable topdir is the relative path from working directory (where script
# resides) to the top level folder containing the data files after unzipping.
# Details are given in README.txt file provided in the zipped data.
# The following definition assumes that run_analysis.R is in the same folder
# as "UCI HAR Dataset"
topdir <- 'UCI HAR Dataset'

# -----------------
# LOAD THE FEATURES
# -----------------
# For each data set (test or train), the number of columns and their order is
# defined in the features.txt file. The feature labels are read into a data frame.
features <- read.table( paste( topdir, 'features.txt', sep='/'))

# -----------------------------
# DEFINE THE SUBSET OF FEATURES
# -----------------------------
# Perform some pattern substitution on the features to create readable names
#    tBodyAcc-mean()-X becomes tBodyAcc.mean.X
# The array fnames will be used as column names when loading the measurements
# data set.
fnames <- gsub( '\\-', '.', sub( '\\(\\)', '', features$V2))

# Find the indices of time-based measurements that are representative of the 
# the mean or the standard deviation of a quantity. Start by identifying the 
# desired data names (see features_info.txt in downloaded raw data), then find
# the corresponding indices.

# Search for pattern ".mean." or pattern ".std." in features with prefix t.
# Return the row indices in variable ind_tms
ind_tms <- grep( '(t.*\\.mean\\.)|(t.*\\.std\\.)', fnames, value=FALSE)

# --------------------
# LOAD THE ACTIVITIES
# --------------------
# Read the activity labels provided in the activity_labels.txt file.
# The first column is activity ID, the second column is activity label
# This will be used to replace activity ID by activity label in forming the
# tidy data set
activity <- read.table( paste( topdir, 'activity_labels.txt', sep='/'), 
                        col.names = c("actId", "actLabel") )

# ---------------------------------
# PREPARE DATA FRAME FOR FINAL DATA
# ---------------------------------
# Prepare a data frame for combining the train and test data sets where
# the meausrements are averaged by subject and by activity. 
# The README.txt provided with the raw data says that there are a total of 30 
# subjects identified by numerals 1 to 30. Use that fact to size the final data.
#
# The number of observations in the final set is 30 times the number of activities.
# The final data set will be nominally arranged by subject, then activity. 
nb_act <- nrow(activity)  # the number of activities
nb_obs <- 30*nb_act
#
# The number of measurements is the number of features that match the pattern
# criteria shown above: length(ind_tms)
nb_meas <- length(ind_tms) # the number of measurements
#
# Initialize a data frame with columns for subject, activity, status (test or 
# train)
df <- data.frame( 
        subject = rep(0, nb_obs),
        activity = factor( vector( mode="character", length=nb_obs), 
                           levels=activity$actLabel ), 
        status = factor( vector( mode="character", length=nb_obs),
                           levels=c("test", "train"))  )
#
# Append data columns place holder to be filled in as data is processed
df <- cbind( df, matrix( 0, nrow=nb_obs, ncol=nb_meas) )
#
# Copy over the names extracted from the features to the data columns. Start
# at column 4 to skip columns for subject, activity, and status. There are nb_meas 
# measurements, so stop at column 3+nb_meas.
names(df)[4:(nb_meas+3)] <- fnames[ind_tms]

# ----------------------
# LOAD THE TEST DATA SET
# ----------------------
# The test data come in three files in sub-folder "test": 
#   subject_test.txt: the subject (integer from 1 to 30)
#         y_test.txt: the activity (integer from 1 to 6)
#         x_test.txt: the measurements for the test set (defined by features)
#
# Load the subject data for the test set. Give it name "subject"
test_subj <- read.table( paste( topdir, 'test', 'subject_test.txt', sep='/'),
                         col.names="subject")
#
# Load the activity data for the test set.  Give it name "actId"
test_act <- read.table( paste( topdir, 'test', 'y_test.txt', sep='/'), 
                        col.names="actId")
#
# Load the test set using fnames as column names
test_data <- read.table( paste( topdir, 'test', 'X_test.txt', sep='/'),
                         col.names = fnames)
# 
# Assemble the test data to aid in sorting by subject and activity.
#
# Create activity label data from activity id and corresponding label
test_actLabels <- activity$actLabel[ test_act$actId]

# Create a data frame using only columns selected by ind_tms. Bind with the
# subject and activity label 
test_df <- cbind(test_subj, test_actLabels, test_data[,ind_tms])
# Rename the activity
names(test_df)[names(test_df)=="test_actLabels"] <- "activity"

# Delete the big data set to clear memeory
rm( "test_data", "test_act", "test_subj")

#
# -----------------------------------
# ASSIGN TEST SET DATA TO FINAL DATA
# -----------------------------------
# loop over subjects and activity and calculate mean data
for (s in unique(test_df$subject) ) {
        #
        for (a in 1:nb_act) {
                #
                # find matching subject and activity
                ind <- which(  test_df$subject==s
                      & test_df$activity==activity$actLabel[a])
                
                # calculate data mean and store in data frame
                df[nb_act*(s-1)+a, 4:(nb_meas+3)] <-
                                colMeans( test_df[ind , 3:(nb_meas+2)])
                
                # record whether test or train subject
                df$status[nb_act*(s-1)+a] = "test"
                # record activity label
                df$activity[nb_act*(s-1)+a] = activity$actLabel[a]
                # record subject ID
                df$subject[nb_act*(s-1)+a] = s
        }
}


# ----------------------
# LOAD THE TRAIN DATA SET
# ----------------------
# The train data come in three files in sub-folder "train": 
#   subject_train.txt: the subject (integer from 1 to 30)
#         y_train.txt: the activity (integer from 1 to 6)
#         x_train.txt: the measurements for the train set (defined by features)
#
# Load the subject data for the train set. Give it name "subject"
train_subj <- read.table( paste( topdir, 'train', 'subject_train.txt', sep='/'),
                         col.names="subject")
#
# Load the activity data for the train set.  Give it name "actId"
train_act <- read.table( paste( topdir, 'train', 'y_train.txt', sep='/'), 
                        col.names="actId")
#
# Load the train set using fnames as column names
train_data <- read.table( paste( topdir, 'train', 'X_train.txt', sep='/'),
                         col.names = fnames)
# 
# Assemble the train data to aid in sorting by subject and activity.
#
# Create activity label data from activity id and corresponding label
train_actLabels <- activity$actLabel[ train_act$actId]

# Create a data frame using only columns selected by ind_tms. Bind with the
# subject and activity label 
train_df <- cbind(train_subj, train_actLabels, train_data[,ind_tms])
# Rename the activity
names(train_df)[names(train_df)=="train_actLabels"] <- "activity"

# Delete the big data set to clear memeory
rm( "train_data", "train_act", "train_subj")

#
# -----------------------------------
# ASSIGN TRAIN SET DATA TO FINAL DATA
# -----------------------------------
# loop over subjects and activity and calculate mean data
for (s in unique(train_df$subject) ) {
        #
        for (a in 1:nb_act) {
                #
                # find matching subject and activity
                ind <- which(  train_df$subject==s
                               & train_df$activity==activity$actLabel[a])
                
                # calculate data mean and store in data frame
                df[nb_act*(s-1)+a, 4:(nb_meas+3)] <-
                        colMeans( train_df[ind , 3:(nb_meas+2)])
                
                # record whether test or train subject
                df$status[nb_act*(s-1)+a] = "train"
                # record activity label
                df$activity[nb_act*(s-1)+a] = activity$actLabel[a]
                # record subject ID
                df$subject[nb_act*(s-1)+a] = s
        }
}

#
# ----------------------------
# SAVE FINAL DATA SET TO FILE
# ----------------------------
write.table(df, file="train_test.txt", row.name=FALSE)

