## Introduction
This repository holds the train_test.txt data set, associated code book, 
and R-script used for generating it.

## The Data
The raw data set operated upon is from the "Getting and Cleaning Data" 
Coursera class website. This data were collected by processing
measurements from the accelerometers on the Samsung Galaxy S smartphone for 30
subjects performing 6 activities. The data are presented in multiple ascii
files for "train" and "test" subjects. See Reference [1].

The run_analysis.R script merges the "train" set and the "test" set for a
selection of 40 variables representing the time-domain data associated with
mean and standard deviation quantities from the original data set. Furthermore
the extracted data is averaged by subject and activity, leading to a final
data set of 30*6=180 observations and 40 measurements. The final data set also
includes information about the subject, the activity, and whether it came from
the train or the test set in the raw data. Naming conventions and data details
are given in the code book.

## The Processing
The raw data was downloaded (April 11 2015) from:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

The data is unzipped into folder "UCI HAR Dataset". The script (run_analysis.R)
is to be invoked from folder containing "UCI HAR Dataset"

The final data set is saved to file "train_test.txt".
The column separtor is a space, and the first line has column headers.

## Reference
[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012