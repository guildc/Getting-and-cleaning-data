Title: Getting and Cleaning Data Project README.mdAuthor: Camelia GuildDate: 4/01/2016Input data: Human Activity Recognition Using Smartphones Dataset Version 1.0 (see original data source link below)Output data: tidyData.txtR script: run_Analysis.R================================================================The R script, run_Analysis.R, does the following:1.	Downloads and unzips the data files and places them in the working directory.2.	Loads the following files: activity_lables.txt, features.txt, subject_train.txt, x_train.txt, y_train.txt, subject_test.txt, x_test.txt, and y_test.txt3.	Creates a training dataset and a test dataset and merges the two.4.	Creates a tidy dataset that contains the mean values and standard deviations on each variable for each activity and on each subject.Description of the data fileThe tidyData set is derived from experimental data carried out with a group of 30 volunteers within an age bracket of 19-48 years.  Each person performed six activities (Walking, Walking_Upstairs, Walking_Downstairs, Sitting, Standing, Laying) wearing a smartphone (Samsung Galaxy S II) on the waist. Mean values and standard deviations were calculated on each variable for each activity and each subject. Thus, the tidyData file contains a total of 180 observations on each of 69 variables (or 6 activity values per each subject per each measurement variable; 30*6 =180). For each record the following variables are provided:================================================================1.	SubjectID: An identifier of the subject who carried out the experiment.2.	ActivityID: an identifier of the six activities each person performed.3.	activityType: activity labels.Means and standard deviations were calculated on each of the following variables:4.	timeBodyAccelerometer-XYZ5.	timeGravityAccelerometer-XYZ6.	timeBodyAccelerometerJerk-XYZ7.	timeBodyGyroscope-XYZ8.	timeBodyGyroscopeJerk-XYZ9.	timeBodyAccelerometerMagnitude10.	timeGravityAccelerometerMagnitude11.	timeBodyAccelerometerJerkMagnitude12.	timeBodyGyroscopeMagnitude13.	timeBodyGyroscopeJerkMagnitude14.	freqBodyAccelerometer-XYZ15.	freqBodyAccelerometerJerk-XYZ16.	freqBodyGyroscope-XYZ17.	freqBodyAccelerometerMagnitude18.	freqBodyAccelerometerJerkMagnitude19.	freqBodyGyroscopeMagnitude20.	freqBodyGyroscopeJerkMagnitudeThe dataset includes the following files:================================1.	README.md2.	CodeBook.md: Describes data collection, extraction, cleaning, and transformations on variables contained in the tidyData file.3.	run_Analysis.R: Script on the derivation of the final dataset, tidyData.txt.4.	tidyData.txt: This is the analysis dataset.Original data source:===============================A full description of the data used in this project can be found at The UCI Machine Learning Repository: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+SmartphonesData: Human Activity Recognition Using Smartphones Dataset Version 1.0https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zipLicense:========Use of this dataset in publications must be acknowledged by referencing the following publication [1] [1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.