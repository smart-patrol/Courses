# Introduction

The script `run_analysis.R`performs the 5 steps described in the course project's definition.

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names. 
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

* First, it downloads the data and extracts it into a single folder and then reads in the file. Working directory must be correctly set for this to work.
* Next, it merges training and test sets.
* Then it pulls the descriptive activity names and approiately labels the features.
* This is then followed by extracting only the measurments for the mean and standard deviation
* Finally, an indpendent daty set is created according to tidy data principles with the average of each subject for each activity.

# Variables

* `x_tr`, `y_tr`, `x_tst`, `y_tst`, `sb_tr` and `sb_tst` contain the data from the downloaded files.
* `dat` merges these into one single data set.
* `feat` contains the correct names for the descriptive variables and measurments.
* `actv` houses the categorical activity by subject.
* `dat2` take `dat`, `feat` and `actv` combining them in to a single data.frame with only meand and std selected.
* Finally, `d_agg` contains the tidied data of these with lables proprely cleaned and cast in long form.
