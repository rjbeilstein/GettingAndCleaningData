#  Copyright (C) Robert J. Beilstein, 2015
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#  Getting and Cleaning Data Course Project.  Read Samsung accelerometer data and clean,
#  tidy and summarise data to get average mean and SD values for various accelerometer readings
#
# Author: rjb
###############################################################################

library(data.table)
library(dplyr)
library(tidyr)

#
# Fetch the zipped data
#

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip","acdata.zip","curl",extra="--anyauth")

#
# Get the column labels for all of the "features"
#

colLabels<-tbl_df(read.table(unz("acdata.zip","UCI HAR Dataset/features.txt"),stringsAsFactors=FALSE,col.names=c("Colno","Label")))

#
# Activity Labels -- activity names and codes
#

actLabels<-tbl_df(read.table(unz("acdata.zip","UCI HAR Dataset/activity_labels.txt"),stringsAsFactors=FALSE,col.names=c("ActivityNumber","ActivityName"))) %>% select(matches("ActivityName"))

#
#  Now get training data.  Note we ONLY care about mean and std deviation values
#

#
#  First, the subject associated with each observation

trainSubj<-tbl_df(read.table(unz("acdata.zip","UCI HAR Dataset/train/subject_train.txt"),stringsAsFactors=FALSE,col.names="Subject"))

#
# Then the activities for each observation
#

trainAct<-tbl_df(read.table(unz("acdata.zip","UCI HAR Dataset/train/y_train.txt"),stringsAsFactors=FALSE,col.names="Activity"))

trainActivity<-actLabels[trainAct$Activity,]

#
#  OK.  Now we have everything we need for the training data.  We will combine the Subject and Activity
# information with the columns from the function data representing the means and standard deviations
#

trainData<-bind_cols(trainSubj,trainActivity) %>%
bind_cols(select(tbl_df(read.table(unz("acdata.zip","UCI HAR Dataset/train/X_train.txt"),stringsAsFactors=FALSE,col.names=colLabels$Label)),contains(".mean."),contains(".std."))) %>%
 tbl_df()


#
#  Similarly get test data.  Again, we ONLY care about mean and std deviation values
#

#
#  First, the subject associated with each observation

testSubj<-tbl_df(read.table(unz("acdata.zip","UCI HAR Dataset/test/subject_test.txt"),stringsAsFactors=FALSE,col.names="Subject"))

#
# Then the activities for each observation
#

testAct<-tbl_df(read.table(unz("acdata.zip","UCI HAR Dataset/test/y_test.txt"),stringsAsFactors=FALSE,col.names="Activity"))

testActivity<-actLabels[testAct$Activity,]

#
#  OK.  Now we have everything we need for the test data.  We will combine the Subject and Activity
# information with the columns from the function data representing the means and standard deviations
#

testData<-bind_cols(testSubj,testActivity) %>%
        bind_cols(select(tbl_df(read.table(unz("acdata.zip","UCI HAR Dataset/test/X_test.txt"),stringsAsFactors=FALSE,col.names=colLabels$Label)),contains(".mean."),contains(".std."))) %>%
        tbl_df()

#
#  OK.  Now we have the data we want from both the training and test observations.  We now need to combine it
#  into a single data set, Sort it in order by subject and activity, and then group it by the same columns
#

accelData<-bind_rows(testData,trainData) %>% arrange(Subject,ActivityName) %>% group_by(Subject,ActivityName)


#
# Finally, we will calculate the mean (average) value of each of the data columns using the grouping
# defined above to calculate the required result
#

courseData<-courseData<-summarise_each(accelData,funs(mean))

#
#  Now all that remains is to write this to an external file using write.table
#

write.table(courseData,"CourseData.txt",row.names=FALSE)
