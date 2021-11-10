library(taskscheduleR)

setwd("C:/Users/tayar/Google Drive/Options")

#pull data 30 minutes after open 
taskscheduler_create(taskname = "morningDataPull", rscript = "C:/Users/tayar/Google Drive/Options/Data_Option_Pull.r", 
                     schedule = "WEEKLY", starttime = "09:00", days = c("MON","TUE","WED", "THU","FRI"))

#pull data 1 hour after close 
taskscheduler_create(taskname = "afterNoonDataPull", rscript = "C:/Users/tayar/Google Drive/Options/Data_Option_Pull.r", 
                     schedule = "WEEKLY", starttime = "17:30", days = c("MON","TUE","WED", "THU","FRI"))


