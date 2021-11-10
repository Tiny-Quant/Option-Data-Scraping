#Pull data for historical analysis 

#set working directory 
setwd("C:/Users/tayar/Google Drive/Options")

#Source the function code
source("function.r")

#Stock List 
stockList = c("F", "GE", "WORK", "WFC", "BP", "BAC", "GM", "SNAP", "MGM", "AAL", "DAL", "LUV", "JBLU")

#get the date and time 
date_Time = Sys.time()
date_Time = strftime(date_Time, format = "%m-%d-%Y %I-%M")

#Call data 
setwd("C:/Users/tayar/Google Drive/Options/Historical Data/calls")
for(stock in stockList){
  x  = getOptionData(symbol = stock, exp = "2020", src = "yahoo", type = "calls", upperStrike = 50, lowerStrike = 0) 
  write.csv(x, file = (paste(stock,"calls", date_Time,".csv", sep = "")), append = F)  
}

#Put data
setwd("C:/Users/tayar/Google Drive/Options/Historical Data/puts")
for(stock in stockList){
  x  = getOptionData(symbol = stock, exp = "2020", src = "yahoo", type = "puts", upperStrike = 50, lowerStrike = 0) 
  write.csv(x, file = (paste(stock,"puts", date_Time ,".csv", sep = "")), append = F)  
}   