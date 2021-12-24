# Pull option data from all the stocks in the S&P 500. 
# Stores the data as multiple csv files. 

library(quantmod)
library(tidyverse)

# Import helper functions. 
getOptionData <- function(symbol = "F", exp, src = "yahoo", type)
{
    opt_Chain <- getOptionChain(Symbols = symbol, Exp = exp, src = src)
    opt_Chain_return <- list()
    for(name in names(opt_Chain)){
      opt_Chain_return[[name]] <- opt_Chain %>% 
        pluck(name) %>% 
        pluck(type) %>% 
        select(Strike, Bid, Ask, Last, Vol) %>%
        filter(!is.na(Vol) & (Last + Bid + Ask) > 0.03)
  } 
  #Data Formating
  #Adding DTE 
    opt_Chain_return <- bind_rows(opt_Chain_return, .id = "Exp")
    opt_Chain_return$Exp <- as.Date(opt_Chain_return$Exp, format = "%b.%d.%Y")
    opt_Chain_return$Strike <- ordered(as.factor(opt_Chain_return$Strike))
    opt_Chain_return$DTE <- (opt_Chain_return$Exp - Sys.Date())
    return(opt_Chain_return)
}

## TEST ##

# Declare a file path were the data should be stored into 
setwd("C:/Users/Admin/Google Drive/3Theta/Historical Option Data/Calls")

# Use the helper function to pull the call data. 
x <- getOptionData(symbol = "AMZN", exp = c("2021", "2022"), src = "yahoo", 
    type = "calls")

# Write the data to the file path declared above. 
write.csv(x, file = "test_AMNZ.csv")
