
# To track execution timestart_time <- Sys.time()
start_time = Sys.time()
# Pull option data from all the stocks in the S&P 500.
# Stores the data as multiple csv files.

library(quantmod)
library(tidyverse)

# Import helper functions.
getOptionData <- function(symbol = "F", exp, src = "yahoo", type)
{
  opt_Chain <- getOptionChain(Symbols = symbol, Exp = exp, src = src)
  opt_Chain_return <- list()
  for(name in names(opt_Chain)) {
    opt_Chain_return[[name]] <- opt_Chain %>%
      pluck(name) %>%
      pluck(type) %>%
      filter(!is.na(Vol) & (Last + Bid + Ask) > 0.03) %>%
      select(-c(Chg, ITM))
  }
  #Data Formating
  opt_Chain_return <- bind_rows(opt_Chain_return, .id = "Exp")
  opt_Chain_return %>% mutate(
    Exp = as.Date(Exp, format = "%b.%d.%Y"),
    Strike = ordered(as.factor(Strike)),
    Underlying = getQuote(Symbols = symbol)$Last,
    DTE = Exp - Sys.Date(),
    )
  return(opt_Chain_return)
}

# Declare a file path were the data should be stored into
SP_500_Symbols <- read.csv(file = paste("C:/Users/Admin/Google Drive/",
  "Github/Option Data Scraping/",
  "Option-Data-Scraping/SP_500_Symbols.csv", sep = ""))
names(SP_500_Symbols) <- "Symbol"

# Calls
setwd(paste("C:/Users/Public/Data Mining/",
  "Option Data/Historical Quotes - Yahoo/SP_500/Calls",
  sep = ""))
for(symbol in SP_500_Symbols){
  x <- getOptionData(symbol = symbol, exp = c("2021","2022", "2023", "2024"), type = "calls") %>%
    mutate(Time_Pull = Sys.time())
  write.csv(x, file = paste(symbol, "-Calls", sep = ""))
}
# First run to create the csv files.

# Subsequent runs append to the intial csv files.
#y <- getOptionData(symbol = "AMZN", exp = c("2021", "2022"), type = "calls") %>%
  #mutate(Time_Pull = Sys.time())
#write.table(y, file = 'test_2.csv', append = T, sep = ",",
  #quote = F, col.names = F, row.names = T)

# To track execution time.
end_time <- sys.time()

# To keep records of execution times
df_ex_time <- cbind(end_time - start_time, toString(Sys.time()))

write.table(df_ex_time,
   file = paste("C:/Users/Public/Data Mining/Option Data/",
    "Historical Quotes - Yahoo/SP_500/Execution Times.csv", sep = ""),
   append = T, sep = ",", quote = F, col.names = F, row.names = F)
