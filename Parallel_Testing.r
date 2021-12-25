# Testing the execution times of different parallel
# computing methods.

# Core Dependencies
library(quantmod)
library(dplyr)
library(purrr)

# New dependencies
library(parallel)
library(foreach)
library(doParallel)

# Intialize parallel workers.
numCores <- detectCores()

cl <- makePSOCKcluster(8)
registerDoParallel(cl)

#Parallel
start_time = Sys.time()
getOptionData <- function(symbol, exp, type)
{
    opt_Chain <- getOptionChain(Symbols = symbol, Exp = exp)
    opt_Chain_return <- foreach(name = names(opt_Chain),
        .combine = rbind,
        .packages = c('dplyr', 'purrr')) %dopar%
        {
            opt_Chain %>%
                pluck(name) %>%
                pluck("calls") %>%
                filter(!is.na(Vol) & (Last + Bid + Ask) > 0.03) %>%
                select(-c(Chg, ITM)) %>%
                mutate(Exp = as.Date(name, format = "%b.%d.%Y"),
                    DTE = difftime(Exp, Sys.Date(), unit = "days"))
        }
    #Data Formating
    opt_Chain_return <- opt_Chain_return %>% mutate(
        Exp = as.Date(Exp, format = "%b.%d.%Y"),
        Strike = ordered(as.factor(Strike)),
        Time_Pull = Sys.time(),
        Underlying = getQuote(Symbols = symbol)$Last,
        DTE = difftime(Exp, Sys.Date(), unit = "days"))
    return(opt_Chain_return)
}

SP_500_Symbols <- c("AAPL", "F", "GE", "AMZN")

# Intialize parallel workers.
cl2 <- makePSOCKcluster(64)
registerDoParallel(cl2)

foreach(symbol = SP_500_Symbols,
    .packages = c('dplyr', 'purrr', 'quantmod',
    'parallel', 'foreach', 'doParallel')) %dopar%
    {
    # Calls
    # File Path to write csv files to.
    setwd(paste("C:/Users/Public/Data Mining/",
    "Option Data/Historical Quotes - Yahoo/SP_500/Calls",
    sep = ""))
    x <- getOptionData(symbol = symbol,
        exp = c("2021", "2022", "2023", "2024"),
        type = "calls")
    write.csv(x, file = paste(symbol, "-Calls.csv", sep = ""))

    # Puts
    # File Path to write csv files to.
    setwd(paste("C:/Users/Public/Data Mining/",
    "Option Data/Historical Quotes - Yahoo/SP_500/Puts",
    sep = ""))
    y <- getOptionData(symbol = symbol,
        exp = c("2021", "2022", "2023", "2024"),
        type = "puts")
    write.csv(y, file = paste(symbol, "-Puts.csv", sep = ""))
    }

end_time <- Sys.time()

#Kill parallel workers
stopCluster(cl)
stopCluster(cl2)

#Report execution time.
print(paste("Executed in ",
    round(end_time - start_time, 2),
    " seconds"))
