# Function for repeated use 

# Import dependency 
library(quantmod)
library(tidyverse)

# Returns reduced and formated option chain data
# @param symbol - the equity ticker that you want data on 
# @param exp - the upper bound for the expirations you want data on 
# @param src - the source you want to pull the data from 
# @param type - calls or puts 
# @param upperStrike 
# @param lowerStrike 
# @returns: a formatted data frame of option chain data 
getOptionData = function (symbol = "F", exp, src = "yahoo", type, 
  upperStrike = 5000, lowerStrike = 0){
    opt_Chain = getOptionChain(Symbols = symbol, Exp = exp, src = src)
    opt_Chain_return = list()
    for(name in names(opt_Chain)){
      opt_Chain_return[[name]] = opt_Chain %>% 
        pluck(name) %>% 
        pluck(type) %>% 
        select(Strike, Bid, Ask, Last, Vol)
  }
  #Data Formating
  #Adding DTE 
    opt_Chain_return = bind_rows(opt_Chain_return,.id = "Exp")
    opt_Chain_return$Exp = as.Date(opt_Chain_return$Exp, format = "%b.%d.%Y")
    opt_Chain_return$Strike = ordered(as.factor(opt_Chain_return$Strike))
    opt_Chain_return$DTE = (opt_Chain_return$Exp - Sys.Date())
    return(opt_Chain_return)
} 

#Black-Scholes Option Pricing Model 
# @ param S - the spot price of the underlying stock 
# @ param K - the strike price of the option 
# @ param TTM - time to maturity of the option (DTE/365)
# @ param sigma - historical volatility 
# @ return - data frame with option value and the greeks 
BS_OPM = function (S, K, TTM, rf, sigma, type) {
  d1 = (log(S / K) + (rf + 0.5 * sigma^2) * TTM) / (sigma*sqrt(TTM))
  d2 = d1 - sigma * sqrt(TTM)
  
  if(type == "calls"){
    opt_val = S * pnorm(d1) - K * exp(-rf * TTM) * pnorm(d2)
    
    #the greeks 
    delta = pnorm(d1, mean = 0, sd = 1)
    gamma = dnorm(d1,mean = 0, sd = 1) / (S * sigma * sqrt(TTM))
    vega = S * dnorm(d1 ,mean = 0, sd = 1) * sqrt(TTM)
    theta = - ((S * sigma * dnorm(d1, mean = 0, sd = 1))) / 
      (2*sqrt(TTM)) - (rf * K * exp(-rf * TTM)) * 
      pnorm(d2, mean = 0, sd = 1)
    rho = K * TTM * exp(-rf * TTM) * 
      pnorm(d2, mean = 0, sd = 1)
    
  }
  
  if(type == "puts"){
    opt_val = K * exp(-rf * TTM) * pnorm(-d2) -S * pnorm(-d1) 
    
    #the greeks 
    delta = pnorm(d1, mean = 0, sd = 1) - 1
    gamma = dnorm(d1,mean = 0, sd = 1) / (S * sigma * sqrt(TTM))
    vega = S * dnorm(d1 ,mean = 0, sd = 1) * sqrt(TTM)
    theta = - ((S * sigma * dnorm(d1, mean = 0, sd = 1))) / 
      (2*sqrt(TTM)) + (rf * K * exp(-rf * TTM)) * 
      pnorm(-d2, mean = 0, sd = 1)
    rho = -K * TTM * exp(-rf * TTM) * 
      pnorm(-d2, mean = 0, sd = 1)
  }
  
  return(cbind(opt_val, delta, gamma, vega, theta,rho))
}


#Uses the Bisection Method to solve for the implied volatility 
#based on an obeserved market price of an option and the 
#Black-scholes model 
#@param S: the price of the stock 
#@param K: the strike price of the option 
#@param TTM:time of maturity (DTE / 365)
#@param rf:the risk-free rate 
#@param hist_volat: the historical volatility of the stock 
#@param opt_val: the market price of the option 
#@param type: call or put 
imp_Volat = function(S, K, TTM, rf, hist_volat, opt_val, type){
  sigma = hist_volat
  sigma_up = 1 
  sigma_down =  0.001
  count = 0 
  epsilon = BS_OPM(S, K, TTM, rf, sigma, type) - opt_val
  while(abs(epsilon) > 0.00001 && count < 1000){
    if(epsilon < 0){
      sigma_down = -sigma 
      sigma = (sigma_up + sigma) / 2 
    }else{
      sigma_up = sigma
      sigma = (sigma_down + sigma) / 2
    }
    epsilon = BS_OPM(S,K,TTT,rf,sigma,type) - opt_val
    count = count + 1 
  }
  if(count == 1000){
    return(NA)
  }else{
    return(sigma)
  }
}

#Creates a list of xts objects of given securities to represent a portfolio
#@param: symbols - a vector of strings containing ticker symbols 
#@param: start - the start date of the portfolio 
#@param: end - the end date of the portfolio 
creatPort = function(symbols, start, end){
  port_List = list()
  for(stocks in symbols){
    port_List[[stocks]] = getSymbols(Symbols = stocks, from = start, to = end,
                                     src = "yahoo", auto.assign = F)
  }
  return(port_List)
}

#Returns a vector of share quantities as close as possible to given target weights 
#constrained by a certain budget. 
share = function(prices, targets, budget)
{
  est_Shares = round((targets * budget) / prices)
  
  total_Cost = sum(prices * est_Shares)
  
  est_weights = est_Shares * prices / total_Cost
  
  error = (est_weights / targets) - 1 
  
  print("Error:")
  print(error)
  print("Weights:")
  print(est_weights)
  
  return(est_Shares)
}

#Returns the weights of securities in a portfolio 
Weights = function(shares, price)
{
  weights = shares * price / sum(shares * price) 
  return(weights)
}



