# Option-Data-Scraping

Data_Option_Pull.r: Scrapes the current option chains for a given vector of symbols off
yahoo finance and stores them seperate .csv files. If you use it, be sure to adjust the 
file paths, the expiration range ("exp"), and tickers you would like to pull data for.  

automate_pull.r: Uses the taskscheduleR package to run Data_Option_Pull.r automatically
every weekday at 9:00 AM, and 5:30 PM. If you use it, be sure to adjust the time according 
to whatever timezone your computer is in. 

function.r: Contains helper functions for the other two scripts. 

Status: Updating. 

References: 
Ang, C. S. (2021). Analyzing financial data and implementing financial models using R. 
Springer. 
