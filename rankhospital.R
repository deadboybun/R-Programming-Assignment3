rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  library(plyr)
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")
  
  validOutcome = c("heart attack","heart failure","pneumonia")
  if (!outcome %in% validOutcome) { stop("invalid outcome") }
  
  data <- data[data$State == state,]
  
  if(nrow(data) == 0) stop("invalid state")
  
  fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  colName <- fullColName[match(outcome,validOutcome)]

  data <- data[!is.na(data[,colName]),]
  
  data <- arrange(data, as.double(data[,colName]), (data$Hospital.Name))
  
  if(num == "best")
    data[1,]$Hospital.Name
  else if (num == "worst")
    data[nrow(data),]$Hospital.Name
  else 
    data[num,]$Hospital.Name
    
}