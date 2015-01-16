rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  library(plyr)  
  
  validOutcome = c("heart attack","heart failure","pneumonia");
  if (!outcome %in% validOutcome) { stop("invalid outcome"); }
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available");

  fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia");
  colName <- fullColName[match(outcome,validOutcome)];
  
  states <- sort(unique(data$State));
  
  df <- data.frame(hospital = character(length(states)), state = character(length(states)), stringsAsFactors = FALSE);
  
  for(i in 1:length(states)){
    temp <- data[data$State == states[i] & !is.na(data[colName]),];
    temp <- arrange(temp, as.double(temp[,colName]), (temp$Hospital.Name));

    df$state[i] <- states[i];
    
    if(num == "best") {
      df$hospital[i] <- temp[1,]$Hospital.Name;
    } else if (num == "worst") {
      df$hospital[i] <- temp[nrow(temp),]$Hospital.Name;
    } else {
      df$hospital[i] <- temp[num,]$Hospital.Name;
    }
  }
  
  row.names(df) <- states;
  
  df;
}