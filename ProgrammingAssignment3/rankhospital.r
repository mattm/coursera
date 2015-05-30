rankhospital <- function(state, outcome, num = "best") {
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  validStates = unique(outcomeData[,7])
  if (! state %in% validStates) {
    stop("invalid state")
  }
  
  validOutcomes = c("heart attack", "heart failure", "pneumonia")
  if (! outcome %in% validOutcomes) {
    stop("invalid outcome")
  }

  if (outcome == "pneumonia") {
    dataColName = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  } else if (outcome == "heart attack") {
    dataColName = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome == "heart failure") {
    dataColName = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  }
  
  stateData = outcomeData[outcomeData$State == state & outcomeData[dataColName] != "Not Available", c("Hospital.Name", dataColName)]
  stateData[,2] = as.numeric(stateData[,2])
  sorted = stateData[order(stateData[dataColName], stateData$Hospital.Name), ]
  
  if (is.numeric(num)) {
    if (num > nrow(sorted)) {
      NA
    } else {
      sorted[num,]$Hospital.Name
    }
  } else {
    if (num == "best") {
      sorted[1,]$Hospital.Name
    } else if (num == "worst") {
      sorted[nrow(sorted),]$Hospital.Name
    }
  }
}