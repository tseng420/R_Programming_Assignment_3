best <- function(state, outcome) {
  ## Read outcome data
  best.df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if (!state %in% best.df$State) {
    stop("invalid state")
  }
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("invalid outcome")
  } 
  ## Return hospital name in that state with lowest 30-day death rate
  sub.best <- subset(best.df, best.df[, 7] == state, select=c(2, 7, 11, 17, 23))
  if (outcome == "heart attack") {
    sub.best[,3] <- as.numeric(sub.best[, 3])
    min <- min(sub.best[,3], na.rm = TRUE)
    sub.state <- subset(sub.best, sub.best[,3]==min)
    ## Handling ties
    sorted_data <- sort(sub.state[,1])
  }
  if (outcome == "heart failure") {
    sub.best[,4] <- as.numeric(sub.best[, 4])
    min <- min(sub.best[,4], na.rm = TRUE)
    sub.state <- subset(sub.best, sub.best[,4]==min)
    ## Handling ties
    sorted_data <- sort(sub.state[,1])
  }
  if (outcome == "pneumonia") {
    sub.best[,5] <- as.numeric(sub.best[, 5])
    min <- min(sub.best[,5], na.rm = TRUE)
    sub.state <- subset(sub.best, sub.best[,5]==min)
    ## Handling ties
    sorted_data <- sort(sub.state[,1])
  }
  sorted_data[1]
}