rankhospital <- function(state, outcome, num = "best") {

    best.df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
   
    if (!state %in% best.df$State) {
      stop("invalid state")
    }
    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
      stop("invalid outcome")
    } 

    sub.state <- subset(best.df, best.df[, 7] == state, select=c(2, 7, 11, 17, 23))
  
    if (outcome == "heart attack") {
        sub.state[,3] <- as.numeric(sub.state[, 3])
        sub.state <- subset(sub.state, !is.na(sub.state[,3]))
        sub.order <- sub.state[order(sub.state[,3], sub.state[,1]), ]
        y <- c(1:nrow(sub.order))
        sub.rank <- cbind(sub.order, y)
      
        if (num == "best") {
            x<- subset(sub.rank, sub.rank[, 6] ==1)
            return(x[,1])
        }
        else if (num == "worst") {
            x<- subset(sub.rank, sub.rank[, 6] == max(sub.rank[,6]))
            return(x[,1])
        }
        else if (num > max(sub.rank[,6])) {
            return(NA)
        }
        else {
            x<- subset(sub.rank, sub.rank[, 6] == num)
            return(x[,1])
        }
    }
    if (outcome == "heart failure") {
        sub.state[,4] <- as.numeric(sub.state[, 4])
        sub.state <- subset(sub.state, !is.na(sub.state[,4]))
        sub.order <- sub.state[order(sub.state[,4], sub.state[,1]), ]
        y <- c(1:nrow(sub.order))
        sub.rank <- cbind(sub.order, y)
    
        if (num == "best") {
           x<- subset(sub.rank, sub.rank[, 6] ==1)
           return(x[,1])
        }
        else if (num == "worst") {
            x<- subset(sub.rank, sub.rank[, 6] == max(sub.rank[,6]))
            return(x[,1])
        }
        else if (num > max(sub.rank[,6])) {
            return(NA)
        }
        else {
            x<- subset(sub.rank, sub.rank[, 6] == num)
            return(x[,1])
        }    
    } 
    if (outcome == "pneumonia") {
        sub.state[,5] <- as.numeric(sub.state[, 5])
        sub.state <- subset(sub.state, !is.na(sub.state[,5]))
        sub.order <- sub.state[order(sub.state[,5], sub.state[,1]), ]
        y <- c(1:nrow(sub.order))
        sub.rank <- cbind(sub.order, y)
        if (num == "best") {
            x<- subset(sub.rank, sub.rank[, 6] ==1)
            return(x[,1])
        }
        else if (num == "worst") {
            x<- subset(sub.rank, sub.rank[, 6] == max(sub.rank[,6]))
            return(x[,1])
        }
        else if (num > max(sub.rank[,6])) {
            return(NA)
        }
        else {
            x<- subset(sub.rank, sub.rank[, 6] == num)
            return(x[,1])
        }
    }
}