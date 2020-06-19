days <- c(4,5,6,7)
probs <- c(0.1,0.2, 0.3, 0.4)
currentDay <- 4
waitUntil <- 6

simulateLibraryReturn <- function(nTrials){
    bookReturns <- sample(days, size = nTrials,replace = TRUE, prob = probs )
    foreignLibIndicators  <- sample(c(0,1), size = nTrials, replace = TRUE)
    bookReturnsWithDelays <- bookReturns + 2*foreignLibIndicators

    notebook <-cbind(bookReturnsWithDelays, foreignLibIndicators, bookReturns)
    
    delayedBookReturns <- notebook[bookReturnsWithDelays > currentDay,]
    #delayedBookReturns <- bookReturnsWithDelays[bookReturnsWithDelays > currentDay] 
    result <- mean(delayedBookReturns[,3] == waitUntil)
    #
    #result <- mean(delayedBookReturns == waitUntil)
    
    cat("result", result, fill = TRUE)
}

simulateLibraryReturn(nTrials = 1000)