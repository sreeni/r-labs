days <- c(4,5,6,7)
probs <- c(0.1,0.2, 0.3, 0.4)
currentDay <- 4
waitUntil <- 6

simulateLibraryReturn <- function(nTrials){
    bookReturns <- sample(days, size = nTrials,replace = TRUE, prob = probs )
    foreignLibIndicators  <- sample(c(0,1), size = nTrials, replace = TRUE)
    foreignLibraryDelay = 2
    b <- bookReturns + foreignLibraryDelay * foreignLibIndicators

    notebook <- cbind(bookReturns, foreignLibIndicators, b)
    
    delayedBookReturns <- notebook[b > currentDay,]
    result1 <- mean(delayedBookReturns[,3] == waitUntil)
    cat("result1", result1, fill = TRUE)
    
    bgrt4 <- b[b > currentDay] 
    result2 <- mean(bgrt4 == waitUntil)
    cat("result2", result2, fill = TRUE)
}

simulateLibraryReturn(nTrials = 10000)