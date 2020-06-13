  flipCoin <- function(nFlips, pHead){
    flippedCoins <- sample(c(1,0), nFlips, replace= TRUE,prob = c(pHead, 1-pHead))
    return (flippedCoins)  
  }
  
  areThereConsecutiveHeads <- function(kHeads, flippedCoins){
    kHeadsArray = array(1, kHeads)
    kHeadsMiddle = c(0,kHeadsArray,0)
    kHeadsStart = c('^', kHeadsArray,0)
    kHeadsEnd = c(0,kHeadsArray, '$')
    kHeadsVector = c(kHeadsStart,'|', kHeadsMiddle, '|', kHeadsEnd)
    expectedPattern = paste(kHeadsVector, collapse="")
    actual = paste(flippedCoins, collapse="")
    match <- grepl(expectedPattern, actual)
    return (match)
  }
  
  
  consecutiveHeads <- function(nTrials, nFlips, kHeads, pHead){
    count <- 0
    for(i in 1:nTrials){
      headsConsecutive <- areThereConsecutiveHeads(kHeads, flipCoin(nFlips, pHead))
      if(headsConsecutive){
        count <- count + 1
      }
    }
    cat(fill = TRUE,"probability getting", kHeads, "consecutive heads with pHead(", pHead, ") in ", nFlips,"flips =", count/nTrials)
  }
  
  consecutiveHeads(nTrials = 10000, nFlips = 10, kHeads = 5, pHead = 0.5)
