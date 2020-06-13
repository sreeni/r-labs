flipCoin <- function(nFlips, pHead){
  flippedCoins <- sample(0:1, nFlips, replace= TRUE,prob = c(pHead, 1-pHead))
  return (flippedCoins)  
}

areThereConsecutiveHeads <- function(kHeads, flippedCoins){
  kHeadsWithTailTermination = c(array(0, kHeads),1)
  kHeadsEndOfString = c('.*?',array(0,kHeads))
  expected = paste(kHeadsWithTailTermination,"|",kHeadsEndOfString, collapse="")
  cat(fill=TRUE, "expected pattern", expected)
  actual = paste(flippedCoins, collapse="")
  match <- grepl(pattern=expected, x=actual, fixed=TRUE)
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

consecutiveHeads(nTrials = 1000, nFlips = 20, kHeads = 8, pHead = 0.5)
