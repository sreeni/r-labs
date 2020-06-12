getHat <- function (nHats){
  return(sample(1:nHats, 1))
}

simulateNotGettingHatback <- function(nHats, nTrials){
  nPeople <- nHats
  favEventCount <- 0
  for(i in 1:nTrials){
    noOneGotHatBack <- TRUE
    for(j in 1:nPeople){
      hat <- getHat(nHats)
      if(hat == j){
        noOneGotHatBack <- FALSE
      }
    }
    if(noOneGotHatBack){
      favEventCount <- favEventCount + 1
    }
  }
  probability = favEventCount/nTrials
  cat("probability that no one got hat back", probability, "\n")
}

simulateNotGettingHatback(nHats = 100, nTrials = 10000)