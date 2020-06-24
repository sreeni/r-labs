println <-function(...){
  cat(..., fill = T)
}

generateBalls <- function(){
  bag <- c()
  for(i in 1:11){
    bag <- c(bag, array(i,i))
  }
  bag
}

isThirdPick <- function(balls){
  i <- 1
  while(i <= length(balls)){
    if(balls[i] == balls[i+1]){
      i <- i + 1
    } else if(balls[i+2] == 11) {
      #println("3rd pick at ", balls[1:5])
        return(TRUE)
    } else {
      return (FALSE)
    }
  }
  return(FALSE)
}

pickBalls <- function(){
  sample(generateBalls(), 66, replace = FALSE)  
}

nbaSimulation <- function (nTrials){
  count_x_1 <- 0
  count_x_2 <- 0
  count_x_3 <- 0
  count_x_4 <- 0
  
  for(i in 1:nTrials){
    balls <- pickBalls()
    if(balls[1] == 11){
      count_x_1 <- count_x_1 + 1
    } else if(balls[2] == 11){
      count_x_2 <- count_x_2 + 1
    } else if(isThirdPick(balls)) {
        count_x_3 <- count_x_3 + 1
    }
  }
  
  println("p_x_1", count_x_1/nTrials)
  println("p_x_2", count_x_2/nTrials)
  println("p_x_3", count_x_3/nTrials)
  p_x_4 <- (1-(count_x_1 + count_x_2 +count_x_3)/nTrials)
  println("p_x_4", p_x_4)
}

nbaSimulation(1000)