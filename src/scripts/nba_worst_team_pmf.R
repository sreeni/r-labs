# NOTE: install purrr package before running this program.

println <-function(...){
  cat(..., fill = T)
}

ballsOfSameType <- function(i){
  array(i,i)
}

generateBalls <- function(){
   listOfBalls <- purrr::map(.x = seq(1,11), .f = ballsOfSameType)
   purrr::as_vector(listOfBalls)
}

pickBalls <- function(){
  sample(generateBalls(), 66, replace = FALSE)  
}

nthPick <- function(balls, n){
  indexOfWorstTeam <- which(unique(balls) == 11)
  indexOfWorstTeam == n
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
    } else if(nthPick(balls, 2)){
      #println("2nd pick", balls[1:10])
      count_x_2 <- count_x_2 + 1
    } else if(nthPick(balls, 3)) {
      #println("3rd pick", balls[1:10])
      count_x_3 <- count_x_3 + 1
    }
  }
  
  println("p_x_1", count_x_1/nTrials)
  println("p_x_2", count_x_2/nTrials)
  println("p_x_3", count_x_3/nTrials)
  p_x_4 <- (1-(count_x_1 + count_x_2 +count_x_3)/nTrials)
  println("p_x_4", p_x_4)
}

nbaSimulation(10000)