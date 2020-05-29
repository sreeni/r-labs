
max_passengers <- 2
single_sample <- 1


nreps <- 10000
nstops <- 10
count <- 0

tryAlightPassenger <- function(passengers){
   if(runif(1) < 0.2){
      return(passengers - 1)
   }
   return (passengers)
}

tryBoardingPassengers <- function(){
   boarding_probabilities <- c(0.5, 0.4, 0.1)
   newCount <- (sample(0:max_passengers, single_sample, prob=boarding_probabilities))
   return (newCount)
}

for (i in 1:nreps) {
    passengers <- 0

    for (j in 1:nstops) {
      if (passengers > 0){
         for (k in 1:passengers){
            passengers <- tryAlightPassenger(passengers)
         }
      }
      passengers <- passengers + tryBoardingPassengers()
    }
   
   if(passengers == 0){
      count <- count + 1
   }
}

print(count/nreps)