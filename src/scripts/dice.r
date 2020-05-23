# roll d dice; find P(total = k)

#simulate dice roll
roll <- function(num_dice){
  return(sample(1:6, num_dice, replace=TRUE))
}

#function to calculate probability that total is k when rolling d dice for  nreps
prob_tot_k <- function(num_dice, expected_sum, nreps) {
  # do the experiment n reps
   sums <- replicate(nreps, sum(roll(num_dice)))
  return (mean(sums==expected_sum))
}

var_dices = 3
var_sum=8
var_reps=1000000

prob_tot_k(var_dices, var_sum, var_reps)