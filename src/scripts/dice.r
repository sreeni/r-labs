# roll d dice; find P(total = k)

#simulate dice roll
roll <- function(num_dice){
  return(sample(1:6, num_dice, replace=TRUE))
}

#function to calculate probability that total is k when rolling d dice for  nreps
prob_tot_k <- function(num_dice, expected_sum, nreps) {
  # do the experiment n reps
   sums <- replicate(nreps, sum(roll(num_dice)))# [3, 4, 5]
  return (mean(sums==expected_sum)) #8 -> [3, 4, 8] [8,8,8,]  1/3
}

var_dices = 2
var_sum=8
var_reps=1000000

cat("prop dice", prob_tot_k(var_dices, var_sum, var_reps))

