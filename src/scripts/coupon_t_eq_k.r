collectCoupons <- function(nCouponTypes, nCouponsToCollect) {
  coupons <- sample(1:nCouponTypes, nCouponsToCollect, replace = TRUE )
  return(coupons)
}

allCouponsCollected <- function(nCouponTypes, coupons){
  couponsCollected <- array(nCouponTypes, data = FALSE)
  for(i in coupons){
    couponsCollected[i] <- TRUE
  }
  return (all(couponsCollected))
}

runExperiment <- function(nCouponTypes, nTrials, kStart, kEnd){
  for(k in kStart: kEnd){
    count <- 0
    for(i in 1:nTrials){
      coupons <- collectCoupons(nCouponTypes, k)
      if(allCouponsCollected(nCouponTypes, coupons)){
        count <- count + 1
      }
    }
    p_k <- count/nTrials
    cat("probability for k=",k, p_k, "\n")
  }
}

runExperiment(nCouponTypes = 10, nTrials = 10000, kStart =9, kEnd = 50)

