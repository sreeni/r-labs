#ALOHA SIM Finds P(X1=2) P(X2=2) P(X1=1|X2=2)
sim <- function(p, q, nreps){
    count_x2_eq_2 <- 0
    count_x1_eq_1 <- 0
    count_x1_eq_2 <- 0
    
    count_x2_eq_2_given_x1_eq_1 <- 0

    for(i in 1:nreps){
        numsend <- 0 #no messages sent so far
        X1 <- 0
        X2 <- 0

        #Simulate A and B's decision whether to send in Epoch 1
        for (j in 1:2){
            #simulate sending of message for each node
            if (runif(1) < p){
                numsend <- numsend + 1
            }
        }
        
        #initialize X1 based on number of messages sent.
        if(numsend == 1){
            X1 <- 1
        } else {
            X1 <- 2
        }

        #increment count of x1 count for 1 and 2 messages sent.
        if(X1 == 2){
            count_x1_eq_2 <- count_x1_eq_2 + 1
        }

        #Simulate A and B's decision whether to send in Epoch 2
        num_active <- X1
        if(X1 == 1){
            if(runif(1) < q){
                num_active <- num_active + 1
            }
        }

        if(num_active == 1){
            #try sending message
            if(runif(1) < p){
                X2 <- 0 #message was sent successfully
            } else {
                X2 <- 1 #message was not sent
            }
        } else {
            num_send <- 0
            for(i in 1:2){
                if(runif(1) < p){
                    num_send <- num_send + 1
                }
            }

            if(num_send == 1){
                X2 == 1 #one message was sent, hence one node became inactive
            } else {
                X2 == 2 #message was not sent due to collission
            }
        }

        if(X2 == 2){
            count_x2_eq_2 <- count_x2_eq_2 + 1
        }
        
        if(X1 == 1){
            count_x1_eq_1 <- count_x1_eq_1 + 1
            if(X2 == 2){
                count_x2_eq_2_given_x1_eq_1 <- count_x2_eq_2_given_x1_eq_1 + 1
            }
        }
    }

    #Print results
    cat("P(X1 = 2) ", count_x1_eq_2/nreps, "\n")
    cat("P(X2 = 2) ", count_x2_eq_2/nreps, "\n")
    cat("P(X2 = 2 | X1 = 1) ", count_x2_eq_2_given_x1_eq_1/count_x1_eq_1, "\n")
}

sim(0.4, 0.8, 1000)