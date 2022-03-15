### starting parameters
N        <- 200000
prop_exp <- i
pi_ne    <- j
risk     <- t
pi_e     <- pi_ne*risk


### validation indices: SE_AUB
SE_AUB           <- 0.90

### validation indices: E=1 & Y=1
SE_A_e           <- k
SE_B_given_A_e   <- 0.8
 
SE_A_int_B_e       <- SE_A_e * SE_B_given_A_e
SE_B_e             <- SE_AUB - SE_A_e + SE_A_int_B_e
SE_A_given_B_e     <- SE_A_int_B_e / SE_B_e
SE_B_given_not_A_e <- (SE_B_e - SE_A_int_B_e) / (1 - SE_A_e)
  
### validation indices: E=0 & Y=1
SE_A_ne          <- 0.5
SE_B_given_A_ne  <- 0.8

SE_A_int_B_ne       <- SE_A_ne * SE_B_given_A_ne
SE_B_ne             <- SE_AUB - SE_A_ne + SE_A_int_B_ne
SE_A_given_B_ne     <- SE_A_int_B_ne / SE_B_ne
SE_B_given_not_A_ne <- (SE_B_ne - SE_A_int_B_ne) / (1 - SE_A_ne)

### validation indices: E=1 & Y=0
SP_A_e           <- 0.99
SP_B_e           <- 0.90

SP_A_int_B_e     <- SP_A_e * SP_B_e
SP_AUB_e         <- SP_A_e + SP_B_e - SP_A_int_B_e

### validation indices: E=0 & Y=0
SP_A_ne          <- 0.99
SP_B_ne          <- 0.90

SP_A_int_B_ne    <- SP_A_ne * SP_B_ne
SP_AUB_ne        <- SP_A_ne + SP_B_ne - SP_A_int_B_ne

### Bootstrap parameters
na    = 250  # A sample size
nb    = 250  # B sample size
nsam  = 1000 # Monte Carlo rep
nboot = 500  # Bootstrap sample size

