# Number of cores to be used 
n_of_core_to_be_used <- 2


#----------------------
# Simulation parameters
#----------------------

# Population size (scalar)
N <- 1000000

# Proportion of exposed (vector)
#prop_exp_list    <- c(0.05, 0.2)    
prop_exp_list    <- c(0.05) 

# Prevalence in the non-exposed group (vector)
#pi_ne_list <- c(0.01, 0.05, 0.1)    
pi_ne_list <- c(0.05)    

# Risk ratio (vector)
# risk_list <- c(1.2, 2)  
risk_list <- c(2)  

# Sensitivity of A intersect B (vector)
# SE_A_int_B_list <- c(0, 0.2, 0.4)
SE_A_int_B_list <- c(0.4)

# Sample sizes (list of lists)
# sample_size_list <-  list(list(a = 100, b = 100, c = 50), 
#                           list(a = 200, b = 200, c = 100),
#                           list(a = 300, b = 300, c = 150)) 

sample_size_list <-  list(list(a = 200, b = 200, c = 100))

# Sensitivity ratio across exposure groups: SE_e / SE_ne  (vector)
#se_ratio_list <- c(0.6, 0.8, 1, 1/0.6, 1/0.8)
se_ratio_list <- c(0.6)

# Sensitivity in the non exposed group (scalar)
SE_ne_scalar <- 0.5

# Sensitivity of A union B (scalar)
SE_AUB   <- 0.80

# Monte Carlo replication (scalar)
nsam  = 1000 

# Bootstrap sample size (scalar)
nboot = 500  