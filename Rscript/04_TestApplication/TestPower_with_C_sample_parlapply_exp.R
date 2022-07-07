#-------------------
# Prevalences & Data
#-------------------
P_A_e = data[A==1&E==1, .N]/data[E==1, .N]
P_A_ne = data[A==1&E==0, .N]/data[E==0, .N]

P_B_e = data[B==1&E==1, .N]/data[E==1, .N]
P_B_ne = data[B==1&E==0, .N]/data[E==0, .N]

P_C_e = data[C==1&E==1, .N]/data[E==1, .N]
P_C_ne = data[C==1&E==0, .N]/data[E==0, .N]

P_A =  data[A==1, .N]/N
P_B = data[B==1, .N]/N
P_C = data[C==1, .N]/N

DT_A <- data[A==1]
DT_B <- data[B==1]
DT_C <- data[C==1]

# sample size A
data_aggregated_A <- data[, .N, .(A, E)]

na_e <- min(as.integer(na/2), data_aggregated_A[E==1 & A==1, N])
na_ne <- min(as.integer(na/2), data_aggregated_A[E==0 & A==1, N])
if (na_e < as.integer(na/2) & na_ne == as.integer(na/2) ) {
  na_ne <- min(na - na_e, data_aggregated_A[E==0 & A==1, N])
}
if (na_e == as.integer(na/2) & na_ne < as.integer(na/2) ) {
  na_e <- min(na - na_ne, data_aggregated_A[E==1 & A==1, N])
}

# sample size B
data_aggregated_B <- data[, .N, .(B, E)]

nb_e <- min(as.integer(nb/2), data_aggregated_B[E==1 & B==1, N])
nb_ne <- min(as.integer(nb/2), data_aggregated_B[E==0 & B==1, N])
if (nb_e < as.integer(nb/2) & nb_ne == as.integer(nb/2) ) {
  nb_ne <- min(nb - nb_e, data_aggregated_B[E==0 & B==1, N])
}
if (nb_e == as.integer(nb/2) & nb_ne < as.integer(nb/2) ) {
  nb_e <- min(nb - nb_ne, data_aggregated_B[E==1 & B==1, N])
}

# sample size C
data_aggregated_C <- data[, .N, .(C, E)]

nc_e <- min(as.integer(nc/2), data_aggregated_C[E==1 & C==1, N])
nc_ne <- min(as.integer(nc/2), data_aggregated_C[E==0 & C==1, N])
if (nc_e < as.integer(nc/2) & nc_ne == as.integer(nc/2) ) {
  nc_ne <- min(nc - nc_e, data_aggregated_C[E==0 & C==1, N])
}
if (nc_e == as.integer(nc/2) & nc_ne < as.integer(nc/2) ) {
  nc_e <- min(nc - nc_ne, data_aggregated_C[E==1 & C==1, N])
}


#---------------------------------
# Generation nsam original samples
#---------------------------------
list_of_original_samples <- vector(mode = "list")

cores <- detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)
list_of_samples <- foreach(1:nsam, .packages='data.table') %dopar% {
  
  list_of_original_samples[["A"]] = rbind(DT_A[E==1][sample(nrow(DT_A[E==1]), na_e),],
                                          DT_A[E==0][sample(nrow(DT_A[E==0]), na_ne),])
  
  list_of_original_samples[["B"]] = rbind(DT_B[E==1][sample(nrow(DT_B[E==1]), nb_e),],
                                          DT_B[E==0][sample(nrow(DT_B[E==0]), nb_ne),])
  
  list_of_original_samples[["C"]] = rbind(DT_C[E==1][sample(nrow(DT_C[E==1]), nc_e),],
                                          DT_C[E==0][sample(nrow(DT_C[E==0]), nc_ne),])
  
  return(list_of_original_samples)
}
stopCluster(cl = cl)

#-------------------------------------------------------------------------------------
# Function to generate nboot bootstrap samples, cumputing the statistic and return 1/0
#-------------------------------------------------------------------------------------

boot_function <- function(list_of_original_samples){
  
  sample_tmp_A_e <- list_of_original_samples[["A"]][list_of_original_samples[["A"]]$E==1,]
  sample_tmp_B_e <- list_of_original_samples[["B"]][list_of_original_samples[["B"]]$E==1,]
  sample_tmp_C_e <- list_of_original_samples[["C"]][list_of_original_samples[["C"]]$E==1,]
  sample_tmp_A_ne <- list_of_original_samples[["A"]][list_of_original_samples[["A"]]$E==0,]
  sample_tmp_B_ne <- list_of_original_samples[["B"]][list_of_original_samples[["B"]]$E==0,]
  sample_tmp_C_ne <- list_of_original_samples[["C"]][list_of_original_samples[["C"]]$E==0,]
  
  TX_boot = c()
  for (v in 1:nboot) {
    #original_sample_A_e <- list_of_original_samples[["A"]][list_of_original_samples[["A"]]$E==1]
    
    boot_sample_tmp_A = rbind(sample_tmp_A_e[sample(nrow(sample_tmp_A_e), na_e, replace = TRUE),],
                              sample_tmp_A_ne[sample(nrow(sample_tmp_A_ne), na_ne, replace = TRUE),])
    
    boot_sample_tmp_B = rbind(sample_tmp_B_e[sample(nrow(sample_tmp_B_e), nb_e, replace = TRUE),],
                              sample_tmp_B_ne[sample(nrow(sample_tmp_B_ne), nb_ne, replace = TRUE),])
    
    boot_sample_tmp_C= rbind(sample_tmp_C_e[sample(nrow(sample_tmp_C_e), nc_e, replace = TRUE),],
                              sample_tmp_C_ne[sample(nrow(sample_tmp_C_ne), nc_ne, replace = TRUE),])
    
    # A=1 & E=1
    PPV_A_e_tmp = (sum(boot_sample_tmp_A$A==1 & boot_sample_tmp_A$Y==1 & boot_sample_tmp_A$E==1)) / 
      (sum(boot_sample_tmp_A$A==1 & boot_sample_tmp_A$E==1))
    
    PPV_A_e_tmp = ifelse(PPV_A_e_tmp>0 & (!is.na(PPV_A_e_tmp)) & PPV_A_e_tmp != Inf,
                          PPV_A_e_tmp,  1/(2*sum(boot_sample_tmp_A$A==1 & boot_sample_tmp_A$E==1)))
    
    # B=1 & E=1
    PPV_B_e_tmp = (sum(boot_sample_tmp_B$B==1 & boot_sample_tmp_B$Y==1 & boot_sample_tmp_B$E==1)) / 
      (sum(boot_sample_tmp_B$B==1 &  boot_sample_tmp_B$E==1))
    
    PPV_B_e_tmp = ifelse(PPV_B_e_tmp>0 & (!is.na(PPV_B_e_tmp)) & PPV_B_e_tmp != Inf,
                          PPV_B_e_tmp,  1/(2*sum(boot_sample_tmp_B$B==1 &  boot_sample_tmp_B$E==1)))
    
    # C=1 & E=1
    PPV_C_e_tmp = (sum(boot_sample_tmp_C$C==1 & boot_sample_tmp_C$Y==1 & boot_sample_tmp_C$E==0)) / 
      (sum(boot_sample_tmp_C$C==1 & boot_sample_tmp_C$E==0))
    
    PPV_C_e_tmp = ifelse(PPV_C_e_tmp>0 & (!is.na(PPV_C_e_tmp)) & PPV_C_e_tmp != Inf,
                          PPV_C_e_tmp,  1/(2*sum(boot_sample_tmp_C$C==1 & boot_sample_tmp_C$E==0)))
    # A=1 & E=0
    PPV_A_ne_tmp = (sum(boot_sample_tmp_A$A==1 & boot_sample_tmp_A$Y==1 & boot_sample_tmp_A$E==0)) / 
      (sum(boot_sample_tmp_A$A==1 & boot_sample_tmp_A$E==0))
    
    PPV_A_ne_tmp = ifelse(PPV_A_ne_tmp>0 & (!is.na(PPV_A_ne_tmp)) & PPV_A_ne_tmp != Inf,
                           PPV_A_ne_tmp,  1/(2*sum(boot_sample_tmp_A$A==1 & boot_sample_tmp_A$E==0)))
    # B=1 & E=0
    PPV_B_ne_tmp = (sum(boot_sample_tmp_B$B==1 & boot_sample_tmp_B$Y==1 & boot_sample_tmp_B$E==0)) / 
      (sum(boot_sample_tmp_B$B==1 & boot_sample_tmp_B$E==0))
    
    PPV_B_ne_tmp = ifelse(PPV_B_ne_tmp>0 & (!is.na(PPV_B_ne_tmp)) & PPV_B_ne_tmp != Inf,
                           PPV_B_ne_tmp,  1/(2*sum(boot_sample_tmp_B$B==1 & boot_sample_tmp_B$E==0)))
    # C=1 & E=0
    PPV_C_ne_tmp = (sum(boot_sample_tmp_C$C==1 & boot_sample_tmp_C$Y==1 & boot_sample_tmp_C$E==0)) / 
      (sum(boot_sample_tmp_C$C==1 & boot_sample_tmp_C$E==0))
    
    PPV_C_ne_tmp = ifelse(PPV_C_ne_tmp>0 & (!is.na(PPV_C_ne_tmp)) & PPV_C_ne_tmp != Inf,
                           PPV_C_ne_tmp,  1/(2*sum(boot_sample_tmp_C$C==1 & boot_sample_tmp_C$E==0)))
    
    
    # Test Statistic
    TX_boot[v] = statistic_with_C_sample(P_A_e = P_A_e, 
                                         P_B_e = P_B_e, 
                                         P_C_e = P_C_e,
                                         P_A_ne = P_A_ne,
                                         P_B_ne = P_B_ne,
                                         P_C_ne = P_C_ne,
                                         PPV_A_e =  PPV_A_e_tmp, 
                                         PPV_B_e =  PPV_B_e_tmp, 
                                         PPV_C_e =  PPV_C_e_tmp,
                                         PPV_A_ne = PPV_A_ne_tmp,
                                         PPV_B_ne =  PPV_B_ne_tmp,
                                         PPV_C_ne =  PPV_C_ne_tmp)
  }
  quant_025 = quantile(TX_boot, 0.025, na.rm = T)
  quant_975 = quantile(TX_boot, 0.975, na.rm = T)
  as.integer(quant_025 < 0 & quant_975>0)
  # Return 0 if the null hypothesis is not rejected, 1 otherwise
  return(as.integer(quant_025 < 0 & quant_975>0))
}

#-----------------------------------------------------------------------------
# Appling boot_function to list_of_samples and computing the power of the test
#-----------------------------------------------------------------------------
cores <- detectCores()
cl <- makeCluster(cores[1]- 1)
clusterExport(cl = cl, list("boot_function", 
                            "nboot", 
                            "list_of_samples", 
                            "na_e",
                            "nb_e",
                            "nc_e",
                            "na_ne",
                            "nb_ne",
                            "nc_ne",
                            "P_A_e" ,
                            "P_B_e" ,
                            "P_C_e" ,
                            "P_A_ne",
                            "P_B_ne",
                            "P_C_ne",
                            "statistic_with_C_sample"),
              envir = .GlobalEnv)
              #envir = environment())

list_of_test_results <- parLapply(cl = cl, list_of_samples, boot_function)
stopCluster(cl)

power_of_test <- 1 - (Reduce("+", list_of_test_results) / nsam)