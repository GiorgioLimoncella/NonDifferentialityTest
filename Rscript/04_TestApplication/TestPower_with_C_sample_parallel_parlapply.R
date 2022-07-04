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

#---------------------------------
# Generation nsam original samples
#---------------------------------
list_of_original_samples <- vector(mode = "list")

cores <- detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)
list_of_samples <- foreach(1:nsam) %dopar%{
  list_of_original_samples[["A"]] = DT_A[sample(nrow(DT_A),na),]
  list_of_original_samples[["B"]] = DT_B[sample(nrow(DT_B),nb),]
  list_of_original_samples[["C"]] = DT_C[sample(nrow(DT_C),nc),]
  return(list_of_original_samples)
}
stopCluster(cl = cl)

#-------------------------------------------------------------------------------------
# Function to generate nboot bootstrap samples, cumputing the statistic and return 1/0
#-------------------------------------------------------------------------------------

boot_function <- function(list_of_original_samples){
  TX_boot = c()
  for (v in 1:nboot) {
    boot_sample_tmp_A = list_of_original_samples[["A"]][sample(nrow(list_of_original_samples[["A"]]), na, replace = TRUE),]
    boot_sample_tmp_B = list_of_original_samples[["B"]][sample(nrow(list_of_original_samples[["B"]]), nb, replace = TRUE),]
    boot_sample_tmp_C = list_of_original_samples[["C"]][sample(nrow(list_of_original_samples[["C"]]), nc, replace = TRUE),]
    
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
                            "na",
                            "nb",
                            "nc",
                            "P_A_e" ,
                            "P_B_e" ,
                            "P_C_e" ,
                            "P_A_ne",
                            "P_B_ne",
                            "P_C_ne",
                            "statistic_with_C_sample"),
              envir = environment())

list_of_test_results <- parLapply(cl = cl, list_of_samples, boot_function)
stopCluster(cl)

power_of_test <- 1 - (Reduce("+", list_of_test_results) / nsam)
