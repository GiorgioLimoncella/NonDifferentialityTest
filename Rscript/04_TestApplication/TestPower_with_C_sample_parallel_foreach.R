#################################
######      Bootstrap      ######
#################################
# Sets: A==1 and B==1
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

#bootstrap_power
PPV_A_e = PPV_A_ne = PPV_B_e = PPV_B_ne = PPV_C_e = PPV_C_ne = NULL
n1=n2=n3=n4=NULL


#-----------------------------------------------
# Generation nsam original samples in parallel
#-----------------------------------------------

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

#------------------------------------------------------------------
# function to generate nboot sample bootstrap for 1 original sample
#------------------------------------------------------------------

boot_function_parallel <- function(list_of_original_samples){
  
  cores <- detectCores()
  cl <- makeCluster(cores[1]- 1)
  registerDoParallel(cl)
  
  list_of_statistic <- foreach(1:nboot, 
                               .export = c("statistic_with_C_sample", 
                                           "na", 
                                           "nb", 
                                           "nc", 
                                           "P_A_e",
                                           "P_B_e", 
                                           "P_C_e", 
                                           "P_A_ne",
                                           "P_B_ne",
                                           "P_C_ne"),
                               .combine="c") %dopar% {
                                 
    boot_sample_tmp_A = list_of_original_samples[["A"]][sample(nrow(list_of_original_samples[["A"]]), na, replace = TRUE),]
    boot_sample_tmp_B = list_of_original_samples[["B"]][sample(nrow(list_of_original_samples[["B"]]), nb, replace = TRUE),]
    boot_sample_tmp_C = list_of_original_samples[["C"]][sample(nrow(list_of_original_samples[["C"]]), nc, replace = TRUE),]
    
    PPV_A_e_tmp = (sum(boot_sample_tmp_A$A==1 & boot_sample_tmp_A$Y==1 & boot_sample_tmp_A$E==1)) / 
      (sum(boot_sample_tmp_A$A==1 & boot_sample_tmp_A$E==1))
    
    PPV_B_e_tmp = (sum(boot_sample_tmp_B$B==1 & boot_sample_tmp_B$Y==1 & boot_sample_tmp_B$E==1)) / 
      (sum(boot_sample_tmp_B$B==1 &  boot_sample_tmp_B$E==1))
    
    PPV_C_e_tmp = (sum(boot_sample_tmp_C$C==1 & boot_sample_tmp_C$Y==1 & boot_sample_tmp_C$E==0)) / 
      (sum(boot_sample_tmp_C$C==1 & boot_sample_tmp_C$E==0))
    
    PPV_A_ne_tmp = (sum(boot_sample_tmp_A$A==1 & boot_sample_tmp_A$Y==1 & boot_sample_tmp_A$E==0)) / 
      (sum(boot_sample_tmp_A$A==1 & boot_sample_tmp_A$E==0))
    
    PPV_B_ne_tmp = (sum(boot_sample_tmp_B$B==1 & boot_sample_tmp_B$Y==1 & boot_sample_tmp_B$E==0)) / 
      (sum(boot_sample_tmp_B$B==1 & boot_sample_tmp_B$E==0))
    
    PPV_C_ne_tmp = (sum(boot_sample_tmp_C$C==1 & boot_sample_tmp_C$Y==1 & boot_sample_tmp_C$E==0)) / 
      (sum(boot_sample_tmp_C$C==1 & boot_sample_tmp_C$E==0))
    
    TX_boot0 = statistic_with_C_sample(P_A_e = P_A_e, 
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
    
    return(TX_boot0)
  }
  stopCluster(cl = cl)
  
  
  quant_025 = quantile(list_of_statistic,0.025, na.rm = T)
  quant_975 = quantile(list_of_statistic,0.975, na.rm = T)
  
  return(as.integer(quant_025 < 0 & quant_975>0))
}


#--------------------------------------------------
# applying the function to all the original samples
#--------------------------------------------------
start_parallel <- Sys.time()
tmp <- boot_function_parallel(list_of_samples[[1]])
end_parallel <- Sys.time()
time_parallel <- end_parallel - start_parallel


vector_test_power <- lapply(list_of_samples, FUN =  boot_function_parallel)

#--------------------------------
# computing the power of the test
#--------------------------------
test_power = 1- sum(vector_test_power)/nsam