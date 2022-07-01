#----------------
# Bootstrap power
#----------------

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

#---------------------------------------------
# Generation nsam original samples in parallel
#---------------------------------------------
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

#-----------------------------------------------
# Generation nboot bootstrap samples in parallel
#-----------------------------------------------
boot_function_1 <- function(list_of_original_samples){
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

#tmp<-boot_function_1(list_of_samples[[1]])


boot_function_0 <- function(original_sample_data){
  statistic_vector<- replicate(nboot, boot_function_1(original_sample_data))
  quant_025 = quantile(statistic_vector,0.025, na.rm = T)
  quant_975 = quantile(statistic_vector,0.975, na.rm = T)
  
  return(as.integer(quant_025 < 0 & quant_975>0))
}

boot_function_2 <- function(list_of_original_samples){
  TX_boot0 = c()
  for (v in 1:nboot) {
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
    
    TX_boot0[v] = statistic_with_C_sample(P_A_e = P_A_e, 
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
  quant_025 = quantile(TX_boot0,0.025, na.rm = T)
  quant_975 = quantile(TX_boot0,0.975, na.rm = T)
  
  return(as.integer(quant_025 < 0 & quant_975>0))
}

#tmp<-boot_function_0(nboot, list_of_samples[[1]])

# cores <- detectCores()
# cl <- makeCluster(cores[1]- 1)
# clusterExport(cl = cl, list("boot_function_0", 
#                             "boot_function_1", 
#                             "nboot", 
#                             "list_of_samples", 
#                             "na",
#                             "nb",
#                             "nc",
#                             "P_A_e" ,
#                             "P_B_e" ,
#                             "P_C_e" ,
#                             "P_A_ne",
#                             "P_B_ne",
#                             "P_C_ne",
#                             "statistic_with_C_sample"),
#               envir = environment())

# par_test <- function(n){
#   parSapply(cl = cl, seq(n), function(i){boot_function_0(nboot, list_of_samples[[1]])})
#par_test(1000)
# }


#results <- parLapply(cl = cl, list_of_samples, boot_function_0)

#results <- parLapply(cl = cl, list_of_samples, boot_function_0)
stopCluster(cl)


#####################
# for loop

start_parallel <- Sys.time()
cores <- detectCores()
cl <- makeCluster(cores[1]- 1)
clusterExport(cl = cl, list("boot_function_0", 
                            "boot_function_1", 
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

# par_test <- function(n){
#   parSapply(cl = cl, seq(n), function(i){boot_function_0(nboot, list_of_samples[[1]])})
#par_test(1000)
# }


#results <- parLapply(cl = cl, list_of_samples, boot_function_0)

results <- parLapply(cl = cl, list_of_samples, boot_function_2)
stopCluster(cl)
end_parallel <- Sys.time()

time_parallel <- end_parallel - start_parallel
time_parallel
