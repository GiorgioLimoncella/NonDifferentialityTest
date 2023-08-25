# commented because already in the environment from "TestPower_with_C_sample_parallel_parlapply.R"

# #-------------------
# # Prevalences & Data
# #-------------------
# P_A_e = data[A==1&E==1, .N]/data[E==1, .N]
# P_A_ne = data[A==1&E==0, .N]/data[E==0, .N]
# 
# P_B_e = data[B==1&E==1, .N]/data[E==1, .N]
# P_B_ne = data[B==1&E==0, .N]/data[E==0, .N]
# 
# P_C_e = data[C==1&E==1, .N]/data[E==1, .N]
# P_C_ne = data[C==1&E==0, .N]/data[E==0, .N]
# 
# P_A =  data[A==1, .N]/N
# P_B = data[B==1, .N]/N
# P_C = data[C==1, .N]/N
# 
# DT_A <- data[A==1]
# DT_B <- data[B==1]
# DT_C <- data[C==1]
# 
# #---------------------------------
# # Generation nsam original samples
# #---------------------------------
# list_of_original_samples <- vector(mode = "list")
# 
# cores <- detectCores()
# cl <- makeCluster()
# registerDoParallel(cl)
# list_of_samples <- foreach(1:nsam) %dopar%{
#   list_of_original_samples[["A"]] = DT_A[sample(nrow(DT_A),na),]
#   list_of_original_samples[["B"]] = DT_B[sample(nrow(DT_B),nb),]
#   list_of_original_samples[["C"]] = DT_C[sample(nrow(DT_C),nc),]
#   return(list_of_original_samples)
# }
# stopCluster(cl = cl)


#-------------------------------
# Function to compute PPV and RR
#-------------------------------
PPV_RR_function <- function(list_of_samples){
  
  PPV_A_e <- list_of_samples[["A"]][A==1 & E==1 & Y==1, .N] / list_of_samples[["A"]][A==1 & E==1, .N]
  PPV_A_ne <- list_of_samples[["A"]][A==1 & E==0 & Y==1, .N] / list_of_samples[["A"]][A==1 & E==0, .N]
  
  PPV_B_e <- list_of_samples[["B"]][B==1 & E==1 & Y==1, .N] / list_of_samples[["B"]][B==1 & E==1, .N]
  PPV_B_ne <- list_of_samples[["B"]][B==1 & E==0 & Y==1, .N] / list_of_samples[["B"]][B==1 & E==0, .N]
  
  PPV_C_e <- list_of_samples[["C"]][C==1 & E==1 & Y==1, .N] / list_of_samples[["C"]][C==1 & E==1, .N]
  PPV_C_ne <- list_of_samples[["C"]][C==1 & E==0 & Y==1, .N] / list_of_samples[["C"]][C==1 & E==0, .N]
  
  RR <- (P_A_e * PPV_A_e + P_B_e * PPV_B_e - P_C_e * PPV_C_e) / 
    (P_A_ne * PPV_A_ne + P_B_ne * PPV_B_ne - P_C_ne * PPV_C_ne)
  
  row_PPV <- data.table(PPV_A_e  = PPV_A_e ,
                        PPV_A_ne = PPV_A_ne,
                        PPV_B_e  = PPV_B_e ,
                        PPV_B_ne = PPV_B_ne,
                        PPV_C_e  = PPV_C_e ,
                        PPV_C_ne = PPV_C_ne,
                        RR = RR)
}


PPV_RR_function_empty <- function(list_of_samples){
  
  PPV_A_e <- list_of_samples[["A"]][A==1 & E==1 & Y==1, .N] / list_of_samples[["A"]][A==1 & E==1, .N]
  PPV_A_ne <- list_of_samples[["A"]][A==1 & E==0 & Y==1, .N] / list_of_samples[["A"]][A==1 & E==0, .N]
  
  PPV_B_e <- list_of_samples[["B"]][B==1 & E==1 & Y==1, .N] / list_of_samples[["B"]][B==1 & E==1, .N]
  PPV_B_ne <- list_of_samples[["B"]][B==1 & E==0 & Y==1, .N] / list_of_samples[["B"]][B==1 & E==0, .N]
  
  
  RR <- (P_A_e * PPV_A_e + P_B_e * PPV_B_e) / 
    (P_A_ne * PPV_A_ne + P_B_ne * PPV_B_ne)
  
  row_PPV <- data.table(PPV_A_e  = PPV_A_e ,
                        PPV_A_ne = PPV_A_ne,
                        PPV_B_e  = PPV_B_e ,
                        PPV_B_ne = PPV_B_ne,
                        RR = RR)
}


#--------------------------------------------------------------------------------
# Appling PPV_RR_function to list_of_samples and computing PPV & RR distributions
#--------------------------------------------------------------------------------
#cores <- detectCores()
cl <- makeCluster(n_of_core_to_be_used)

if(SE_A_int_B_ne == 0){
  clusterExport(cl = cl, list("PPV_RR_function",  
                              "list_of_samples", 
                              "data.table",
                              "P_A_e" ,
                              "P_B_e" ,
                              "P_A_ne",
                              "P_B_ne"),
                envir = environment())
  
  list_of_PPV_RR_results <- parLapply(cl = cl, list_of_samples, PPV_RR_function_empty)
}else{
  clusterExport(cl = cl, list("PPV_RR_function",  
                              "list_of_samples", 
                              "data.table",
                              "P_A_e" ,
                              "P_B_e" ,
                              "P_C_e" ,
                              "P_A_ne",
                              "P_B_ne",
                              "P_C_ne"),
                envir = environment())
  
  list_of_PPV_RR_results <- parLapply(cl = cl, list_of_samples, PPV_RR_function)
}

stopCluster(cl)

DT_PPV_RR <- rbindlist(list_of_PPV_RR_results) 

