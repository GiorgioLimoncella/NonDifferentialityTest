# RiskRatio_est <- function(P_A_e, 
#                           P_B_e, 
#                           P_C_e,
#                           P_A_ne, 
#                           P_B_ne, 
#                           P_C_ne,
#                           PPV_A_e, 
#                           PPV_B_e, 
#                           PPV_A_ne,
#                           PPV_B_ne){
#   
#   RR <- (P_A_e*PPV_A_e + P_B_e*PPV_B_e - P_C_e*max(PPV_A_e, PPV_B_e)) /
#     (P_A_ne*PPV_A_ne + P_B_ne*PPV_B_ne - P_C_ne*max(PPV_A_ne, PPV_B_ne)) 
#   
#   return(RR)
# }

RiskRatio_est_with_C_sample <- function(P_A_e, 
                                        P_B_e, 
                                        P_C_e,
                                        P_A_ne, 
                                        P_B_ne, 
                                        P_C_ne,
                                        PPV_A_e, 
                                        PPV_B_e,
                                        PPV_C_e,
                                        PPV_A_ne,
                                        PPV_B_ne,
                                        PPV_C_ne){
  
  RR <- (P_A_e*PPV_A_e + P_B_e*PPV_B_e - P_C_e*PPV_C_e) /
    (P_A_ne*PPV_A_ne + P_B_ne*PPV_B_ne - P_C_ne*PPV_C_ne) 
  
  return(RR)
}




RiskRatio_est_with_C_empty <- function(P_A_e, 
                                        P_B_e, 
                                        P_A_ne, 
                                        P_B_ne, 
                                        PPV_A_e, 
                                        PPV_B_e,
                                        PPV_A_ne,
                                        PPV_B_ne){
  
  RR <- (P_A_e*PPV_A_e + P_B_e*PPV_B_e) /
        (P_A_ne*PPV_A_ne + P_B_ne*PPV_B_ne) 
  
  return(RR)
}