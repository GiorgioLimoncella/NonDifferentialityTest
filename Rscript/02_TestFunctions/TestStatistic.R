# statistic <- function(P_A_e, 
#                       P_B_e, 
#                       P_C_e,
#                       P_A_ne, 
#                       P_B_ne, 
#                       P_C_ne,
#                       PPV_A_e, 
#                       PPV_B_e, 
#                       PPV_A_ne,
#                       PPV_B_ne){
#   
#   TA_e <- P_A_e * PPV_A_e
#   TA_ne <- P_A_ne * PPV_A_ne
#   TB_e <- P_B_e * PPV_B_e
#   TB_ne <- P_B_ne * PPV_B_ne
#   TC_e <- P_C_e * max(PPV_A_e, PPV_B_e)
#   TC_ne <- P_C_ne * max(PPV_A_ne, PPV_B_ne)
#   
#   
#   TX = ((TA_e * (TA_ne + TB_ne - TC_ne)) / (TA_ne * (TA_e + TB_e - TC_e))) -1
#   
#   return(TX)
# }

statistic_with_C_sample <- function(P_A_e, 
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
  
  TA_e <- P_A_e * PPV_A_e
  TA_ne <- P_A_ne * PPV_A_ne
  TB_e <- P_B_e * PPV_B_e
  TB_ne <- P_B_ne * PPV_B_ne
  TC_e <- P_C_e * PPV_C_e
  TC_ne <- P_C_ne * PPV_C_ne
  
  
  TX = ((TA_e * (TA_ne + TB_ne - TC_ne)) / (TA_ne * (TA_e + TB_e - TC_e))) -1
  
  return(TX)
}