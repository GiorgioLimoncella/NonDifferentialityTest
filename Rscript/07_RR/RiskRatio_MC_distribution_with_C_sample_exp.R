#------------
# Risk Ratio
#------------

tx_boot = c()
RR_est = c( )
RR_single_indicator = c( )
PPV_A_e = c()
PPV_A_ne = c()
PPV_B_e = c()
PPV_B_ne = c()
PPV_C_e = c()
PPV_C_ne = c()


for(i in 1:nsam){
  
  sam_Ya = rbind(DT_A[E==1][sample(nrow(DT_A[E==1]), na_e),],
                  DT_A[E==0][sample(nrow(DT_A[E==0]), na_ne),])
  
  sam_Yb = rbind(DT_B[E==1][sample(nrow(DT_B[E==1]), nb_e),],
                  DT_B[E==0][sample(nrow(DT_B[E==0]), nb_ne),])
  
  sam_Yc = rbind(DT_C[E==1][sample(nrow(DT_C[E==1]), nc_e),],
                  DT_C[E==0][sample(nrow(DT_C[E==0]), nc_ne),])

  # A=1 & E=1
  PPV_A_e_tmp = (sum(sam_Ya$A==1 & sam_Ya$Y==1 & sam_Ya$E==1)) / 
    (sum(sam_Ya$A==1 & sam_Ya$E==1))
  
  PPV_A_e_tmp = ifelse(PPV_A_e_tmp>0 & (!is.na(PPV_A_e_tmp)) & PPV_A_e_tmp != Inf,
                       PPV_A_e_tmp,  1/(2*sum(sam_Ya$A==1 & sam_Ya$E==1)))
  
  # B=1 & E=1
  PPV_B_e_tmp = (sum(sam_Yb$B==1 & sam_Yb$Y==1 & sam_Yb$E==1)) / 
    (sum(sam_Yb$B==1 &  sam_Yb$E==1))
  
  PPV_B_e_tmp = ifelse(PPV_B_e_tmp>0 & (!is.na(PPV_B_e_tmp)) & PPV_B_e_tmp != Inf,
                       PPV_B_e_tmp,  1/(2*sum(sam_Yb$B==1 &  sam_Yb$E==1)))
  
  # C=1 & E=1
  PPV_C_e_tmp = (sum(sam_Yc$C==1 & sam_Yc$Y==1 & sam_Yc$E==0)) / 
    (sum(sam_Yc$C==1 & sam_Yc$E==0))
  
  PPV_C_e_tmp = ifelse(PPV_C_e_tmp>0 & (!is.na(PPV_C_e_tmp)) & PPV_C_e_tmp != Inf,
                       PPV_C_e_tmp,  1/(2*sum(sam_Yc$C==1 & sam_Yc$E==0)))
  # A=1 & E=0
  PPV_A_ne_tmp = (sum(sam_Ya$A==1 & sam_Ya$Y==1 & sam_Ya$E==0)) / 
    (sum(sam_Ya$A==1 & sam_Ya$E==0))
  
  PPV_A_ne_tmp = ifelse(PPV_A_ne_tmp>0 & (!is.na(PPV_A_ne_tmp)) & PPV_A_ne_tmp != Inf,
                        PPV_A_ne_tmp,  1/(2*sum(sam_Ya$A==1 & sam_Ya$E==0)))
  # B=1 & E=0
  PPV_B_ne_tmp = (sum(sam_Yb$B==1 & sam_Yb$Y==1 & sam_Yb$E==0)) / 
    (sum(sam_Yb$B==1 & sam_Yb$E==0))
  
  PPV_B_ne_tmp = ifelse(PPV_B_ne_tmp>0 & (!is.na(PPV_B_ne_tmp)) & PPV_B_ne_tmp != Inf,
                        PPV_B_ne_tmp,  1/(2*sum(sam_Yb$B==1 & sam_Yb$E==0)))
  # C=1 & E=0
  PPV_C_ne_tmp = (sum(sam_Yc$C==1 & sam_Yc$Y==1 & sam_Yc$E==0)) / 
    (sum(sam_Yc$C==1 & sam_Yc$E==0))
  
  PPV_C_ne_tmp = ifelse(PPV_C_ne_tmp>0 & (!is.na(PPV_C_ne_tmp)) & PPV_C_ne_tmp != Inf,
                        PPV_C_ne_tmp,  1/(2*sum(sam_Yc$C==1 & sam_Yc$E==0)))
  
  PPV_A_e = c(PPV_A_e, PPV_A_e_tmp)
  
  PPV_B_e = c(PPV_B_e, PPV_B_e_tmp)
  
  PPV_C_e = c(PPV_C_e, PPV_C_e_tmp)
  
  PPV_A_ne = c(PPV_A_ne, PPV_A_ne_tmp)
  
  PPV_B_ne = c(PPV_B_ne, PPV_B_ne_tmp)
  
  PPV_C_ne = c(PPV_C_ne, PPV_C_ne_tmp) 
  
  
  
  
  RR_est = c(RR_est, RiskRatio_est_with_C_sample(P_A_e = P_A_e, 
                                   P_B_e = P_B_e, 
                                   P_C_e = P_C_e,
                                   P_A_ne = P_A_ne,
                                   P_B_ne = P_B_ne,
                                   P_C_ne = P_C_ne,
                                   PPV_A_e =  PPV_A_e[i], 
                                   PPV_B_e =  PPV_B_e[i],
                                   PPV_C_e =  PPV_C_e[i],
                                   PPV_A_ne = PPV_A_ne[i],
                                   PPV_B_ne =  PPV_B_ne[i],
                                   PPV_C_ne =  PPV_C_ne[i]))
  
  RR_single_indicator = c( RR_single_indicator, (P_A_e/P_A_ne) * (PPV_A_e[i]/PPV_A_ne[i]))
  

}


#-----
# MSE
#-----

rmse_RR_multiple <-  sqrt(sum((RR_est - risk)^2)/nsam)
rmse_RR_single <-  sqrt(sum((RR_single_indicator - risk)^2)/nsam)




#------
# Plot
#------
DT_RR_est <- data.table(RR_estimate = RR_est)
plt_RR <- ggplot(DT_RR_est, aes(RR_estimate))+
  geom_density(col="grey", fill="grey", alpha = 0.5)+
  #scale_x_continuous(breaks = c(-0.5, 0, 0.5), limits = c(-0.2,1))+
  geom_vline(xintercept = mean(RR_est), col="black")+
  geom_vline(xintercept = risk, col="salmon")+
  labs(x="Risk Ratio estimate", y="")+
  theme_minimal()

DT_RR_single_indicator_est <- data.table(RR_estimate = RR_single_indicator)
plt_RR_single_indicator <- ggplot(DT_RR_single_indicator_est, aes(RR_estimate))+
  geom_density(col="grey", fill="grey", alpha = 0.5)+
  #scale_x_continuous(breaks = c(-0.5, 0, 0.5), limits = c(-0.2,1))+
  geom_vline(xintercept = mean(RR_single_indicator), col="black")+
  geom_vline(xintercept = risk, col="salmon")+
  labs(x="Risk Ratio estimate", y="")+
  theme_minimal()
