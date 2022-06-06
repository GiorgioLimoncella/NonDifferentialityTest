################################################################################
###########################      Bootstrap      ################################
################################################################################

# Sets A==1 and B==1
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


#bootstrap_power

PPV_A_e = PPV_A_ne = PPV_B_e = PPV_B_ne = NULL
n1=n2=n3=n4=NULL

tx_boot=c()

RR_est=c( )
RR_single_indicator=c( )
PPV_A_e0=PPV_A_ne0=PPV_B_e0=PPV_B_ne0=c( )
PPV_A_e2=PPV_A_ne2=PPV_B_e2=PPV_B_ne2=c( )


for(i in 1:nsam){
  
  sam_Ya1 = DT_A[sample(nrow(DT_A),na),]
  sam_Yb1 = DT_B[sample(nrow(DT_B),nb),]

    
  PPV_A_e0 = c(PPV_A_e0,
               (sum(sam_Ya1$A==1&sam_Ya1$Y==1&sam_Ya1$E==1))/
                 (sum(sam_Ya1$A==1&sam_Ya1$E==1)))
  
  PPV_B_e0 = c(PPV_B_e0,
               (sum(sam_Yb1$B==1&sam_Yb1$Y==1&sam_Yb1$E==1))/
                 (sum(sam_Yb1$B==1&sam_Yb1$E==1)))
  
  PPV_A_ne0 = c(PPV_A_ne0,
                (sum(sam_Ya1$A==1&sam_Ya1$Y==1&sam_Ya1$E==0))/
                  (sum(sam_Ya1$A==1&sam_Ya1$E==0)))
  
  PPV_B_ne0 = c(PPV_B_ne0,
                (sum(sam_Yb1$B==1&sam_Yb1$Y==1&sam_Yb1$E==0))/
                  (sum(sam_Yb1$B==1&sam_Yb1$E==0)))
  
  RR_est = c(RR_est, RiskRatio_est(P_A_e = P_A_e, 
                                   P_B_e = P_B_e, 
                                   P_C_e = P_C_e,
                                   P_A_ne = P_A_ne,
                                   P_B_ne = P_B_ne,
                                   P_C_ne = P_C_ne,
                                   PPV_A_e =  PPV_A_e0[i], 
                                   PPV_B_e =  PPV_B_e0[i], 
                                   PPV_A_ne = PPV_A_ne0[i],
                                   PPV_B_ne =  PPV_B_ne0[i]))
  
  RR_single_indicator = c( RR_single_indicator, (P_A_e/P_A_ne) * (PPV_A_e0[i]/PPV_A_ne0[i]))
  
}

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
