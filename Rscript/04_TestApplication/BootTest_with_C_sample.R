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
DT_C <- data[C==1]

#bootstrap_power

PPV_A_e = PPV_A_ne = PPV_B_e = PPV_B_ne = PPV_C_e = PPV_C_ne = NULL
n1=n2=n3=n4=NULL

tx_boot=c()

TX_boot0=TX_boot2=c()
PPV_A_e0=PPV_A_ne0=PPV_B_e0=PPV_B_ne0=PPV_C_e0=PPV_C_ne0=c()
PPV_A_e2=PPV_A_ne2=PPV_B_e2=PPV_B_ne2=PPV_C_e2=PPV_C_ne2=c()

set.seed(3)

sam_Ya1 = DT_A[sample(nrow(DT_A),na),]
sam_Yb1 = DT_B[sample(nrow(DT_B),nb),]
sam_Yc1 = DT_C[sample(nrow(DT_C),nc),]

for(j in 1:nboot){
  
  sam_Ya_boot = sam_Ya1[sample(nrow(sam_Ya1), na, replace = TRUE),]
  sam_Yb_boot = sam_Yb1[sample(nrow(sam_Yb1), nb, replace = TRUE),]
  sam_Yc_boot = sam_Yc1[sample(nrow(sam_Yc1), nc, replace = TRUE),]
  
  PPV_A_e0 = c(PPV_A_e0,
               (sum(sam_Ya_boot$A==1&sam_Ya_boot$Y==1&sam_Ya_boot$E==1))/
                 (sum(sam_Ya_boot$A==1&sam_Ya_boot$E==1)))
  
  PPV_B_e0 = c(PPV_B_e0,
               (sum(sam_Yb_boot$B==1&sam_Yb_boot$Y==1&sam_Yb_boot$E==1))/
                 (sum(sam_Yb_boot$B==1&sam_Yb_boot$E==1)))
  
  PPV_C_e0 = c(PPV_C_e0,
               (sum(sam_Yc_boot$C==1&sam_Yc_boot$Y==1&sam_Yc_boot$E==1))/
                 (sum(sam_Yc_boot$C==1&sam_Yc_boot$E==1)))
  
  PPV_A_ne0 = c(PPV_A_ne0,
               (sum(sam_Ya_boot$A==1&sam_Ya_boot$Y==1&sam_Ya_boot$E==0))/
                 (sum(sam_Ya_boot$A==1&sam_Ya_boot$E==0)))
  
  PPV_B_ne0 = c(PPV_B_ne0,
               (sum(sam_Yb_boot$B==1&sam_Yb_boot$Y==1&sam_Yb_boot$E==0))/
                 (sum(sam_Yb_boot$B==1&sam_Yb_boot$E==0)))

  PPV_C_ne0 = c(PPV_C_ne0,
                (sum(sam_Yc_boot$C==1&sam_Yc_boot$Y==1&sam_Yc_boot$E==0))/
                  (sum(sam_Yc_boot$C==1&sam_Yc_boot$E==0)))  
  
  TX_boot0 = c(TX_boot0, statistic_with_C_sample(P_A_e = P_A_e, 
                                                 P_B_e = P_B_e, 
                                                 P_C_e = P_C_e,
                                                 P_A_ne = P_A_ne,
                                                 P_B_ne = P_B_ne,
                                                 P_C_ne = P_C_ne,
                                                 PPV_A_e =  PPV_A_e0[j], 
                                                 PPV_B_e =  PPV_B_e0[j], 
                                                 PPV_C_e =  PPV_C_e0[j],
                                                 PPV_A_ne = PPV_A_ne0[j],
                                                 PPV_B_ne =  PPV_B_ne0[j],
                                                 PPV_C_ne =  PPV_C_ne0[j]))
  
}

lb_025 = quantile(TX_boot0,0.025)
ub_975 = quantile(TX_boot0,0.975)

DT_test <- data.table(test_statistic = TX_boot0)
plt_test <- ggplot(DT_test, aes(TX_boot0))+
  geom_density(col="grey", fill="grey", alpha = 0.5)+
  scale_x_continuous(breaks = c(-0.5, 0, 0.5))+
  geom_vline(xintercept = c(lb_025, ub_975), col="black")+
  geom_vline(xintercept = 0, col="red")+
  labs(x="test statistic", y="")+
  theme_minimal()

