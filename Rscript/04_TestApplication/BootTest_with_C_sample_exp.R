#-----------
# Bootstrap
#-----------

TX_boot = c()
PPV_A_e = c()
PPV_A_ne = c()
PPV_B_e = c()
PPV_B_ne = c()
PPV_C_e = c()
PPV_C_ne = c()

#set.seed(3)

sam_orig_Ya = rbind(DT_A[E==1][sample(nrow(DT_A[E==1]), na_e),],
                    DT_A[E==0][sample(nrow(DT_A[E==0]), na_ne),])

sam_orig_Yb = rbind(DT_B[E==1][sample(nrow(DT_B[E==1]), nb_e),],
                    DT_B[E==0][sample(nrow(DT_B[E==0]), nb_ne),])

sam_orig_Yc = rbind(DT_C[E==1][sample(nrow(DT_C[E==1]), nc_e),],
                    DT_C[E==0][sample(nrow(DT_C[E==0]), nc_ne),])

for(j in 1:nboot){
  
  
  sam_Ya_boot = rbind(sam_orig_Ya[E==1][sample(nrow(sam_orig_Ya[E==1]), na_e),],
                      sam_orig_Ya[E==0][sample(nrow(sam_orig_Ya[E==0]), na_ne),])
  
  sam_Yb_boot = rbind(sam_orig_Yb[E==1][sample(nrow(sam_orig_Yb[E==1]), nb_e),],
                      sam_orig_Yb[E==0][sample(nrow(sam_orig_Yb[E==0]), nb_ne),])
  
  sam_Yc_boot = rbind(sam_orig_Yc[E==1][sample(nrow(sam_orig_Yc[E==1]), nc_e),],
                      sam_orig_Yc[E==0][sample(nrow(sam_orig_Yc[E==0]), nc_ne),])
  
  
  # A=1 & E=1
  PPV_A_e_tmp = (sum(sam_Ya_boot$A==1 & sam_Ya_boot$Y==1 & sam_Ya_boot$E==1)) / 
    (sum(sam_Ya_boot$A==1 & sam_Ya_boot$E==1))
  
  PPV_A_e_tmp = ifelse(PPV_A_e_tmp>0 & (!is.na(PPV_A_e_tmp)) & PPV_A_e_tmp != Inf,
                       PPV_A_e_tmp,  1/(2*sum(sam_Ya_boot$A==1 & sam_Ya_boot$E==1)))
  
  if (is.infinite(PPV_A_e_tmp)) {
    stop()
    
  }
  
  
  # B=1 & E=1
  PPV_B_e_tmp = (sum(sam_Yb_boot$B==1 & sam_Yb_boot$Y==1 & sam_Yb_boot$E==1)) / 
    (sum(sam_Yb_boot$B==1 &  sam_Yb_boot$E==1))
  
  PPV_B_e_tmp = ifelse(PPV_B_e_tmp>0 & (!is.na(PPV_B_e_tmp)) & PPV_B_e_tmp != Inf,
                       PPV_B_e_tmp,  1/(2*sum(sam_Yb_boot$B==1 &  sam_Yb_boot$E==1)))
  
  # C=1 & E=1
  PPV_C_e_tmp = (sum(sam_Yc_boot$C==1 & sam_Yc_boot$Y==1 & sam_Yc_boot$E==0)) / 
    (sum(sam_Yc_boot$C==1 & sam_Yc_boot$E==0))
  
  PPV_C_e_tmp = ifelse(PPV_C_e_tmp>0 & (!is.na(PPV_C_e_tmp)) & PPV_C_e_tmp != Inf,
                       PPV_C_e_tmp,  1/(2*sum(sam_Yc_boot$C==1 & sam_Yc_boot$E==0)))
  # A=1 & E=0
  PPV_A_ne_tmp = (sum(sam_Ya_boot$A==1 & sam_Ya_boot$Y==1 & sam_Ya_boot$E==0)) / 
    (sum(sam_Ya_boot$A==1 & sam_Ya_boot$E==0))
  
  PPV_A_ne_tmp = ifelse(PPV_A_ne_tmp>0 & (!is.na(PPV_A_ne_tmp)) & PPV_A_ne_tmp != Inf,
                        PPV_A_ne_tmp,  1/(2*sum(sam_Ya_boot$A==1 & sam_Ya_boot$E==0)))
  # B=1 & E=0
  PPV_B_ne_tmp = (sum(sam_Yb_boot$B==1 & sam_Yb_boot$Y==1 & sam_Yb_boot$E==0)) / 
    (sum(sam_Yb_boot$B==1 & sam_Yb_boot$E==0))
  
  PPV_B_ne_tmp = ifelse(PPV_B_ne_tmp>0 & (!is.na(PPV_B_ne_tmp)) & PPV_B_ne_tmp != Inf,
                        PPV_B_ne_tmp,  1/(2*sum(sam_Yb_boot$B==1 & sam_Yb_boot$E==0)))
  # C=1 & E=0
  PPV_C_ne_tmp = (sum(sam_Yc_boot$C==1 & sam_Yc_boot$Y==1 & sam_Yc_boot$E==0)) / 
    (sum(sam_Yc_boot$C==1 & sam_Yc_boot$E==0))
  
  PPV_C_ne_tmp = ifelse(PPV_C_ne_tmp>0 & (!is.na(PPV_C_ne_tmp)) & PPV_C_ne_tmp != Inf,
                        PPV_C_ne_tmp,  1/(2*sum(sam_Yc_boot$C==1 & sam_Yc_boot$E==0)))
  
  PPV_A_e = c(PPV_A_e, PPV_A_e_tmp)
  
  PPV_B_e = c(PPV_B_e, PPV_B_e_tmp)
  
  PPV_C_e = c(PPV_C_e, PPV_C_e_tmp)
  
  PPV_A_ne = c(PPV_A_ne, PPV_A_ne_tmp)
  
  PPV_B_ne = c(PPV_B_ne, PPV_B_ne_tmp)
  
  PPV_C_ne = c(PPV_C_ne, PPV_C_ne_tmp) 
  
  TX_boot = c(TX_boot, statistic_with_C_sample(P_A_e = P_A_e, 
                                               P_B_e = P_B_e, 
                                               P_C_e = P_C_e,
                                               P_A_ne = P_A_ne,
                                               P_B_ne = P_B_ne,
                                               P_C_ne = P_C_ne,
                                               PPV_A_e =  PPV_A_e[j], 
                                               PPV_B_e =  PPV_B_e[j], 
                                               PPV_C_e =  PPV_C_e[j],
                                               PPV_A_ne = PPV_A_ne[j],
                                               PPV_B_ne =  PPV_B_ne[j],
                                               PPV_C_ne =  PPV_C_ne[j]))
  
}

lb_025 = quantile(TX_boot,0.025)
ub_975 = quantile(TX_boot,0.975)

DT_test <- data.table(test_statistic = TX_boot)
plt_test <- ggplot(DT_test, aes(TX_boot))+
  geom_density(col="grey", fill="grey", alpha = 0.5)+
  scale_x_continuous(breaks = c(-0.5, 0, 0.5))+
  geom_vline(xintercept = c(lb_025, ub_975), col="black")+
  geom_vline(xintercept = 0, col="red")+
  labs(x="test statistic", y="")+
  theme_minimal()

