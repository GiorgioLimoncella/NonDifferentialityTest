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

TX_boot0=TX_boot2=matrix(0,nsam,nboot)
PPV_A_e0=PPV_A_ne0=PPV_B_e0=PPV_B_ne0=matrix(0,nsam,nboot)
PPV_A_e2=PPV_A_ne2=PPV_B_e2=PPV_B_ne2=matrix(0,nsam,nboot)

#set.seed(2908)

for(i in 1:nsam){
  
  sam_Ya1 = DT_A[sample(nrow(DT_A),na),]
  sam_Yb1 = DT_B[sample(nrow(DT_B),nb),]
  
  for(j in 1:nboot){
    
    sam_Ya_boot = sam_Ya1[sample(nrow(sam_Ya1), na, replace = TRUE),]
    sam_Yb_boot = sam_Yb1[sample(nrow(sam_Yb1), nb, replace = TRUE),]
    
    PPV_A_e0[i,j] = (sum(sam_Ya_boot$A==1&sam_Ya_boot$Y==1&sam_Ya_boot$E==1))/(sum(sam_Ya_boot$A==1&sam_Ya_boot$E==1))
    PPV_B_e0[i,j] = (sum(sam_Yb_boot$B==1&sam_Yb_boot$Y==1&sam_Yb_boot$E==1))/(sum(sam_Yb_boot$B==1&sam_Yb_boot$E==1))
    PPV_A_ne0[i,j] = (sum(sam_Ya_boot$A==1&sam_Ya_boot$Y==1&sam_Ya_boot$E==0))/(sum(sam_Ya_boot$A==1&sam_Ya_boot$E==0))
    PPV_B_ne0[i,j] = (sum(sam_Yb_boot$B==1&sam_Yb_boot$Y==1&sam_Yb_boot$E==0))/(sum(sam_Yb_boot$B==1&sam_Yb_boot$E==0))
    
    TX_boot0[i,j] = statistic(P_A_e = P_A_e, 
                              P_B_e = P_B_e, 
                              P_C_e = P_C_e,
                              P_A_ne = P_A_ne,
                              P_B_ne = P_B_ne,
                              P_C_ne = P_C_ne,
                              PPV_A_e =  PPV_A_e0[i,j], 
                              PPV_B_e =  PPV_B_e0[i,j], 
                              PPV_A_ne = PPV_A_ne0[i,j],
                              PPV_B_ne =  PPV_B_ne0[i,j])
    
  }
}


#set.seed(2908)

for(i in 1:nsam){
  
  sam_Ya_boot = sam_Ya1[sample(nrow(sam_Ya1), na, replace = TRUE),]
  sam_Yb_boot = sam_Yb1[sample(nrow(sam_Yb1), nb, replace = TRUE),]
  
  for(j in 1:nboot){
    
    PPV_B_e2[i,j] = ifelse( PPV_B_e0[i,j]>0 & (!is.na(PPV_B_e0[i,j])) & PPV_B_e0[i,j] != Inf,
                            PPV_B_e0[i,j],  1/(2*sum(sam_Yb_boot$B==1&sam_Yb_boot$E==1)))
    PPV_A_e2[i,j] = ifelse( PPV_A_e0[i,j]>0 & (!is.na(PPV_A_e0[i,j])) & PPV_A_e0[i,j] != Inf,
                            PPV_A_e0[i,j],  1/(2*sum(sam_Ya_boot$A==1&sam_Ya_boot$E==1)))
    
    PPV_B_ne2[i,j] = ifelse( PPV_B_ne0[i,j]>0 & (!is.na(PPV_B_ne0[i,j])) & PPV_B_ne0[i,j] != Inf,
                             PPV_B_ne0[i,j],  1/(2*sum(sam_Yb_boot$B==1&sam_Yb_boot$E==0)))
    PPV_A_ne2[i,j] = ifelse( PPV_A_ne0[i,j]>0 & (!is.na(PPV_A_ne0[i,j])) & PPV_A_ne0[i,j] != Inf,
                             PPV_A_ne0[i,j],  1/(2*sum(sam_Ya_boot$A==1&sam_Ya_boot$E==0)))

    
    TX_boot2[i,j] = statistic(P_A_e = P_A_e, 
                              P_B_e = P_B_e, 
                              P_C_e = P_C_e,
                              P_A_ne = P_A_ne,
                              P_B_ne = P_B_ne,
                              P_C_ne = P_C_ne,
                              PPV_A_e =  PPV_A_e2[i,j], 
                              PPV_B_e =  PPV_B_e2[i,j], 
                              PPV_A_ne = PPV_A_ne2[i,j],
                              PPV_B_ne =  PPV_B_ne2[i,j])
  }
}

quant_025 = quant_975 = c()
for(i in 1:nsam){
  quant_025[i] = quantile(TX_boot2[i,],0.025, na.rm = T)
  quant_975[i] = quantile(TX_boot2[i,],0.975, na.rm = T)
}

acc_95 = length(which(quant_025 < 0 & quant_975>0 ))/nsam
rej_95 = 1-acc_95
power_vector= c(power_vector, rej_95)
