################################################################################
########################## Bootstrap ###########################################
################################################################################

# Sets A==1 and B==1
n=nb=250
nsam = 1000

P_A_e = sum(A==1&E==1)/sum(E==1)
P_A_ne = sum(A==1&E==0)/sum(E==0)

P_B_e = sum(B==1&E==1)/sum(E==1)
P_B_ne = sum(B==1&E==0)/sum(E==0)

P_C_e = sum(C==1&E==1)/sum(E==1)
P_C_ne = sum(C==1&E==0)/sum(E==0) 

P_A =  sum(A==1)/N
P_B = sum(B==1)/N
P_c = sum(C==1)/N


c1 = P_B_e/P_A_e; c2 = P_B_ne/P_A_ne ; c3 = P_C_e/P_A_e ; c4 = P_C_ne/P_A_ne

matrix_A = as.data.frame(cbind(Y[A==1],A[A==1],E[A==1]))
matrix_B = as.data.frame(cbind(Y[B==1],B[B==1],E[B==1]))
colnames(matrix_A)=c("Y","A","E")
colnames(matrix_B)=c("Y","B","E")


#bootstrap_power

PPV_A_e = PPV_A_ne = PPV_B_e = PPV_B_ne = NULL
n1=n2=n3=n4=NULL
nboot=500
tx_boot=c()

TX_boot0=TX_boot2=matrix(0,nsam,nboot)
PPV_A_e0=PPV_A_ne0=PPV_B_e0=PPV_B_ne0=matrix(0,nsam,nboot)
PPV_A_e2=PPV_A_ne2=PPV_B_e2=PPV_B_ne2=matrix(0,nsam,nboot)

#set.seed(2908)

for(i in 1:nsam){
  
  sam_Ya1 = matrix_A[sample(nrow(matrix_A),n),]
  sam_Yb1 = matrix_B[sample(nrow(matrix_B),nb),]
  
  for(j in 1:nboot){
    
    sam_Ya_boot = sam_Ya1[sample(nrow(sam_Ya1), n, replace = TRUE),]
    sam_Yb_boot = sam_Yb1[sample(nrow(sam_Yb1), nb, replace = TRUE),]
    
    PPV_A_e0[i,j] = (sum(sam_Ya_boot$A==1&sam_Ya_boot$Y==1&sam_Ya_boot$E==1))/(sum(sam_Ya_boot$A==1&sam_Ya_boot$E==1))
    PPV_B_e0[i,j] = (sum(sam_Yb_boot$B==1&sam_Yb_boot$Y==1&sam_Yb_boot$E==1))/(sum(sam_Yb_boot$B==1&sam_Yb_boot$E==1))
    PPV_A_ne0[i,j] = (sum(sam_Ya_boot$A==1&sam_Ya_boot$Y==1&sam_Ya_boot$E==0))/(sum(sam_Ya_boot$A==1&sam_Ya_boot$E==0))
    PPV_B_ne0[i,j] = (sum(sam_Yb_boot$B==1&sam_Yb_boot$Y==1&sam_Yb_boot$E==0))/(sum(sam_Yb_boot$B==1&sam_Yb_boot$E==0))
    
    TX_boot0[i,j] = c1*PPV_B_e0[i,j]/PPV_A_e0[i,j]-c2*PPV_B_ne0[i,j]/PPV_A_ne0[i,j] - c3*max(PPV_B_e0[i,j],PPV_A_e0[i,j])/PPV_A_e0[i,j] + c4*max(PPV_B_ne0[i,j],PPV_A_ne0[i,j])/PPV_A_ne0[i,j] 
    
  }}


#set.seed(2908)

for(i in 1:nsam){
  
  sam_Ya_boot = sam_Ya1[sample(nrow(sam_Ya1), n, replace = TRUE),]
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
    
    TX_boot2[i,j] = c1*PPV_B_e2[i,j]/PPV_A_e2[i,j]-c2*PPV_B_ne2[i,j]/PPV_A_ne2[i,j] - c3*max(PPV_B_e2[i,j],PPV_A_e2[i,j])/PPV_A_e2[i,j] + c4*max(PPV_B_ne2[i,j],PPV_A_ne2[i,j])/PPV_A_ne2[i,j]
    
  }
}

quant_025 = quant_975 = c()
for(i in 1:nsam){
  quant_025[i] = quantile(TX_boot2[i,],0.025)
  quant_975[i] = quantile(TX_boot2[i,],0.975)
}

acc_95 = length(which(quant_025 < 0 & quant_975>0 ))/nsam
rej_95 = 1-acc_95
power= c(power, rej_95)