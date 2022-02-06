library(data.table)

rm(list=ls())


  ## starting parameters
  N = 10000; prop_e= 0.2
  pi_ne = 0.05; risk = 2
  pi_e = pi_ne*risk
  
  ## validation indices: exposed
  SE_A_e=0.7
  SE_AintB_e = 0.1
  SP_A_e = 0.99
  SP_B_e=0.95

  ## validation indices: non-exposed
  SE_A_ne = 0.4
  SE_AintB_ne = 0.1
  SP_A_ne = 0.99
  SP_B_ne = 0.95

  # Data generation
  E = c(rep(0,(1-prop_e)*N), rep(1,prop_e*N))
  
  Y = ifelse(E==1, sapply(E,function(pe){rbinom(1,1,pi_e)}),
             sapply(E,function(pne){rbinom(1,1,pi_ne)}))
  
  dati = cbind(E,Y)
  
  A = NA
  A = ifelse(Y==1 & E==1, apply(dati, 1, function(SEe){rbinom(1, 1, SE_A_e)}),
             ifelse(Y==1 & E==0, apply(dati, 1, function(SEne){rbinom(1, 1, SE_A_ne)}), 
                    ifelse(Y==0 & E==1, apply(dati, 1, function(FPe){rbinom(1, 1, 1-SP_A_e)}), 
                           ifelse(Y==0 & E==0, apply(dati, 1, function(FPne){rbinom(1, 1, 1-SP_A_ne)}), A))))
  
  B = NA
  B = ifelse(Y==1 & A==0, apply(dati, 1, function(SE1){rbinom(1, 1, 1)}),
             ifelse(Y==1 & A==1 & E==0, apply(dati, 1, function(SEne){rbinom(1, 1, SE_AintB_ne)}),
                    ifelse(Y==1 & A==1 & E==1, apply(dati, 1, function(SEe){rbinom(1, 1, SE_AintB_e)}),
                           ifelse(Y==0 & E==1, apply(dati, 1, function(FPe){rbinom(1, 1, 1-SP_B_e)}), 
                                  ifelse(Y==0 & E==0, apply(dati, 1, function(FPne){rbinom(1, 1, 1-SP_B_ne)}), B)))))
  
  C = NA
  C = A*B
  
  
  ################################################################################
  ###########################      Bootstrap      ################################
  ################################################################################
  
  # Sets A==1 and B==1
  n=nb=100
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

  quant_025 = quant_975 = c()
  for(i in 1:nsam){
    quant_025[i] = quantile(TX_boot0[i,],0.025)
    quant_975[i] = quantile(TX_boot0[i,],0.975)
  }
  
  acc_95 = length(which(quant_025 < 0 & quant_975>0 ))/nsam
  rej_95 = 1-acc_95
  rej_95
