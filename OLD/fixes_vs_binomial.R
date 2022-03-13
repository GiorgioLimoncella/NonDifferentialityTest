library(data.table)
## starting parameters
N = 10000; prop_e= 0.2
pi_ne = 0.05; risk = 2
pi_e = pi_ne*risk

## validation indices: exposed
SE_A_e=0.7
SE_B_e = 0.6
SP_A_e = 0.99
SP_B_e=0.95

## validation indices: non-exposed
SE_A_ne = 0.4
SE_B_ne = 0.6
SP_A_ne = 0.99
SP_B_ne = 0.95
E = c(rep(0,(1-prop_e)*N), rep(1,prop_e*N))

SE_fixed = c()
SE_binomial = c()

for (i in 1:100) {
  Y = ifelse(E==1, sapply(E,function(pe){rbinom(1,1,pi_e)}),
             sapply(E,function(pne){rbinom(1,1,pi_ne)}))
  
  dati = data.table(cbind(E,Y))
  
  
  start_time <- Sys.time()
  
  A = NA
  A = ifelse(Y==1 & E==1, apply(dati, 1, function(SEe){rbinom(1, 1, SE_A_e)}),
             ifelse(Y==1 & E==0, apply(dati, 1, function(SEne){rbinom(1, 1, SE_A_ne)}), 
                    ifelse(Y==0 & E==1, apply(dati, 1, function(FPe){rbinom(1, 1, 1-SP_A_e)}), 
                           ifelse(Y==0 & E==0, apply(dati, 1, function(FPne){rbinom(1, 1, 1-SP_A_ne)}), A))))
  
  B = NA
  B = ifelse(Y==1 & E==1, apply(dati, 1, function(SEe){rbinom(1, 1, SE_B_e)}),
             ifelse(Y==1 & E==0, apply(dati, 1, function(SEne){rbinom(1, 1, SE_B_ne)}), 
                    ifelse(Y==0 & E==1, apply(dati, 1, function(FPe){rbinom(1, 1, 1-SP_B_e)}), 
                           ifelse(Y==0 & E==0, apply(dati, 1, function(FPne){rbinom(1, 1, 1-SP_B_ne)}), B))))
  
  C = NA
  C = A*B
  
  SE_binomial = c(SE_binomial, sum(A==1&E==1&Y==1)/sum(E==1&Y==1))
  
}


for (i in 1:100) {
  ## Population: Non-Exposed
  N_non_exposed=(1-prop_e)*N
  E_0=rep(0, N_non_exposed)
  
  
  Y_0_ne=rep(0, N_non_exposed*(1-pi_ne))
  Y_1_ne=rep(1, N_non_exposed*pi_ne)
  Y_ne=c(Y_1_ne, Y_0_ne)
  
  ## Population: Exposed
  N_exposed=(prop_e)*N
  E_1=rep(1,N_exposed)
  Y_0_e=rep(0,N_exposed*(1-pi_e))
  Y_1_e=rep(1,N_exposed*pi_e)
  Y_e=c(Y_1_e, Y_0_e)
  
  ## Population: aggregated 
  Y=c(Y_e, Y_ne)
  if(length(Y)<N){
    diff=N-length(Y)
    Y=c(Y, rep(0, diff))
  }
  if(length(Y)>N){
    Y=Y[1:N]
  }
  E=c(E_1, E_0)
  if(length(E)<N){
    diff=N-length(E)
    E=c(E, rep(0, diff))
  }
  if(length(E)>N){
    E=E[1:N]
  }
  data=data.table(E,Y)
  data_E_Y = data[ ,.N, by=.(Y, E)][order(Y, E)]
  
  # A non-exposed
  A_TP_ne=rep(1, SE_A_ne*length(Y_1_ne))
  A_FN_ne=rep(0, (1-SE_A_ne)*length(Y_1_ne))
  A_FP_ne=rep(1, (1-SP_A_ne)*length(Y_0_ne))
  A_TN_ne=rep(0, (SP_A_ne)*length(Y_0_ne))
  
  A_ne=c(A_TP_ne, A_FN_ne,A_FP_ne, A_TN_ne)
  
  # B non-exposed
  
  B_TP_int_ne=rep(1, (SE_A_ne*SE_B_ne)*length(Y_1_ne))
  B_FN_ne=rep(0, (1-SE_B_ne)*length(Y_1_ne)) 
  B_TP_ne=rep(1, (SE_B_ne- (SE_A_ne*SE_B_ne))*length(Y_1_ne))
  
  B_FP_int_ne=rep(1, ((1-SP_A_ne)*(1-SP_B_ne))*length(Y_0_ne))
  B_TN_ne=rep(0, (1-(1-SP_B_ne))*length(Y_0_ne)) 
  B_FP_ne=rep(1, ((1-SP_B_ne) - ((1-SP_A_ne)*(1-SP_B_ne)))*length(Y_0_ne))
  
  B_ne=c(B_TP_int_ne, B_FN_ne, B_TP_ne, B_FP_int_ne, B_TN_ne, B_FP_ne)
  
  
  # A exposed
  A_TP_e=rep(1, SE_A_e*length(Y_1_e))
  A_FN_e=rep(0, (1-SE_A_e)*length(Y_1_e))
  A_FP_e=rep(1, (1-SP_A_e)*length(Y_0_e))
  A_TN_e=rep(0, (SP_A_e)*length(Y_0_e))
  
  A_e=c(A_TP_e, A_FN_e,A_FP_e, A_TN_e)
  
  # B exposed
  
  B_TP_int_e=rep(1, (SE_A_e*SE_B_e)*length(Y_1_e))
  B_FN_e=rep(0, (1-SE_B_e)*length(Y_1_e)) 
  B_TP_e=rep(1, (SE_B_e- (SE_A_e*SE_B_e))*length(Y_1_e))
  
  B_FP_int_e=rep(1, ((1-SP_A_e)*(1-SP_B_e))*length(Y_0_e))
  B_TN_e=rep(0, (1-(1-SP_B_e))*length(Y_0_e)) 
  B_FP_e=rep(1, ((1-SP_B_e) - ((1-SP_A_e)*(1-SP_B_e)))*length(Y_0_e))
  
  B_e=c(B_TP_int_e, B_FN_e, B_TP_e, B_FP_int_e, B_TN_e, B_FP_e)
  
  
  
  ## Algorithm: aggregated 
  A=c(A_e, A_ne)
  
  if(length(A)<N){
    diff=N-length(A)
    A=c(A, rep(0, diff))
  }
  
  if(length(A)>N){
    A=A[1:N]
  }
  
  B=c(B_e, B_ne)
  
  if(length(B)<N){
    diff=N-length(B)
    B=c(B, rep(0, diff))
  }
  
  if(length(A)>N){
    B=B[1:N]
  }
  
  ## Algorithm: C (intersection)
  C=ifelse(A==1&B==1, 1 , 0)
  
  dati=data.table(E, Y, A, B, C)
  
  SE_fixed = c(SE_binomial, dati[A==1&E==1&Y==1, .N]/ dati[E==1&Y==1, .N])
}
