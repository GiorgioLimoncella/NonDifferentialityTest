library(data.table)

## starting parameters
N = 20000; prop_exp= 0.2
pi_ne = 0.05; risk = 2
pi_e = pi_ne*risk

## validation indices: exposed
SE_A_e=0.7
SE_AintB_e = 0.1
SP_A_e = 0.99
SP_B_e=0.95

## validation indices: non-exposed
SE_A_ne=0.5
SE_AintB_ne = 0.1
SP_A_ne = 0.99
SP_B_ne=0.95

## Population: Non-Exposed
N_non_exposed=(1-prop_exp)*N
E_0=rep(0, N_non_exposed)


Y_0_ne=rep(0, N_non_exposed*(1-pi_ne))
Y_1_ne=rep(1,N_non_exposed*pi_ne)
Y_ne=c(Y_1_ne, Y_0_ne)

## Population: Exposed
N_exposed=(prop_exp)*N
E_1=rep(1,N_exposed)
Y_0_e=rep(0,N_exposed*(1-pi_e))
Y_1_e=rep(1,N_exposed*pi_e)
Y_e=c(Y_1_e, Y_0_e)

## Population: aggregated 
Y=c(Y_e, Y_ne)
E=c(E_1, E_0)
data=data.table(E,Y)
data_E_Y = data[ ,.N, by=.(Y, E)][order(Y, E)]


## Algorithm: A non-exposed
A_TP_ne=rep(1, SE_A_ne*length(Y_1_ne))
A_FN_ne=rep(0, (1-SE_A_ne)*length(Y_1_ne))
A_TN_ne=rep(0, (SP_A_ne)*length(Y_0_ne))
A_FP_ne=rep(1, (1-SP_A_ne)*length(Y_0_ne))

A_ne=c(A_TP_ne, A_FN_ne, A_TN_ne, A_FP_ne)

## Algorithm: B non-exposed
B_TP_int_ne=rep(1, SE_AintB_ne*length(Y_1_ne))
B_FN_ne=rep(0, (SE_A_ne-SE_AintB_ne)*length(Y_1_ne)) # B_FN_e=rep(0, (1-SE_AintB_ne-(1-SE_A_ne))*length(Y_1_ne))
B_TP_ne=rep(1, (1-SE_A_ne)*length(Y_1_ne))
B_TN_ne=rep(0, (SP_B_ne)*length(Y_0_ne))
B_FP_ne=rep(1, (1-SP_B_ne)*length(Y_0_ne))

B_ne=c(B_TP_int_ne, B_FN_ne, B_TP_ne, B_TN_ne, B_FP_ne)

## Algorithm: A exposed
A_TP_e=rep(1, SE_A_e*length(Y_1_e))
A_FN_e=rep(0, (1-SE_A_e)*length(Y_1_e))
A_TN_e=rep(0, (SP_A_e)*length(Y_0_e))
A_FP_e=rep(1, (1-SP_A_e)*length(Y_0_e))

A_e=c(A_TP_e, A_FN_e, A_TN_e, A_FP_e)

## Algorithm: B exposed
B_TP_int_e=rep(1, SE_AintB_e*length(Y_1_e))
B_FN_e=rep(0, (SE_A_e-SE_AintB_e)*length(Y_1_e)) # B_FN_e=rep(0, (1-SE_AintB_ne-(1-SE_A_ne))*length(Y_1_ne))
B_TP_e=rep(1, (1-SE_A_e)*length(Y_1_e))
B_TN_e=rep(0, (SP_B_e)*length(Y_0_e))
B_FP_e=rep(1, (1-SP_B_e)*length(Y_0_e))

B_e=c(B_TP_int_e, B_FN_e, B_TP_e, B_TN_e, B_FP_e)

## Algorithm: aggregated 
A=c(A_e, A_ne)
B=c(B_e, B_ne)

## Algorithm: C (intersection)
C=ifelse(A==1&B==1, 1 , 0)

data=data.table(E, Y, A, B, C)
data_aggregated = data[, .N, by=.(Y, E, A, B, C)][order(Y, E, A, B, C)]
data_aggregated_E0 = data[E==0, .N, by=.(Y, E, A, B, C)][order(Y, E, A, B, C)]
data_aggregated_E1 = data[E==1, .N, by=.(Y, E, A, B, C)][order(Y, E, A, B, C)]
data_aggregated_E1[, sum(N)]
