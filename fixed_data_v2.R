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
