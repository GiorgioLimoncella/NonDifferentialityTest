## Population: Non-Exposed
N_non_exposed <- (1-prop_exp)*N
E_0           <- rep(0, N_non_exposed)

Y_0_ne <- rep(0, N_non_exposed*(1-pi_ne))
Y_1_ne <- rep(1,N_non_exposed*pi_ne)
Y_ne   <- c(Y_1_ne, Y_0_ne)

## Population: Exposed
N_exposed <- (prop_exp)*N
E_1       <- rep(1,N_exposed)

Y_0_e <- rep(0,N_exposed*(1-pi_e))
Y_1_e <- rep(1,N_exposed*pi_e)
Y_e   <- c(Y_1_e, Y_0_e)

## Population: aggregated 
Y <- c(Y_e, Y_ne)
E <- c(E_1, E_0)

data     <- data.table(E,Y)
data_E_Y <- data[ ,.N, by=.(Y, E)][order(Y, E)]




###########
###  A  ###
###########


## Algorithm: A | E==0 & Y==1
data[Y==1 & E==0, i:= seq_along(1:.N)]
max_i <- max(data[Y==1 & E==0, i])
data[Y==1 & E==0 & i<(SE_A_ne * max_i), A:= 1]
data[Y==1 & E==0 & is.na(A), A:= 0]

data <- data[, -c("i")]

## Algorithm: A | E==1 & Y==1
data[Y==1 & E==1, i:= seq_along(1:.N)]
max_i <- max(data[Y==1 & E==1, i])
data[Y==1 & E==1 & i<(SE_A_e * max_i), A:= 1]
data[Y==1 & E==1 & is.na(A), A:= 0]

data <- data[, -c("i")]

## Algorithm: A | E==0 & Y==0
data[Y==0 & E==0, i:= seq_along(1:.N)]
max_i <- max(data[Y==0 & E==0, i])
data[Y==0 & E==0 & i<((1-SP_A_ne) * max_i), A:= 1]
data[Y==0 & E==0 & is.na(A), A:= 0]

data <- data[, -c("i")]

## Algorithm: A | E==1 & Y==0
data[Y==0 & E==1, i:= seq_along(1:.N)]
max_i <- max(data[Y==0 & E==1, i])
data[Y==0 & E==1 & i<((1-SP_A_e) * max_i), A:= 1]
data[Y==0 & E==1 & is.na(A), A:= 0]

data <- data[, -c("i")]


data_E_Y_A <- data[ ,.N, by=.(Y, E, A)][order(Y, E, A)]



###########
###  B  ###
###########

## Algorithm: B | E==0 & Y==1 & A==1
data[Y==1 & E==0 & A==1, i:= seq_along(1:.N)]
max_i <- max(data[Y==1 & E==0 & A==1, i])
data[Y==1 & E==0 & A==1 & i<(SE_B_given_A_ne * max_i), B:= 1]
data[Y==1 & E==0 & A==1 & is.na(B), B:= 0]

data <- data[, -c("i")]

## Algorithm: B | E==0 & Y==1 & A==0
data[Y==1 & E==0 & A==0, i:= seq_along(1:.N)]
max_i <- max(data[Y==1 & E==0 & A==0, i])
data[Y==1 & E==0 & A==0 & i<(SE_B_given_not_A_ne * max_i), B:= 1]
data[Y==1 & E==0 & A==0 & is.na(B), B:= 0]

data <- data[, -c("i")]

## Algorithm: B | E==1 & Y==1 & A==1
data[Y==1 & E==1 & A==1, i:= seq_along(1:.N)]
max_i <- max(data[Y==1 & E==1 & A==1, i])
data[Y==1 & E==1 & A==1 & i<(SE_B_given_A_e * max_i), B:= 1]
data[Y==1 & E==1 & A==1 & is.na(B), B:= 0]

data <- data[, -c("i")]

## Algorithm: B | E==1 & Y==1 & A==0
data[Y==1 & E==1 & A==0, i:= seq_along(1:.N)]
max_i <- max(data[Y==1 & E==1 & A==0, i])
data[Y==1 & E==1 & A==0 & i<(SE_B_given_not_A_e * max_i), B:= 1]
data[Y==1 & E==1 & A==0 & is.na(B), B:= 0]

data <- data[, -c("i")]




## Algorithm: B | E==0 & Y==0 & A==0
data[E==0 & Y==0 & A==0, i:= seq_along(1:.N)]
max_i <- max(data[E==0 & Y==0 & A==0, i])
data[E==0 & Y==0 & A==0 & i<((1-SP_B_ne) * max_i), B:= 1]
data[E==0 & Y==0 & A==0 & is.na(B), B:= 0]

data <- data[, -c("i")]

## Algorithm: B | E==0 & Y==0 & A==1
data[E==0 & Y==0 & A==1, i:= seq_along(1:.N)]
max_i <- max(data[E==0 & Y==0 & A==1, i])
data[E==0 & Y==0 & A==1 & i<((1-SP_B_ne) * max_i), B:= 1]
data[E==0 & Y==0 & A==1 & is.na(B), B:= 0]

data <- data[, -c("i")]


## Algorithm: B | E==1 & Y==0 & A==0
data[E==1 & Y==0 & A==0, i:= seq_along(1:.N)]
max_i <- max(data[E==1 & Y==0 & A==0, i])
data[E==1 & Y==0 & A==0 & i<((1-SP_B_ne) * max_i), B:= 1]
data[E==1 & Y==0 & A==0 & is.na(B), B:= 0]

data <- data[, -c("i")]

## Algorithm: B | E==1 & Y==0 & A==1
data[E==1 & Y==0 & A==1, i:= seq_along(1:.N)]
max_i <- max(data[E==1 & Y==0 & A==1, i])
data[E==1 & Y==0 & A==1 & i<((1-SP_B_ne) * max_i), B:= 1]
data[E==1 & Y==0 & A==1 & is.na(B), B:= 0]

data <- data[, -c("i")]

data_E_Y_A_B <- data[ ,.N, by=.(Y, E, A, B)][order(Y, E, A, B)]


## Algorithm: C
data <- data[, C:= A*B]

## shuffle 
data[, x:= sample(1:nrow(data), nrow(data), replace = F)]
data <- data[order(x)]
data <- data[, -c("x")]

################################################################################
### Validation indices check
# SE_A_e_post <- data[Y==1 & A==1 & E==1, .N]/data[Y==1 & E==1, .N]
# SE_A_ne_post <- data[Y==1 & A==1 & E==0, .N]/data[Y==1 & E==0, .N]
# 
# SE_B_e_post <- data[Y==1 & B==1 & E==1, .N]/data[Y==1 & E==1, .N]
# SE_B_ne_post <- data[Y==1 & B==1 & E==0, .N]/data[Y==1 & E==0, .N]
# 
# SP_A_e_post <- data[Y==0 & A==0 & E==1, .N]/data[Y==0 & E==1, .N]
# SP_A_ne_post <- data[Y==0 & A==0 & E==0, .N]/data[Y==0 & E==0, .N]
# 
# SP_B_e_post <- data[Y==0 & B==0 & E==1, .N]/data[Y==0 & E==1, .N]
# SP_B_ne_post <- data[Y==0 & B==0 & E==0, .N]/data[Y==0 & E==0, .N]



#-------------------
# Prevalences & Data
#-------------------
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

# sample size A
data_aggregated_A <- data[, .N, .(A, E)]

na_e <- min(as.integer(na/2), data_aggregated_A[E==1 & A==1, N])
na_ne <- min(as.integer(na/2), data_aggregated_A[E==0 & A==1, N])
if (na_e < as.integer(na/2) & na_ne == as.integer(na/2) ) {
  na_ne <- min(na - na_e, data_aggregated_A[E==0 & A==1, N])
}
if (na_e == as.integer(na/2) & na_ne < as.integer(na/2) ) {
  na_e <- min(na - na_ne, data_aggregated_A[E==1 & A==1, N])
}

# sample size B
data_aggregated_B <- data[, .N, .(B, E)]

nb_e <- min(as.integer(nb/2), data_aggregated_B[E==1 & B==1, N])
nb_ne <- min(as.integer(nb/2), data_aggregated_B[E==0 & B==1, N])
if (nb_e < as.integer(nb/2) & nb_ne == as.integer(nb/2) ) {
  nb_ne <- min(nb - nb_e, data_aggregated_B[E==0 & B==1, N])
}
if (nb_e == as.integer(nb/2) & nb_ne < as.integer(nb/2) ) {
  nb_e <- min(nb - nb_ne, data_aggregated_B[E==1 & B==1, N])
}

# sample size C
data_aggregated_C <- data[, .N, .(C, E)]

nc_e <- min(as.integer(nc/2), data_aggregated_C[E==1 & C==1, N])
nc_ne <- min(as.integer(nc/2), data_aggregated_C[E==0 & C==1, N])
if (nc_e < as.integer(nc/2) & nc_ne == as.integer(nc/2) ) {
  nc_ne <- min(nc - nc_e, data_aggregated_C[E==0 & C==1, N])
}
if (nc_e == as.integer(nc/2) & nc_ne < as.integer(nc/2) ) {
  nc_e <- min(nc - nc_ne, data_aggregated_C[E==1 & C==1, N])
}
