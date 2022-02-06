###  Analysis of the variability of the test power
#    7 february 2022


rm(list=ls())
library(data.table)


### Parameters that affect variability  
proportion_of_exposed = c(0.05, 0.2)  #prop_e
prevalence_non_exposed = c(0.05, 0.2) #pi_ne 
risk_vector = c(0.5, 2)  #risk 

sensitivity_A_exposed <- c(0.4, 0.5, 0.6)


sensitivity <- list(list(A_exposed = 0.4, A_non_exposed = 0.5, B_exposed = 0.6, B_non_exposed = 0.52),
                    list(A_exposed = 0.5, A_non_exposed = 0.5, B_exposed = 0.6, B_non_exposed = 0.6),
                    list(A_exposed = 0.6, A_non_exposed = 0.5, B_exposed = 0.6, B_non_exposed = 0.68))

Data_generation <- c("fixed") #, "binomial")
 
### Fixed Parameters 
N = 20000
sample_size = 250

SE_A_ne = 0.5

SP_A_e = 0.99
SP_B_e = 0.95
SP_A_ne = 0.99
SP_B_ne = 0.95

compination <- data.table(n = integer(0),
                          proportion_e = character(0),
                          prevalence_ne = character(0),
                          sensitivity_B_e = character(0),
                          sensitivity_A_ne = character(0),
                          sensitivity_B_ne = character(0),
                          col_risk = character(0),
                          data_gen  = character(0))

power_list <- vector(mode = "list")

pippo <- 0

n_iter <- length(proportion_of_exposed)*length(prevalence_non_exposed)*length(risk_vector)*length(Data_generation)
for (prop_e in proportion_of_exposed) {
  for (pi_ne in prevalence_non_exposed) {
    for (risk in risk_vector) {
      for (d_gen in Data_generation) {
        
        power_vector <- c()
        pippo <- pippo + 1
        pluto <- 0
        cat(paste0(pippo, "/", as.character(n_iter), ":  "))
        
        pi_e = pi_ne*risk
        
        for (SE in sensitivity) {
          SE_A_e = SE$A_exposed
          SE_A_ne = SE$A_non_exposed
          SE_B_e = SE$B_exposed
          SE_B_ne = SE$B_non_exposed
          
          compination <- rbind(compination,
                               data.table(n = pippo,
                                          proportion_e = prop_e,
                                          prevalence_ne = pi_ne,
                                          sensitivity_B_e = SE_B_e,
                                          sensitivity_A_ne = SE_A_ne,
                                          sensitivity_B_ne = SE_B_ne,
                                          col_risk = risk,
                                          data_gen  = d_gen))
          
          if (d_gen == "fixed") {
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
          }
          if (d_gen == "binomial") {
            
            # Data generation
            E = c(rep(0,(1-prop_e)*N), rep(1,prop_e*N))
            
            Y = ifelse(E==1, sapply(E,function(pe){rbinom(1,1,pi_e)}),
                       sapply(E,function(pne){rbinom(1,1,pi_ne)}))
            
            dati = data.table(cbind(E,Y))
            
            dati <- dati[Y==1 & E==1, A:=rbinom(dati[Y==1 & E==1, .N], 1, SE_A_e)]
            dati <- dati[Y==1 & E==0, A:=rbinom(dati[Y==1 & E==0, .N], 1, SE_A_ne)]
            dati <- dati[Y==0 & E==1, A:=rbinom(dati[Y==0 & E==1, .N], 1, 1-SP_A_e)]
            dati <- dati[Y==0 & E==0, A:=rbinom(dati[Y==0 & E==0, .N], 1, 1-SP_A_ne)]
            
            dati <- dati[Y==1 & E==1, B:=rbinom(dati[Y==1 & E==1, .N], 1, SE_B_e)]
            dati <- dati[Y==1 & E==0, B:=rbinom(dati[Y==1 & E==0, .N], 1, SE_B_ne)]
            dati <- dati[Y==0 & E==1, B:=rbinom(dati[Y==0 & E==1, .N], 1, 1-SP_B_e)]
            dati <- dati[Y==0 & E==0, B:=rbinom(dati[Y==0 & E==0, .N], 1, 1-SP_B_ne)]
            
            dati <- dati[, C := A*B]
          }
          
          ################################################################################
          ###########################      Bootstrap      ################################
          ################################################################################
          
          # Sets A==1 and B==1
          n=nb= sample_size
          nsam = 1000
          
          P_A_e = dati[A==1&E==1, .N]/dati[E==1, .N]
          P_A_ne = dati[A==1&E==0, .N]/dati[E==0, .N]
          
          P_B_e = dati[B==1&E==1, .N]/dati[E==1, .N]
          P_B_ne = dati[B==1&E==0, .N]/dati[E==0, .N]
          
          P_C_e = dati[C==1&E==1, .N]/dati[E==1, .N]
          P_C_ne = dati[C==1&E==0, .N]/dati[E==0, .N]
          
          P_A =  dati[A==1, .N]/N
          P_B = dati[B==1, .N]/N
          P_C = dati[C==1, .N]/N
          
          
          c1 = P_B_e/P_A_e; c2 = P_B_ne/P_A_ne ; c3 = P_C_e/P_A_e ; c4 = P_C_ne/P_A_ne
          
          #matrix_A = as.data.frame(cbind(Y[A==1],A[A==1],E[A==1]))
          #matrix_B = as.data.frame(cbind(Y[B==1],B[B==1],E[B==1]))
          #colnames(matrix_A)=c("Y","A","E")
          #colnames(matrix_B)=c("Y","B","E")
          
          DT_A <- dati[A==1]
          DT_B <- dati[B==1]
          
          
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
            
            sam_Ya1 = DT_A[sample(nrow(DT_A),n),]
            sam_Yb1 = DT_B[sample(nrow(DT_B),nb),]
            
            for(j in 1:nboot){
              
              sam_Ya_boot = sam_Ya1[sample(nrow(sam_Ya1), n, replace = TRUE),]
              sam_Yb_boot = sam_Yb1[sample(nrow(sam_Yb1), nb, replace = TRUE),]
              
              # PPV_A_e0[i,j] = sam_Ya_boot[A==1 & Y==1 & E==1, .N] / sam_Ya_boot[A==1 & E==1, .N] 
              # PPV_B_e0[i,j] = sam_Yb_boot[B==1 & Y==1 & E==1, .N] / sam_Yb_boot[B==1 & E==1, .N] 
              # 
              # PPV_A_ne0[i,j] = sam_Ya_boot[A==1 & Y==1 & E==0, .N] / sam_Ya_boot[A==1 & E==0, .N] 
              # PPV_B_ne0[i,j] = sam_Yb_boot[B==1 & Y==1 & E==0, .N] / sam_Yb_boot[B==1 & E==0, .N] 
              
              PPV_A_e0[i,j] = (sum(sam_Ya_boot$A==1&sam_Ya_boot$Y==1&sam_Ya_boot$E==1))/(sum(sam_Ya_boot$A==1&sam_Ya_boot$E==1))
              PPV_B_e0[i,j] = (sum(sam_Yb_boot$B==1&sam_Yb_boot$Y==1&sam_Yb_boot$E==1))/(sum(sam_Yb_boot$B==1&sam_Yb_boot$E==1))
              PPV_A_ne0[i,j] = (sum(sam_Ya_boot$A==1&sam_Ya_boot$Y==1&sam_Ya_boot$E==0))/(sum(sam_Ya_boot$A==1&sam_Ya_boot$E==0))
              PPV_B_ne0[i,j] = (sum(sam_Yb_boot$B==1&sam_Yb_boot$Y==1&sam_Yb_boot$E==0))/(sum(sam_Yb_boot$B==1&sam_Yb_boot$E==0))
              
              TA_e <- P_A_e * PPV_A_e0[i,j]
              TA_ne <- P_A_ne * PPV_A_ne0[i,j]
              TB_e <- P_B_e * PPV_B_e0[i,j]
              TB_ne <- P_B_ne * PPV_B_ne0[i,j]
              TC_e <- P_C_e * max(PPV_A_e0[i,j], PPV_B_e0[i,j])
              TC_ne <- P_C_ne * max(PPV_A_ne0[i,j], PPV_B_ne0[i,j])
              
              
              TX_boot0[i,j] = (TA_e * (TA_ne + TB_ne - TC_ne) / TA_ne * (TA_e + TB_e - TC_e)) -1
              
            }
          }
          
          
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
              
              TA_e <- P_A_e * PPV_A_e2[i,j]
              TA_ne <- P_A_ne * PPV_A_ne2[i,j]
              TB_e <- P_B_e * PPV_B_e2[i,j]
              TB_ne <- P_B_ne * PPV_B_ne2[i,j]
              TC_e <- P_C_e * max(PPV_A_e2[i,j], PPV_B_e0[i,j])
              TC_ne <- P_C_ne * max(PPV_A_ne2[i,j], PPV_B_ne0[i,j])
              
              
              TX_boot2[i,j] = ((TA_e * (TA_ne + TB_ne - TC_ne)) / (TA_ne * (TA_e + TB_e - TC_e))) -1
              
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
          
          pluto <- pluto+1
          cat(pluto, "...   ")
        }
        cat( "\n")
        power_list[[as.character(pippo)]] <- power_vector
        
      }
      
    }
    
  }
  
}

