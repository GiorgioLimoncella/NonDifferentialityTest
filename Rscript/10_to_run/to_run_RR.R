rm(list=ls(all.names=TRUE))

### Setting the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir<-setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir<-setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(thisdir)

### Loading parameters
source(paste0(thisdir,"/01_Parameters/ProgramParameters.R"))

DT_RR_1 <- fread(paste0(thisdir, "/05_Results/DT_sim_PPV_v2.csv"))

DT_RR_1 <- DT_RR_1[, identifier := rep( seq(1:1000), 2016)]
DT_RR_1 <- dcast(DT_RR_1, risk + pi_ne + prop_exp + SE_exposed + sample_size + identifier ~ algorithm, value.var = "PPV")


################################################################################
###################                Simulation                ###################
################################################################################

prop_exp_list    <- c(0.01, 0.1)
pi_ne_list       <- c(0.01, 0.05, 0.1)
risk_list        <- c(0.5, 0.9, 1.1, 2)
SE_exposed_list  <- c(0.15, 0.30, 0.40, 0.50, 0.60, 0.70, 0.85)
sample_size_list <- c(250, 500)
counter <- 0


DT_RR <- data.table(PPV = integer(0) ,
                    algorithm = character(0), 
                    prop_exp = integer(0) ,
                    pi_ne = integer(0) ,
                    risk = integer(0) ,
                    SE_exposed = integer(0) ,
                    sample_size = integer(0) ,
                    combination  = integer(0),
                    P_A_e = integer(0),
                    P_A_ne = integer(0),
                    P_B_e = integer(0),
                    P_B_ne = integer(0),
                    P_C_e = integer(0),
                    P_C_ne = integer(0),
                    P_A = integer(0),
                    P_B = integer(0),
                    P_C = integer(0))

DT_RR_tmp <- data.table(PPV = 0,
                    algorithm = "0", 
                    prop_exp = 0,
                    pi_ne = 0,
                    risk = 0,
                    SE_exposed = 0,
                    sample_size = 0,
                    combination  = 0,
                    P_A_e = 0,
                    P_A_ne = 0,
                    P_B_e = 0,
                    P_B_ne = 0,
                    P_C_e = 0,
                    P_C_ne = 0,
                    P_A = 0,
                    P_B = 0,
                    P_C = 0)

time.taken <- c()


for (h in prop_exp_list) {
  for (w in pi_ne_list) {
    for (t in risk_list) {
      for (k in SE_exposed_list) {
        for (z in sample_size_list) {
          start.time <- Sys.time()
          
          #combination <- c(combination, paste0(h, "_", w, "_", t, "_", k))
          #combi<- c(combination, paste0(h, "_", w, "_", t))
          
          ### setting parameters
          source(paste0(thisdir,"/01_Parameters/TestParameters_hwtk.R"))
          
          ### Loading data
          source(paste0(thisdir,"/03_DataGen/ConditionalProbability.R"))
          
          P_A_e_tmp = data[A==1&E==1, .N]/data[E==1, .N]
          P_A_ne_tmp = data[A==1&E==0, .N]/data[E==0, .N]
          
          P_B_e_tmp = data[B==1&E==1, .N]/data[E==1, .N]
          P_B_ne_tmp = data[B==1&E==0, .N]/data[E==0, .N]
          
          P_C_e_tmp = data[C==1&E==1, .N]/data[E==1, .N]
          P_C_ne_tmp = data[C==1&E==0, .N]/data[E==0, .N]
          
          P_A_tmp =  data[A==1, .N]/N
          P_B_tmp = data[B==1, .N]/N
          P_C_tmp = data[C==1, .N]/N
          

          
          DT_RR_tmp <- DT_RR_tmp[, prop_exp:= h]
          DT_RR_tmp <- DT_RR_tmp[, pi_ne:= w]
          DT_RR_tmp <- DT_RR_tmp[, risk:= t]
          DT_RR_tmp <- DT_RR_tmp[, SE_exposed:= k]
          DT_RR_tmp <- DT_RR_tmp[, sample_size := z]
          #DT_RR_tmp <- DT_RR_tmp[, combination:= paste0(h, "_", w, "_", t, "_", k)]
          DT_RR_tmp <- DT_RR_tmp[, combination:= paste0(h, "_", w, "_", t)]
          DT_RR_tmp <- DT_RR_tmp[,P_A_e := P_A_e_tmp]
          DT_RR_tmp <- DT_RR_tmp[,P_A_ne := P_A_ne_tmp]
          DT_RR_tmp <- DT_RR_tmp[,P_B_e := P_B_e_tmp]
          DT_RR_tmp <- DT_RR_tmp[,P_B_ne := P_B_ne_tmp]
          DT_RR_tmp <- DT_RR_tmp[,P_C_e := P_C_e_tmp]
          DT_RR_tmp <- DT_RR_tmp[,P_C_ne := P_C_ne_tmp]
          DT_RR_tmp <- DT_RR_tmp[,P_A := P_A_tmp]
          DT_RR_tmp <- DT_RR_tmp[,P_B := P_B_tmp]
          DT_RR_tmp <- DT_RR_tmp[,P_C := P_C_tmp]
          
          DT_RR <- rbind(DT_RR, DT_RR_tmp)
          
          end.time <- Sys.time()
          time.taken <- c(time.taken, (end.time - start.time))
          
          counter <- counter + 1
          print(counter)
        }
      }
      
    }
    
  }
  
}

DT_RR_final <- merge(DT_RR_1,
               DT_RR,
               all.x = TRUE,
               by = c("risk", "pi_ne", "prop_exp", "SE_exposed", "sample_size"))

DT_RR_final <- DT_RR_final[, RR := (P_A_e*A_e + P_B_e*B_e + P_C_e*C_e) / (P_A_ne*A_ne + P_B_ne*B_ne + P_C_ne*C_ne)]

fwrite(DT_RR_final, paste0(thisdir, "/05_Results/DT_RR_final.csv"))
