rm(list=ls(all.names=TRUE))

### Setting the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir<-setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir<-setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(thisdir)

### Loading parameters
source(paste0(thisdir,"/01_Parameters/ProgramParameters.R"))

### Loading test statistic 
source(paste0(thisdir,"/02_TestStatistic/TestStatistic.R"))


################################################################################
###################                Simulation                ###################
################################################################################

prop_exp_list    <- c(0.01, 0.1)
pi_ne_list       <- c(0.01, 0.05, 0.1)
risk_list        <- c(0.5, 0.9, 1.1, 2)
SE_exposed_list  <- c(0.15, 0.30, 0.40, 0.50, 0.60, 0.70, 0.85)
sample_size_list <- c(250, 500)
counter <- 0


DT_sim_PPV <- data.table(PPV = integer(0) ,
                         algorithm = character(0), 
                         prop_exp = integer(0) ,
                         pi_ne = integer(0) ,
                         risk = integer(0) ,
                         SE_exposed = integer(0) ,
                         sample_size = integer(0) ,
                         combination  = integer(0))
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
  
          ### PPV
          source(paste0(thisdir,"/06_PPV/PPV_distribution.R"))
  
          DT_PPV <- DT_PPV[, prop_exp:= h]
          DT_PPV <- DT_PPV[, pi_ne:= w]
          DT_PPV <- DT_PPV[, risk:= t]
          DT_PPV <- DT_PPV[, SE_exposed:= k]
          DT_PPV <- DT_PPV[, sample_size := z]
          #DT_PPV <- DT_PPV[, combination:= paste0(h, "_", w, "_", t, "_", k)]
          DT_PPV <- DT_PPV[, combination:= paste0(h, "_", w, "_", t)]
  
          DT_sim_PPV <- rbind(DT_sim_PPV, DT_PPV)
  
          end.time <- Sys.time()
          time.taken <- c(time.taken, (end.time - start.time))
  
          counter <- counter + 1
          print(counter)
        }
      }
      
    }
    
  }
  
}

fwrite(DT_sim_PPV, paste0(thisdir, "/05_Results/DT_sim_PPV_v2.csv"))
