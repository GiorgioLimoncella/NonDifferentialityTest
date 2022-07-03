rm(list=ls(all.names=TRUE))

### Setting the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir<-setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir<-setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(thisdir)

### Loading parameters
source(paste0(thisdir,"/01_Parameters/ProgramParameters.R"))

### Loading test statistic 
source(paste0(thisdir,"/02_TestFunctions/TestStatistic.R"))


################################################################################
###################                Simulation                ###################
################################################################################

prop_exp_list    <- c(0.2)
pi_ne_list       <- c(0.05)
risk_list        <- c(2)
SE_exposed_list  <- c(0.15, 0.30, 0.40, 0.50, 0.60, 0.70, 0.85)
sample_size_list <- c(250)

counter <- 0
len <- length(prop_exp_list)*
  length(pi_ne_list)*
  length(risk_list)*
  length(SE_exposed_list)*
  length(sample_size_list)

TestPower <- c()
combination <- c()
time.taken <- c()

DT_comb <- data.table(prop_exp = integer(0),
                      prev_ne = integer(0),
                      risk = integer(0),
                      SE_exp = integer(0),
                      power = integer(0),
                      sample_size = integer(0),
                      SE_AUB = integer(0),           
                      SE_A_e = integer(0),          
                      SE_B_given_A_e  = integer(0),  
                      SE_A_int_B_e = integer(0),       
                      SE_B_e = integer(0),             
                      SE_A_given_B_e = integer(0),     
                      SE_B_given_not_A_e = integer(0), 
                      SE_A_ne = integer(0),          
                      SE_B_given_A_ne = integer(0),  
                      SE_A_int_B_ne = integer(0),      
                      SE_B_ne = integer(0),            
                      SE_A_given_B_ne = integer(0),    
                      SE_B_given_not_A_ne = integer(0),
                      SP_A_e = integer(0),          
                      SP_B_e = integer(0),         
                      SP_A_ne = integer(0),         
                      SP_B_ne = integer(0))

for (h in prop_exp_list) {
  for (w in pi_ne_list) {
    for (t in risk_list) {
      for (k in SE_exposed_list) {
        for (z in sample_size_list) {
          start.time <- Sys.time()
          
          combination <- c(combination, paste0(h, "_", w, "_", t, "_", k))
          
          
          ### setting parameters
          source(paste0(thisdir,"/01_Parameters/TestParameters_hwtk_3.R"))
          
          ### Loading data
          source(paste0(thisdir,"/03_DataGen/ConditionalProbability.R"))
          
          ### Test 
          source(paste0(thisdir,"/04_TestApplication/TestPower_C_from_AB.R"))
          
          tmp <- data.table(prop_exp = h,
                            prev_ne = w,
                            risk = t,
                            SE_exp = k, 
                            power = rej_95,
                            sample_size = z,
                            SE_AUB = SE_AUB,           
                            SE_A_e = SE_A_e,          
                            SE_B_given_A_e  = SE_B_given_A_e,  
                            SE_A_int_B_e = SE_A_int_B_e,       
                            SE_B_e = SE_B_e,             
                            SE_A_given_B_e = SE_A_given_B_e ,     
                            SE_B_given_not_A_e = SE_B_given_not_A_e, 
                            SE_A_ne = SE_A_ne,          
                            SE_B_given_A_ne = SE_B_given_A_ne,  
                            SE_A_int_B_ne = SE_A_int_B_ne,      
                            SE_B_ne = SE_B_ne,            
                            SE_A_given_B_ne = SE_A_given_B_ne,    
                            SE_B_given_not_A_ne = SE_B_given_not_A_ne,
                            SP_A_e = SP_A_e,          
                            SP_B_e = SP_B_e,
                            
                            SP_A_ne = SP_A_ne,         
                            SP_B_ne = SP_B_ne)
          
          DT_comb <- rbind(DT_comb, tmp)
          
          TestPower <- c(TestPower, rej_95)
          end.time <- Sys.time()
          time.taken <- c(time.taken, (end.time - start.time))
          
          
          counter <- counter + 1
          print(paste0(counter, "/" , len))
        }

      }
      
    }
    
  }
  
}

fwrite(DT_comb, paste0(thisdir, "/05_Results/DT_comb_sample_250_C_from_AB_low_C.csv"))

Result <- data.table(Power = TestPower, combination = combination)
fwrite(Result, paste0(thisdir, "/05_Results/C_sample_C_from_AB_low_C.csv"))
