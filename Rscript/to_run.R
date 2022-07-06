################################################################################
###############    Non-Differentiality Test: Simulated Data     ################
################################################################################
rm(list=ls(all.names=TRUE))

#------------------------------
# Setting the working directory
#------------------------------
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir<-setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir<-setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(thisdir)

#---------------------------
# Loading program parameters
#---------------------------
source(paste0(thisdir,"/01_Parameters/ProgramParameters.R"))

#-----------------------------------------------
# Getting date and time, creating results folder
#-----------------------------------------------
now <- paste0(year(Sys.time()), 
              "-",
              month(Sys.time()),
              "-",
              day(Sys.time()), 
              "_", 
              hour(Sys.time()),
              "-",
              minute(Sys.time()))

dirresults <- paste0(thisdir, "/05_Results/", now)
suppressWarnings(if (!file.exists(dirresults)) dir.create(file.path(dirresults)))

#-----------------------
# Loading test statistic 
#-----------------------
source(paste0(thisdir,"/02_TestFunctions/TestStatistic.R"))

#-------------------------
# Defining data parameters
#-------------------------
prop_exp_list    <- c(0.05) #c(0.05, 0.2)
pi_ne_list       <- c(0.01) #c(0.01, 0.1)
risk_list        <- c(0.5) #c(0.5, 1.1, 2)
SE_exposed_list  <- c(0.5) #c(0.15, 0.30, 0.40, 0.50, 0.60, 0.70, 0.85)
sample_size_list <- c(500) #c(250, 500)
 
counter <- 0
len <- length(prop_exp_list)*
  length(pi_ne_list)*
  length(risk_list)*
  length(SE_exposed_list)*
  length(sample_size_list)

TestPower <- c()
combination <- c()

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


#-------------------
# Running simulation 
#-------------------
for (h in prop_exp_list) {
  for (w in pi_ne_list) {
    for (t in risk_list) {
      for (k in SE_exposed_list) {
        for (z in sample_size_list) {
          start_iteration <- Sys.time()
          combination <- c(combination, paste0(h, "_", w, "_", t, "_", k, "_", z))
          
          #-------------------
          # Setting parameters 
          #-------------------
          source(paste0(thisdir,"/01_Parameters/TestParameters_hwtkz.R"))
          
          #-------------
          # Loading data
          #-------------
          source(paste0(thisdir,"/03_DataGen/ConditionalProbability.R"))
          
          #---------------------
          # Computing test power 
          #---------------------
          source(paste0(thisdir,"/04_TestApplication/TestPower_with_C_sample_parlapply_exp.R"))
          
          #----------------------------------
          # Computing PPV and RR distribution 
          #----------------------------------
          source(paste0(thisdir,"/06_PPV/PPV_RR_distribution_parlapply.R"))
          
          #-------------------
          # Collecting results
          #-------------------
          tmp <- data.table(prop_exp = h,
                            prev_ne = w,
                            risk = t,
                            SE_exp = k, 
                            power = power_of_test,
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
          TestPower <- c(TestPower, power_of_test)
          counter <- counter + 1
          # fwrite(DT_PPV_RR, paste0(dirresults, 
          #                          "/DT_PPV_RR_",
          #                          combination[counter], 
          #                          ".csv"))
          end_iteration <- Sys.time()
          time_iteration <- end_iteration - start_iteration
          cat(paste0(counter, "/", len), ":  ")
          cat(time_iteration, "\n")
        }
      }
    }
  }
}

#---------------
# Saving results
#---------------
fwrite(DT_comb, paste0(dirresults, "/DT_combinations.csv"))
Result <- data.table(Power = TestPower, combination = combination)
fwrite(Result, paste0(dirresults, "/Results.csv"))
save.image(paste0(dirresults, "/env.RData"))
