#--------------------------------------
# Non-Differentiality Test: HPC- script
#--------------------------------------
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

#-------------------------------------
# Loading test statistic & RR formulas
#-------------------------------------
source(paste0(thisdir,"/02_TestFunctions/TestStatistic.R"))
source(paste0(thisdir,"/02_TestFunctions/RiskRatio_estimator.R"))


#-------------------------
# Defining data parameters
#-------------------------
source(paste0(thisdir,"/01_Parameters/TestParameters.R"))

sensitivity_list <- list()

for (sr in se_ratio_list) {
  sublist <- list(e = sr * SE_ne_scalar, ne = SE_ne_scalar)
  sensitivity_list <- append(sensitivity_list, list(sublist))
}

#--------------------
# Setting the cluster
#--------------------
setDTthreads(n_of_core_to_be_used)

#--------------------------------
# Creating objects for simulation
#--------------------------------
counter <- 0
len <- length(prop_exp_list)*
  length(pi_ne_list)*
  length(risk_list)*
  length(sensitivity_list)*
  length(sample_size_list)*
  length(SE_A_int_B_list)

TestPower <- c()
combination <- c()

DT_comb <- data.table(prop_exp = integer(0),
                      prev_ne = integer(0),
                      risk = integer(0),
                      SE_ratio = integer(0),
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
cat("Iteration:    Power    (time elapsed)    \n")

for (h in prop_exp_list) {
  for (w in pi_ne_list) {
    for (t in risk_list) {
      for (k in sensitivity_list) {
        for (z in sample_size_list) {
          for (s in SE_A_int_B_list) {
            
            if(counter == 0){
              cat("Iteration:    Power    (time elapsed)    \n")
            }
            
            start_iteration <- Sys.time()
            combination <- c(combination, paste0(h, "_", 
                                                 w, "_", 
                                                 t, "_", 
                                                 k$e, "_", k$ne, "_", 
                                                 z$a, "_", z$b, "_", z$c, "_",
                                                 s))
            
            #-------------------
            # Setting parameters 
            #-------------------
            source(paste0(thisdir,"/01_Parameters/IterativeParameters.R"))
            
            #-------------
            # Loading data
            #-------------
            source(paste0(thisdir,"/03_DataGen/ConditionalProbability_sample_strata_exp.R"))
            
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
                              SE_ratio = round(k$e / k$ne, digits = 2),
                              power = power_of_test,
                              sample_size = paste0(z$a, "_", z$b, "_", z$c),
                              SE_AUB = SE_AUB,           
                              SE_A_e = k$e,          
                              SE_B_given_A_e  = SE_B_given_A_e,  
                              SE_A_int_B_e = SE_A_int_B_e,       
                              SE_B_e = SE_B_e,             
                              SE_A_given_B_e = SE_A_given_B_e ,     
                              SE_B_given_not_A_e = SE_B_given_not_A_e, 
                              SE_A_ne = k$ne,          
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
            fwrite(DT_PPV_RR, paste0(dirresults,
                                     "/DT_PPV_RR_",
                                     combination[counter],
                                     ".csv"))
            end_iteration <- Sys.time()
            time_iteration <- end_iteration - start_iteration
            if((counter + 1) %% 10 == 0){
              cat(" Iteration:  Power (time elapsed)    \n")
            }
            
            cat(paste0(counter, "/", len), "    :  ", power_of_test, " ( ")
            cat(time_iteration, ") \n")
            
            if(counter %% 10 == 0){
              Result <- data.table(Power = TestPower, combination = combination)
              
              fwrite(Result, paste0(dirresults, "/Results.csv"))
            }
          }
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
if (length(TestPower) != length(combination)) {
  warning("Result: length(TestPower) != length(combination)")
}
fwrite(Result, paste0(dirresults, "/Results.csv"))