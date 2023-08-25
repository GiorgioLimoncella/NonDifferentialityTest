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

#--------------------
# Setting the cluster
#--------------------
n_of_core_to_be_used <- 30
setDTthreads(n_of_core_to_be_used)

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
prop_exp_list    <- c(0.05, 0.2)                                     
pi_ne_list       <- c(0.01, 0.05, 0.1)                                     
risk_list        <- c(1.2, 2)  

# sensitivity_list <-  list(list( e = 0.2858, ne = 0.7142),       # 0.4        
#                           list( e = 0.375,  ne = 0.625),        # 0.6
#                           list( e = 0.4445, ne = 0.5555),       # 0.8      
#                           list( e = 0.5,    ne = 0.5),          # 1
#                           list( e = 0.545,  ne = 0.454),        # 1.2
#                           list( e = 0.5833, ne = 0.4166),       # 1.4
#                           list( e = 0.6155, ne = 0.3845))       # 1.6  


sensitivity_list <-  list(#list(e = 0.2, ne = 0.5),       # 0.4        
                          list(e = 0.3, ne = 0.5),       # 0.6
                          list(e = 0.4, ne = 0.5),       # 0.8      
                          list(e = 0.5, ne = 0.5),       # 1
                          list(e = 0.6, ne = 0.5),       # 1.2
                          list(e = 0.7, ne = 0.5))#,       # 1.4
                          #list(e = 0.8, ne = 0.5))       # 1.6  


P_A_int_B_list <- c(0, 0.2, 0.4)

sample_size_list <-  list(list(a = 100, b = 100, c = 50), 
                          list(a = 200, b = 200, c = 100),
                          list(a = 300, b = 300, c = 150))                                     

counter <- 0
len <- length(prop_exp_list)*
  length(pi_ne_list)*
  length(risk_list)*
  length(sensitivity_list)*
  length(sample_size_list)*
  length(P_A_int_B_list)

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
          for (s in P_A_int_B_list) {
            
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
            source(paste0(thisdir,"/01_Parameters/TestParameters_hwtkz.R"))
            
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
            #stop("stop")
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
#save.image(paste0(dirresults, "/env.RData"))


#---------------

#Note 

# invece di fissare SE di B|A:
# 1. set SE_ AUB
# 2. imposta SE di A e SE_ A int B
# 3. ricava SE di B

# metti in chiaro che le SP sono indipendenti dato Y (per semplificare)


