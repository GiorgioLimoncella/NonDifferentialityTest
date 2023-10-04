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

dirresults <- paste0(thisdir, "/10_Test_Application_Results_Known_POP/", now)
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

combination <- c()

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
            #stop()
            counter <- counter + 1
            cat(paste0(counter, " \n"))
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
            
            
            #------
            # test
            #-----
            n_validated_A_e <- z$a/2
            n_validated_B_e <- z$b/2
            
            n_validated_A_ne <- z$a/2
            n_validated_B_ne <- z$b/2
            
            if(SE_A_int_B_e == 0){
              n_validated_AintB_e <- 0
              n_validated_AintB_ne <- 0
            }else{
              n_validated_AintB_e  <- z$b/2
              n_validated_AintB_ne <- z$c/2
            }
            
            
            N_boot <- nboot
            source(paste0(thisdir,"/04_TestApplication/NonDiffTest_fromKnownPop.R"))
            
            
          }
        }
      }
    }
  }
}