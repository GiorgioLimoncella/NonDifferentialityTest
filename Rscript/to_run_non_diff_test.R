#--------------------------------------
# Non-Differentiality Test: Application
#--------------------------------------
rm(list=ls(all.names=TRUE))

#------------------------------
# Setting the working directory
#------------------------------
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir<-setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir<-setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(thisdir)

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

dirresults <- paste0(thisdir, "/09_Test_Application_Results/", now)
suppressWarnings(if (!file.exists(dirresults)) dir.create(file.path(dirresults)))

#-----------
# Parameters
#-----------
# Positive predictive value of A, B and A intersected B in the exposed group 
PPV_A_e      <- 0.9
PPV_B_e      <- 0.5
PPV_AintB_e  <- 0.99

# Positive predictive value of A, B and A intersected B in the non-exposed group 
PPV_A_ne      <- 0.2
PPV_B_ne      <- 0.4
PPV_AintB_ne  <- 0.95

# Number of cases validate of A, B and A intersected B in the exposed group 
n_validated_A_e     <- 100
n_validated_B_e     <- 100
n_validated_AintB_e <- 50

# Number of cases validate of A, B and A intersected B in the non-exposed group 
n_validated_A_ne     <- 100
n_validated_B_ne     <- 100
n_validated_AintB_ne <- 50

# Observed prevalence of A, B and A intersected B in the exposed group 
P_A_e        <- 0.02
P_B_e        <- 0.04
P_AintB_e    <- 0.002

# Observed prevalence of A, B and A intersected B in the non-exposed group 
P_A_ne        <- 0.01
P_B_ne        <- 0.02
P_AintB_ne    <- 0.001

# Number of bootstrap sample
N_boot <- 500

# significance level of the test
alpha <- 0.05

#------------------
# Loading functions
#------------------
source(paste0(thisdir,"/02_TestFunctions/TestStatistic.R"))

#-----------------
# Test Application
#-----------------
source(paste0(thisdir,"/04_TestApplication/NonDiffTest.R"))

