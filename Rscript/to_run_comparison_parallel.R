rm(list=ls(all.names=TRUE))

### Setting the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir<-setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir<-setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(thisdir)

### Loading program parameters
source(paste0(thisdir,"/01_Parameters/ProgramParameters.R"))

### Setting study parameters
source(paste0(thisdir,"/01_Parameters/TestParameters_scenario2.R"))

### Loading test functions 
source(paste0(thisdir,"/02_TestFunctions/TestStatistic.R"))
source(paste0(thisdir,"/02_TestFunctions/RiskRatio_estimator.R"))

### Generating data
source(paste0(thisdir,"/03_DataGen/ConditionalProbability.R"))


##############################################
### Non Differentiality test: non-Parallel ###
##############################################
start_non_parallel <- Sys.time()
### Test Power
source(paste0(thisdir,"/04_TestApplication/TestPower_with_C_sample.R"))
end_non_parallel <- Sys.time()

time_non_parallel <- end_non_parallel - start_non_parallel # Time difference of 7.164448 mins
##########################################
### Non Differentiality test: Parallel ###
##########################################
start_parallel <- Sys.time()
### Test Power
source(paste0(thisdir,"/04_TestApplication/TestPower_with_C_sample_parallel_parlapply.R"))
end_parallel <- Sys.time()

time_parallel <- end_parallel - start_parallel

results
