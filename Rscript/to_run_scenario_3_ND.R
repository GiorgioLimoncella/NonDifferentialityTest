rm(list=ls(all.names=TRUE))

### Setting the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir<-setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir<-setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(thisdir)

### Loading program parameters
source(paste0(thisdir,"/01_Parameters/ProgramParameters.R"))

### Setting study parameters
source(paste0(thisdir,"/01_Parameters/TestParameters_scenario3_ND.R"))

### Loading test functions 
source(paste0(thisdir,"/02_TestFunctions/TestStatistic.R"))
source(paste0(thisdir,"/02_TestFunctions/RiskRatio_estimator.R"))

### Generating data
source(paste0(thisdir,"/03_DataGen/ConditionalProbability.R"))

### RR distribution
source(paste0(thisdir,"/07_RR/RiskRatio_MC_distribution.R"))

#multiple indicator
plt_RR
mean(RR_est) 
sqrt(var(RR_est))


#single indicator
plt_RR_single_indicator
mean(RR_single_indicator) 
sqrt(var(RR_single_indicator))


### Non Differentiality test
source(paste0(thisdir,"/04_TestApplication/BootTest.R"))
plt_test

### Test Power
source(paste0(thisdir,"/04_TestApplication/TestPower.R"))
test_power 
