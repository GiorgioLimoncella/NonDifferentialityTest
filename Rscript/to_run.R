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

prop_exp_list   <-c(0.2)            #c(0.05, 0.2)
pi_ne_list      <-c(0.05)      #c(0.01, 0.05, 0.1)
risk_list       <-c(2)               #c(1.1, 2)
SE_exposed_list <-c(0.35, 0.5, 0.65)      #c(0.3, 0.4, 0.5, 0.6, 0.7)

TestPower <- c()
combination <- c()

for (i in prop_exp_list) {
  for (j in pi_ne_list) {
    for (t in risk_list) {
      for (k in SE_exposed_list) {
        
        combination <- c(combination, paste0(i, "_", j, "_", t, "_", k))
        
        ### setting parameters
        source(paste0(thisdir,"/01_Parameters/TestParameters.R"))
        
        ### Loading data
        source(paste0(thisdir,"/03_DataGen/ConditionalProbability.R"))
        
        ### Test 
        source(paste0(thisdir,"/04_TestApplication/TestPower.R"))
        
        TestPower <- c(TestPower, rej_95)
      }
      
    }
    
  }
  
}



