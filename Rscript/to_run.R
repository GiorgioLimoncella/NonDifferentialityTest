rm(list=ls(all.names=TRUE))

### Setting the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir<-setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir<-setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(thisdir)

### Loading parameters
source(paste0(thisdir,"/01_Parameters/ProgramParameters.R"))
source(paste0(thisdir,"/01_Parameters/TestParameters.R"))

### Loading data
source(paste0(thisdir,"/02_DataGen/ConditionalProbability.R"))

### Loading statistic function
source(paste0(thisdir,"/03_TestStatistic/TestStatistic.R"))

### Test 
source(paste0(thisdir,"/04_TestApplication/TestPower.R"))
