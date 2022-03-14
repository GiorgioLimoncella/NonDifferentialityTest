rm(list=ls(all.names=TRUE))

### Setting the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir<-setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir<-setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(thisdir)

### Loading parameters
source(paste0(thisdir,"/Parameters/ProgramParameters.R"))
source(paste0(thisdir,"/Parameters/TestParameters.R"))

### Loading data
source(paste0(thisdir,"/DataGen/ConditionalProbability.R"))
