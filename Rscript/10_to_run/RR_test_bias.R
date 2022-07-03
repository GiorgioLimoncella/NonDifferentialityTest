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

Prev_osservata_AE  <- data[A==1 & E==1, .N]/data[E==1, .N]
Prev_osservata_ANE <- data[A==1 & E==0, .N]/data[E==0, .N]

Prev_osservata_BE  <- data[B==1 & E==1, .N]/data[E==1, .N]
Prev_osservata_BNE <- data[B==1 & E==0, .N]/data[E==0, .N]

Prev_osservata_CE  <- data[C==1 & E==1, .N]/data[E==1, .N]
Prev_osservata_CNE <- data[C==1 & E==0, .N]/data[E==0, .N]

Prev_osservata_BE  <- data[B==1 & E==1, .N]/data[E==1, .N]
Prev_osservata_BNE <- data[B==1 & E==0, .N]/data[E==0, .N]

SE_posteriori_AE  <- data[Y==1 & A==1 & E==1, .N]/data[Y==1 & E==1, .N]
SE_posteriori_ANE <- data[Y==1 & A==1 & E==0, .N]/data[Y==1 & E==0, .N]

SE_posteriori_BE  <- data[Y==1 & B==1 & E==1, .N]/data[Y==1 & E==1, .N]
SE_posteriori_BNE <- data[Y==1 & B==1 & E==0, .N]/data[Y==1 & E==0, .N]

SE_posteriori_CE  <- data[Y==1 & C==1 & E==1, .N]/data[Y==1 & E==1, .N]
SE_posteriori_CNE <- data[Y==1 & C==1 & E==0, .N]/data[Y==1 & E==0, .N]

PPV_posteriori_AE  <- data[Y==1 & A==1 & E==1, .N]/data[ A==1 & E==1, .N]
PPV_posteriori_ANE <- data[Y==1 & A==1 & E==0, .N]/data[ A==1 & E==0, .N]

PPV_posteriori_BE  <- data[Y==1 & B==1 & E==1, .N]/data[ B==1 & E==1, .N]
PPV_posteriori_BNE <- data[Y==1 & B==1 & E==0, .N]/data[ B==1 & E==0, .N]

PPV_posteriori_CE  <- data[Y==1 & C==1 & E==1, .N]/data[ C==1 & E==1, .N]
PPV_posteriori_CNE <- data[Y==1 & C==1 & E==1, .N]/data[ C==1 & E==1, .N]

RR_posteriori_stimato_max <- (Prev_osservata_AE  * PPV_posteriori_AE  + Prev_osservata_BE  * PPV_posteriori_BE  - Prev_osservata_CE  * max(PPV_posteriori_AE,  PPV_posteriori_BE)) /
                 (Prev_osservata_ANE * PPV_posteriori_ANE + Prev_osservata_BNE * PPV_posteriori_BNE - Prev_osservata_CNE * max(PPV_posteriori_ANE, PPV_posteriori_BNE)) 

RR_posteriori_stimato <- (Prev_osservata_AE*PPV_posteriori_AE + Prev_osservata_BE*PPV_posteriori_BE - Prev_osservata_CE*PPV_posteriori_CE) /
                 (Prev_osservata_ANE*PPV_posteriori_ANE + Prev_osservata_BNE*PPV_posteriori_BNE - Prev_osservata_CNE*PPV_posteriori_CNE) 


PI_E_posteriori <- (data[Y==1 & E==1, .N]/data[ E==1, .N])
PI_NE_posteriori <- (data[Y==1 & E==0, .N]/data[ E==0, .N]) 

RR_posteriori <- (data[Y==1 & E==1, .N]/data[ E==1, .N]) /  (data[Y==1 & E==0, .N]/data[ E==0, .N]) 

SE_posteriori_AE + SE_posteriori_BE - SE_posteriori_CE



PI_E <- (Prev_osservata_AE * PPV_posteriori_AE / SE_posteriori_AE)
PI_NE <- (Prev_osservata_ANE * PPV_posteriori_ANE / SE_posteriori_ANE)

SE_AUB_formula <- (Prev_osservata_AE * PPV_posteriori_AE / PI_E_posteriori) + (Prev_osservata_BE * PPV_posteriori_BE / PI_E_posteriori) - (Prev_osservata_CE * PPV_posteriori_CE / PI_E_posteriori) 
SE_AUB_formula_NE <- (Prev_osservata_ANE * PPV_posteriori_ANE / PI_NE_posteriori) + (Prev_osservata_BNE * PPV_posteriori_BNE / PI_NE_posteriori) - (Prev_osservata_CNE * PPV_posteriori_CNE / PI_NE_posteriori) 


R_formula <- (Prev_osservata_AE * PPV_posteriori_AE / SE_AUB_formula) + (Prev_osservata_BE * PPV_posteriori_BE / SE_AUB_formula) - (Prev_osservata_CE * PPV_posteriori_CE / SE_AUB_formula) 

R_formula_NE <- (Prev_osservata_ANE * PPV_posteriori_ANE / SE_AUB_formula) + (Prev_osservata_BNE * PPV_posteriori_BNE / SE_AUB_formula) - (Prev_osservata_CNE * PPV_posteriori_CNE / SE_AUB_formula) 





Prev_osservata_BNE <- data[B==1 & E==0, .N]/data[E==0, .N]

PPV_posteriori_BNE <- data[Y==1 & B==1 & E==0, .N]/data[ B==1 & E==0, .N]

PI_NE_posteriori   <- data[Y==1 & E==0, .N]/data[ E==0, .N]

SE_B_non_exp <- Prev_osservata_BNE * PPV_posteriori_BNE / PI_NE_posteriori
