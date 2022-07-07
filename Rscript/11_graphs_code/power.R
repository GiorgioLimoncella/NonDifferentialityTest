DT_combinations <- fread(paste0(thisdir, "/05_Results/2022-7-4_18-1/DT_combinations.csv" ))
DT_combinations[is.na(power)]

fix(DT_combinations)

fwrite(DT_combinations, paste0(thisdir, "/05_Results/2022-7-4_18-1/DT_combinations.csv" ))
