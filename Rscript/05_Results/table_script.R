DT <- copy(as.data.table(DT_comb_RR))
DT_1 <- unique(DT[ risk != 0.5, .(prev_ne, risk, prop_exp,SE_A_e, SE_A_ne)])[order(-prev_ne, -risk, prop_exp,SE_A_e )]

fwrite(DT_1, paste0(thisdir, "/05_Results/2022-7-20_18-15_RR/DT.csv"))






DT_2 <- DT[sample_size == "100_100_50", .(prop_exp , prev_ne , risk, SE_exp, bias_mis, sd_mis, rmse_mis, bias_single, sd_single, rmse_single)]

DT_2 <- DT_2[, id := paste(prop_exp , prev_ne , risk, SE_exp, sep = "_")]
DT_2_m <- melt(DT_2, id.vars = c("id"),
       measure.vars = c("bias_mis", "sd_mis", "rmse_mis", "bias_single", "sd_single", "rmse_single"))

setnames(DT_2_m, "value", "sample_250" )

DT_3 <- DT[sample_size == "200_200_100", .(prop_exp , prev_ne , risk, SE_exp, bias_mis, sd_mis, rmse_mis, bias_single, sd_single, rmse_single)]

DT_3 <- DT_3[, id := paste(prop_exp , prev_ne , risk, SE_exp, sep = "_")]
DT_3_m <- melt(DT_3, id.vars = c("id"),
               measure.vars = c("bias_mis", "sd_mis", "rmse_mis", "bias_single", "sd_single", "rmse_single"))

setnames(DT_3_m, "value", "sample_500" )

DT_4 <- merge(DT_2_m, DT_3_m[, .(id, variable, sample_500)], by = c("id", "variable"), all.x = T)

DT_4 <- DT_4[ variable == "bias_mis" | variable == "bias_single", var_id := "bias"]
DT_4 <- DT_4[ variable == "sd_mis" | variable == "sd_single", var_id := "sd"]
DT_4 <- DT_4[ variable == "rmse_mis" | variable == "rmse_single", var_id := "rmse"]


DT_4_mis <- DT_4[variable == "bias_mis"| variable == "sd_mis" |variable ==  "rmse_mis"]


DT_4_single <- DT_4[variable == "bias_single"| variable == "sd_single" |variable ==  "rmse_single"]

DT_5 <- merge(DT_4_mis, DT_4_single, by = c("id", "var_id"), all.x = T)


fwrite(DT_5, paste0(thisdir, "/05_Results/2022-7-20_18-15_RR/DT_final.csv"))

