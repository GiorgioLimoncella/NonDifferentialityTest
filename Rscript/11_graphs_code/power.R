DT_combinations <- fread(paste0(thisdir, "/05_Results/2022-7-4_18-1/DT_combinations.csv" ))
DT_combinations[is.na(power)]

fix(DT_combinations)

fwrite(DT_combinations, paste0(thisdir, "/05_Results/2022-7-4_18-1/DT_combinations.csv" ))


#------
# Bias
#------


for (prop in c(0.05, 0.2)) {
  for (prev in c(0.01, 0.10)) {
    for (ris in c(0.5, 1.2, 2.0)) {
      
      tmp <- DT_comb_RR[prop_exp == prop & prev_ne == prev & risk == ris & sample_size == "200_200_100"]
      
      DT.m1 = melt(tmp, id.vars = c("SE_A_e", "SE_A_ne"),
                   measure.vars = c("bias_single", "bias_mis"))
      
      plt <- ggplot(DT.m1, aes(SE_A_e, value, col = variable))+
        geom_point()+
        geom_line()+
        ggtitle(paste0("prop:", prop, " prev:", prev, " risk:", ris))
      
      tmp <- paste0("prop:", prop, " prev:", prev, " risk:", ris)
      assign(tmp, plt)
      
      print(plt)
    }
  }
}
