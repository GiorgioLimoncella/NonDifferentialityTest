#------------------
# Teast Application
#------------------

if(n_validated_AintB_ne == 0){
  t_vector = c()
  for (boot in 1:N_boot) {
    bootstrap_sample_A_e <- rbinom(n_validated_A_e, 1, PPV_A_e)
    bootstrap_sample_B_e <- rbinom(n_validated_B_e, 1, PPV_B_e)
    
    bootstrap_sample_A_ne <- rbinom(n_validated_A_ne, 1, PPV_A_ne)
    bootstrap_sample_B_ne <- rbinom(n_validated_B_ne, 1, PPV_B_ne)
    
    t <- statistic_with_C_empty(P_A_e, 
                                P_B_e, 
                                P_A_ne, 
                                P_B_ne, 
                                sum(bootstrap_sample_A_e)/n_validated_A_e,
                                sum(bootstrap_sample_B_e)/n_validated_B_e,
                                sum(bootstrap_sample_A_ne)/n_validated_A_ne,
                                sum(bootstrap_sample_B_ne)/n_validated_B_ne)
    t_vector <- c(t_vector, t)
  }
}else{
  t_vector = c()
  for (boot in 1:N_boot) {
    bootstrap_sample_A_e <- rbinom(n_validated_A_e, 1, PPV_A_e)
    bootstrap_sample_B_e <- rbinom(n_validated_B_e, 1, PPV_B_e)
    bootstrap_sample_AintB_e <- rbinom(n_validated_AintB_e, 1, PPV_AintB_e)
    
    bootstrap_sample_A_ne <- rbinom(n_validated_A_ne, 1, PPV_A_ne)
    bootstrap_sample_B_ne <- rbinom(n_validated_B_ne, 1, PPV_B_ne)
    bootstrap_sample_AintB_ne <- rbinom(n_validated_AintB_ne, 1, PPV_AintB_ne)
    
    t <- statistic_with_C_sample(P_A_e, 
                                 P_B_e, 
                                 P_AintB_e,
                                 P_A_ne, 
                                 P_B_ne, 
                                 P_AintB_ne,
                                 sum(bootstrap_sample_A_e)/n_validated_A_e,
                                 sum(bootstrap_sample_B_e)/n_validated_B_e,
                                 sum(bootstrap_sample_AintB_e)/n_validated_AintB_e,
                                 sum(bootstrap_sample_A_ne)/n_validated_A_ne,
                                 sum(bootstrap_sample_B_ne)/n_validated_B_ne,
                                 sum(bootstrap_sample_AintB_ne)/n_validated_AintB_ne)

    t_vector <- c(t_vector, t)
  }
}

write.csv(t_vector, file = paste0(dirresults, "/t_distribution.csv"), row.names = FALSE) 

quant_975 = quantile(t_vector, 1 - (alpha/2), na.rm = T)
quant_025 = quantile(t_vector, alpha/2, na.rm = T)

if(quant_025 < 0 & quant_975>0){
  cat("The null hypothesis (differential sensitivity) cannot be rejected.")
}else{
  cat("The null hypothesis (differential sensitivity) is rejected.")
}

DF <- data.frame(t = t_vector)

plt <- ggplot(DF, aes(x = t))+
  geom_density(col = "salmon", fill = "salmon", alpha = 0.5)+
  geom_vline(xintercept = quant_025, col = "red")+
  geom_vline(xintercept = quant_975, col = "red")+
  geom_vline(xintercept = 0, col = "grey6", linetype = "dashed")+
  xlab("Test statistic bootstrap distribution")+
  labs(title = "Non-Differentiality test")+
  theme_bw()+
  theme(text = element_text(family = "serif"))
  
ggsave(paste0(dirresults, "/NonDiffTest_Stat_Distribution.png"), 
       width = 25,
       height = 20,
       units = c("cm"),
       dpi = 720)


#------------------
# Saving Parameters
#------------------
params <- data.frame(PPV_A_e = PPV_A_e,      
                     PPV_B_e = PPV_B_e,      
                     PPV_AintB_e = PPV_AintB_e,  
                     PPV_A_ne = PPV_A_ne,      
                     PPV_B_ne = PPV_B_ne,      
                     PPV_AintB_ne = PPV_AintB_ne,  
                     n_validated_A_e = n_validated_A_e,    
                     n_validated_B_e = n_validated_B_e,     
                     n_validated_AintB_e = n_validated_AintB_e, 
                     n_validated_A_ne = n_validated_A_ne,     
                     n_validated_B_ne = n_validated_B_ne,     
                     n_validated_AintB_ne = n_validated_AintB_ne, 
                     P_A_e = P_A_e,   
                     P_B_e = P_B_e,   
                     P_AintB_e = P_AintB_e,
                     P_A_ne = P_A_ne,     
                     P_B_ne = P_B_ne,     
                     P_AintB_ne = P_AintB_ne, 
                     N_boot = N_boot, 
                     alpha = alpha) 

write.csv(params, file = paste0(dirresults, "/Parameters.csv"), row.names = FALSE) 
