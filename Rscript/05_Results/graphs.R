se_e <- rep(c( 0.3, 0.4, 0.5, 0.6, 0.7), 4)
Result <- cbind(Result, se_e)

#DF=data.frame(sensitivity_exposed, power, sample)
#ix_label <- c(2, 4)
#DF$label <- ""
#DF$label[ix_label] <- c(0.591, 0.632)

Result[1:5, Scenario := "Prevalence 1%, Risk 1.1"]
Result[6:10, Scenario := "Prevalence 1%, Risk 2"]
Result[11:15, Scenario := "Prevalence 10%, Risk 1.1"]
Result[16:20, Scenario := "Prevalence 10%, Risk 2"]

fwrite(Result, paste0(thisdir, "/05_Results/DT_for_plot.csv"))

plot <- ggplot(Result, aes(x=se_e, y=Power, col = Scenario))+ #, label=label
  geom_point()+
  geom_line(size=1)+
  theme_hc()+
  ylab("")+
  #scale_color_manual(values = c("deeppink", "skyblue3", "yellow3"))+
  scale_x_continuous( breaks=se_e, labels=se_e)+
  geom_vline(xintercept = 0.4, lty= 2, col = "slategrey")+
  geom_vline(xintercept = 0.6, lty= 2, col = "slategrey")
  #geom_label_repel(aes(label = label),
  #                 box.padding   = 0.35, 
  #                 point.padding = 0.5,
  #                 segment.color = 'slategrey')

ploty <- ggplotly(plot)
