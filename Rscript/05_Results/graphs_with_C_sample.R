Result[, se_e := c(0.15, 0.30, 0.40, 0.50, 0.60, 0.70, 0.85)]
plot <- ggplot(Result, aes(x=se_e, y=Power))+ #, label=label
  geom_point()+
  geom_line(size=1)+
  theme_hc()+
  ylab("")+
  #scale_color_manual(values = c("deeppink", "skyblue3", "yellow3"))+
  #scale_x_continuous( breaks=se_e, labels=se_e)+
  geom_vline(xintercept = 0.4, lty= 2, col = "slategrey")+
  geom_vline(xintercept = 0.6, lty= 2, col = "slategrey")
  #geom_label_repel(aes(label = label),
  #                 box.padding   = 0.35, 
  #                 point.padding = 0.5,
  #                 segment.color = 'slategrey')

ploty <- ggplotly(plot)
