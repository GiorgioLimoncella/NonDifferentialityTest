DT_A <- data[A==1]
DT_B <- data[B==1]
DT_C <- data[C==1]

P_A_e = data[A==1&E==1, .N]/data[E==1, .N]
P_A_ne = data[A==1&E==0, .N]/data[E==0, .N]

P_B_e = data[B==1&E==1, .N]/data[E==1, .N]
P_B_ne = data[B==1&E==0, .N]/data[E==0, .N]

P_C_e = data[C==1&E==1, .N]/data[E==1, .N]
P_C_ne = data[C==1&E==0, .N]/data[E==0, .N]

P_A =  data[A==1, .N]/N
P_B = data[B==1, .N]/N
P_C = data[C==1, .N]/N

PPV_A_e<- c()
PPV_A_ne<- c()
PPV_B_e<- c()
PPV_B_ne<- c()
PPV_C_e<- c()
PPV_C_ne<- c()



for(i in 1:nsam){
  
  sample_A = DT_A[sample(nrow(DT_A),na),]
  sample_B = DT_B[sample(nrow(DT_B),nb),]
  sample_C = DT_C[sample(nrow(DT_B),nc),]
  
  PPV_A_e  <- c(PPV_A_e , sample_A[A==1 & E==1 & Y==1, .N]/ sample_A[A==1 & E==1, .N])
  PPV_A_ne <- c(PPV_A_ne, sample_A[A==1 & E==0 & Y==1, .N]/ sample_A[A==1 & E==0, .N])
  PPV_B_e  <- c(PPV_B_e , sample_B[B==1 & E==1 & Y==1, .N]/ sample_B[B==1 & E==1, .N])
  PPV_B_ne <- c(PPV_B_ne, sample_B[B==1 & E==0 & Y==1, .N]/ sample_B[B==1 & E==0, .N])
  PPV_C_e  <- c(PPV_C_e , sample_C[C==1 & E==1 & Y==1, .N]/ sample_C[C==1 & E==1, .N])
  PPV_C_ne <- c(PPV_C_ne, sample_C[C==1 & E==0 & Y==1, .N]/ sample_C[C==1 & E==0, .N])
  
  RR <- (P_A_e * PPV_A_e + P_B_e * PPV_B_e - P_C_e * PPV_C_e) / 
    (P_A_ne * PPV_A_ne + P_B_ne * PPV_B_ne - P_C_ne * PPV_C_ne)
}


DT_PPV_RR <- data.table(PPV_A_e  = PPV_A_e ,
                        PPV_A_ne = PPV_A_ne,
                        PPV_B_e  = PPV_B_e ,
                        PPV_B_ne = PPV_B_ne,
                        PPV_C_e  = PPV_C_e ,
                        PPV_C_ne = PPV_C_ne,
                        RR = RR)

DT_PPV <- data.table(PPV = c(PPV_A_e, 
                             PPV_A_ne,
                             PPV_B_e,
                             PPV_B_ne,
                             PPV_C_e, 
                             PPV_C_ne), 
                     algorithm = c(rep("A_e", 1000),
                                   rep("A_ne", 1000),
                                   rep("B_e", 1000),
                                   rep("B_ne", 1000),
                                   rep("C_e", 1000),
                                   rep("C_ne", 1000)))

DT_RR <- data.table(RR = RR)


# 
# plt <- ggplot(DT_PPV, aes(x = PPV, fill = algorithm))+
#         geom_density(alpha = 0.5)+
#         ggtitle("PPV distributions")+
#         theme_hc()
# 
# plty <- ggplotly(plt)
# 
# 
# 
# plt_RR <- ggplot(DT_RR, aes(RR))+
#   geom_density(fill = "salmon", col = "salmon", alpha= 0.5)+
#   geom_vline(xintercept = 2, col = "grey")+
#   geom_vline(xintercept = mean(DT_RR$RR), col = "salmon")+
#   ggtitle("RR Monte Carlo distributions")+
#   theme_hc()
# 
# plty_RR <- ggplotly(plt_RR)
