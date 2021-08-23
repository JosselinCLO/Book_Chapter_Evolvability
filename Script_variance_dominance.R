Estimates = read.table("Estimation.csv", header = T, dec=",", sep=";")

ratio_VD = c(NULL)
ratio_VDI = c(NULL)
ratio_COVadi = c(NULL)
ratio_H = c(NULL)

for(i in 1:nrow(Estimates)){
  
  ratio_VD[i] = Estimates$V_DO[i]/(Estimates$V_A[i]+Estimates$V_DO[i])
  ratio_VDI[i] = Estimates$V_DI[i]/(Estimates$V_A[i]+Estimates$V_DI[i])
  ratio_COVadi[i] = abs(Estimates$Cov_a_d[i])/(Estimates$V_A[i]+abs(Estimates$Cov_a_d[i]))
  ratio_H[i] = Estimates$H[i]/(Estimates$V_A[i]+Estimates$H[i])
  
}

wilcox.test(ratio_VD)
wilcox.test(ratio_VDI)
wilcox.test(ratio_COVadi)
wilcox.test(ratio_H)

mean(ratio_VD)
mean(ratio_VDI)
mean(ratio_COVadi)
mean(ratio_H)
