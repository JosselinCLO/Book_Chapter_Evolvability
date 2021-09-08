Estimates = read.table("Estimation.csv", header = T, dec=",", sep=";")

# Means

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

mean(ratio_VD)
mean(ratio_VDI)
mean(ratio_COVadi)
mean(ratio_H)

# Bootstrap 

nb_rep = 100

ratio_VD_boot = c(NULL)
ratio_VDI_boot = c(NULL)
ratio_COVadi_boot = c(NULL)
ratio_H_boot = c(NULL)

for(i in 1:nb_rep){
  
  sub_sample = sample(1:40, 40, replace = T)  
  
  for(j in 1:length(sub_sample)){
    
    sub_temp = Estimates[sub_sample[j],]
    
    if(j == 1){sub_fin = sub_temp}
    else{sub_fin = rbind(sub_fin, sub_temp)}
  }

ratio_VD_boot[i] = mean(sub_fin$V_DO/(sub_fin$V_A+sub_fin$V_DO))
ratio_VDI_boot[i] = mean(sub_fin$V_DI/(sub_fin$V_A+sub_fin$V_DI)) 
ratio_COVadi_boot[i] = mean(abs(sub_fin$Cov_a_d)/(sub_fin$V_A+abs(sub_fin$Cov_a_d)))
ratio_H_boot[i] = mean(sub_fin$H/(sub_fin$V_A+sub_fin$H))
  
}
