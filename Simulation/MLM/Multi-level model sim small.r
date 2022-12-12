#This is the R script used to reproduce the examples in the lecture.
#Varying intercept model under multi-level model framework.

#install.packages(lme4)
#install.packages(ggplot2)
library(lme4)
library(ggplot2)
library(MuMIn)
library(dplyr)

setwd("")

#Read in the data
data = read.csv("sim_data_small.csv")



mc_iter = 1000

re_list <- array(0,c(length(unique(data$zone)),3,mc_iter))
fe_list <- array(0,c(3,mc_iter))
res_list <- array(0,c(1600 ,mc_iter))

for (i in c(1:mc_iter)){
    print(i)
    err = rnorm(1600,0,1)
    data$dep2 = data$dep + err
    model_full <- lmer(formula = dep2 ~ 1 + X1 + X2 + (1 + X1 + X2 | zone), data = data)
    
    res_list[,i] = resid(model_full)
    re_list[,,i] = as.matrix(ranef(model_full)$zone)
    fe_list[,i] = array(fixef(model_full))
    
}


re_mean = as.data.frame(apply(re_list, c(1,2), mean))
names(re_mean) = names(ranef(model_full)$zone)

fe_mean = as.data.frame(t(apply(fe_list, c(1), mean)))
names(fe_mean) = names(fixef(model_full))

mean_res = as.data.frame(apply(res_list, c(1), mean))
names(mean_res) = "mlm_resid"


write.csv(re_mean,"re_small.csv")
write.csv(fe_mean,"fe_small.csv")
#write.csv(fe_mean,"mean_res_small.csv")

write.csv(as.matrix(res_list),"full_res_s.csv",row.names = FALSE)


re_mat = (re_list %x% rep(1,1,25))
dim(fe_list) = c(1,3,mc_iter)
fe_mat = (fe_list %x% rep(1,1,1600))

newdata = data[order(data$zone),]

true_b = as.matrix(newdata %>% select('b0','b1','b2'))
dim(true_b) = c(1600,3,1)

true_b_mat = matrix(, nrow = 1600*3, ncol = mc_iter)
dim(true_b_mat) = c(1600,3,mc_iter)

for (j in (1:mc_iter)){
    true_b_mat[,,j] = true_b
}

rmse = sqrt(t(apply((fe_mat + re_mat - true_b_mat)**2, c(2,3), mean)))
rmse_df = as.data.frame(rmse,)
colnames(rmse_df) = c("mlm_8_b0", "mlm_8_b1", "mlm_8_b2")
write.csv(rmse_df,"mlm_8_mc.csv",row.names = FALSE)

