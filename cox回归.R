# cox单因素分析
trans_Vars1 <- c( "sex_2num","hypertension","Smoke_His_liu","cndose_2num","dbp_2num","sbp_2num","age_2num",
                  "DM_His","CKD",
                  "CHF_Liu","iabp_2cat","statin_pre","ccb_pre","acei_arb_pre","diuretics_pre","asipilin_post","clopidogrel_post",
                  "statin_post","ccb_post","acei_arb_post","diuretics_post")
mydata6[,trans_Vars1] <- as.data.frame(apply(mydata6[,trans_Vars1], MARGIN = 2,FUN = as.numeric))

mydata7<-as.data.frame(mydata6[,c("age_2num","sex_2num",
                                  "CKD",
                                  "hypertension",
                                  "DM_His",
                                  "Smoke_His_liu",
                                  "CHF_Liu","iabp_2cat",
                                  "sbp_2num","dbp_2num","bmi_old","hb_2num","tc_2num","lym_2num",
                                  "alb_2num","scr_2num",
                                  "eGFR","tbil_2num","ua_2num","crp","pro_2num","cndose_2num",
                                  "statin_pre","ccb_pre","acei_arb_pre","diuretics_pre",
                                  "asipilin_post","clopidogrel_post","statin_post","ccb_post","acei_arb_post","diuretics_post",
                                  "followup_date","death_30d","death_90d","death_365d")])

str(mydata7)

cox <- data.frame(var1=0,var2=0,var3=0,
                  var4=0,var5=0,var6=0,
                  var7=0,var8=0,var9=0,
                  var10=0)
# death-30d
for (i in 1:32) {
  m=coxph(Surv(time=followup_date,event=death_30d) ~ mydata7[,i], data =  mydata7)
  beta <- as.numeric(coef(m))
  se <- as.numeric(sqrt(diag(vcov(m))))
  HR <- as.numeric(exp(beta))
  HRse <- as.numeric(HR * se)

  cox[i,] <- round(cbind(coef = beta, se = se, z = beta/se, p = 1 - pchisq((beta/se)^2, 1),
                         HR = HR, HRse = HRse,
                         HRz = (HR - 1) / HRse, HRp = 1 - pchisq(((HR - 1)/HRse)^2, 1),
                         HRCILL = exp(beta - qnorm(.975, 0, 1) * se),
                         HRCIUL = exp(beta + qnorm(.975, 0, 1) * se)), 7)
}

cox <- as.data.frame(cox)
colnames(cox) <- c("coef","se","z","p","HR","HRse","HRz","HRp","HRCILL","HRCIUL")
rownames(cox) <- dput(names(mydata7[,1:32]))
write.xlsx(Uni_logistic_CIN_continuous, "CKD和DM人群-COX.xlsx" , sheetName = "单因素-death_30d",
           col.names = T,row.names = T,showNA = F,append = T) #输出


# 多因素分析
################################################################################
multi_model <- coxph(Surv(followup_date , death_30d) ~ CONUT_group + age_2num + sex_2num + hypertension,data = mydata6)
multiCox_Sum <- summary(multi_model)
N <- multiCox_Sum$n
N_Event <- multiCox_Sum$nevent
C_statistics <- round(multiCox_Sum$concordance[1],4)
HR <- round(multiCox_Sum$coefficients[,2],4)
pvalue <- round(multiCox_Sum$coefficients[,5],4)
CIlow = round(multiCox_Sum$conf.int[,3],4)
CIup = round(multiCox_Sum$conf.int[,4],4)
beta <- round(multiCox_Sum$coefficients[,1],4)
result_muiti_model <- data.frame('HR' = HR,
                                 'CI low' = CIlow,
                                 'CI up' = CIup, 
                                 'P Value' = pvalue,
                                 'Total number' = N,
                                 'Event number' = N_Event,
                                 'C statistics' = C_statistics,
                                 'beta' = beta)
colnames(result_muiti_model) <- c("HR","CI low","CI up","P Value","Total number","Event number","C statistics","beta")
write.xlsx(result_muiti_model, "CKD和DM人群-多COX-CONUT.xlsx" , sheetName = "30d",
           col.names = T,row.names = T,showNA = F,append = T) #输出
