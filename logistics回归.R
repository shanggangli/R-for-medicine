#单因素logistics回归
trans_Vars1 <- c( "sex_2num","hypertension","Smoke_His_liu","TCBI_group","cndose_2num","dbp_2num","sbp_2num","age_2num",
                  "DM_His",
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
                                  "TCBI","TCBI_group","CINA","CINB","CINC","CIND")])

str(mydata7)
Uni_logistic_CIN_continuous <- data.frame(var1=0,var2=0,var3=0,var4=0,var5=0,var6=0,var7=0)

for (i in 1:34) {
  m <- glm(CINA ~ mydata7[,i], data = mydata7, family = binomial(link = "logit"))
  n <- lrm(CINA ~ mydata7[,i], data = mydata7,x=T,y=T)
  beta <- round(coef(m)[2],5)
  SE <- round(summary(m)$coefficients[2,2],5)
  OR <- round(exp(coef(m))[2],3)
  CI_lower <- round(exp(confint(m)[2,1]),3)
  CI_upper <- round(exp(confint(m)[2,2]),3)
  p_value <- round(summary(m)$coefficients[2,4],3)
  C_statistics <- round(as.data.frame(n$stats)[6,1],3)
  Uni_logistic_CIN_continuous[i,] <- data.frame('Odds Ratio' = OR,
                                                '95%CI_low' = CI_lower,
                                                '95%CI_up' = CI_upper,
                                                'P Value' = p_value,
                                                'beta' = beta,
                                                'SE' = SE,
                                                'C statistics' = C_statistics)
  Uni_logistic_CIN_continuous <- as.data.frame(Uni_logistic_CIN_continuous)
  colnames(Uni_logistic_CIN_continuous) <- c("Odds Ratio","95%CI_low","95%CI_up","P Value","beta","SE","C_statistics")
  rownames(Uni_logistic_CIN_continuous)[i] <- c(dput(names(mydata7)[i]))
}
#View(Uni_logistic_CIN_continuous)
write.xlsx(Uni_logistic_CIN_continuous, "CAD人群-TCBI.xlsx" , sheetName = "单因素CINA",
           col.names = T,row.names = T,showNA = F,append = T) #输出


#logistic 多因素
#CINA
m<-glm(CINA~TCBI_group_CINA+age_2num+scr_2num+hb_2num+DM_His+cndose_2num+CHF_Liu+iabp_2cat,
       family=binomial(link = "logit"),data=mydata6)
n <- lrm(CINA~TCBI_group_CINA+age_2num+scr_2num+hb_2num+DM_His+cndose_2num+CHF_Liu+iabp_2cat,
         data=mydata6)
beta <- round(coef(m),5)
SE <- round(summary(m)$coefficients[,2],5)
OR <- round(exp(coef(m)),3)
CI_lower <- round(exp(confint(m)[,1]),3)
CI_upper <- round(exp(confint(m)[,2]),3)
p_value <- round(summary(m)$coefficients[,4],3)
C_statistics <- round(as.data.frame(n$stats)[6,1],3)
Multi_logistic <- data.frame("Odds Ratio"=OR,
                             "95%CI_low"=CI_lower,
                             "95%CI_upper"=CI_upper,
                             "P_value"=p_value,
                             "beta"=beta,
                             "SE"=SE,
                             "C_statistics"=C_statistics)

write.xlsx(Multi_logistic, "CAD人群-TCBI-cutoff.xlsx" , sheetName = "多因素CINA",
           col.names = T,row.names = T,showNA = F,append = T) #输出
