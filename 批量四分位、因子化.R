##2.4 增加四分位变量
dput(names(combine_data))

continuous_Var <- c("age_2num", "bodytemp_2num", "height_2num", "weight_2num",
                    "BMI", "wbc_2num", "rbc_2num", "hb_2num", "hct_2num", 
                    "plt_2num", "neu_2num", "lym_2num", "k_2num", "na_2num", 
                    "cysc_2num", "ca_2num", "ua_2num", "ast_2num", "alt_2num", 
                    "CHOL", "TRIG", "APOA", "APOB", "LDLC", "HDLC", "Lpa", 
                    "ck_2num", "ckmb_2num", "Pro_BNP", "BNP", "TNT", "TNI", 
                    "hscrp_2num", "GLU_random", "GLU_2h", "HA1C", 
                    "FPG", "pre_SCr", "post_SCr1", "post_SCr2", "post_SCr3", 
                    "post_max_SCr", "eGFR_rejudge", "eGFR", "urine_PRO", 
                    "urine_PH", "ALB", "BUN", "AST", "ALT", 
                    "tbil_2num", "dbil_2num", "crp_2num", "lvef_2num", 
                    "sbp_2num", "dbp_2num", "hr_2num", "CINA", "CINB", 
                    "CINC", "CIND", "Male")
str(combine_data)
Q4_grou_data <- combine_data[,continuous_Var]
combine_data$age_2num <- as.numeric(combine_data$age_2num)
combine_data$height_2num <- as.numeric(combine_data$height_2num)
combine_data$sbp_2num  <- as.numeric(combine_data$sbp_2num )
combine_data$dbp_2num <- as.numeric(combine_data$dbp_2num)
combine_data$hr_2num <- as.numeric(combine_data$hr_2num)
Q4_grou_data <- combine_data[,continuous_Var]
str(Q4_grou_data)
names(Q4_grou_data)
str(Q4_grou_data)

for(i in 1:57){
  m <- quantile(Q4_grou_data[,i],type = 4,na.rm = T)
  Q4_grou_data[,i+62] <- as.numeric(cut(Q4_grou_data[,i], m))
  colnames(Q4_grou_data)[i+62] <- dput(paste(names(Q4_grou_data)[i],"_Q4",sep = ""))
}
table(Q4_grou_data$lvef_2num_Q4)
View(Q4_grou_data)
names(Q4_grou_data)

##2.5 将分类后的变量转化为因子变量
dput(names(Q4_grou_data)[c(63:119)])

trans_Vars_Q4 <- dput(names(Q4_grou_data)[c(63:119)])

Q4_grou_data[,trans_Vars_Q4] <- as.data.frame(apply(Q4_grou_data[,trans_Vars_Q4], 
                                                    MARGIN = 2,FUN = as.factor))
#Q4_grou_data[,trans_Vars_Q4] <- as.data.frame(apply(Q4_grou_data[,trans_Vars_Q4], 
#                                                    MARGIN = 2,FUN = as.numeric))
str(Q4_grou_data)
