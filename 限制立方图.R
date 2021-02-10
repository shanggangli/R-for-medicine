library(ggplot2)
library(rms)
#限制立方图
Follow_up_date<-as.numeric(lp_dt4$Follow_up_date)
Follow_up_death<-lp_dt4$Follow_up_death
LPa2<-lp_dt4$LPa2/10
age<-lp_dt4$age
gender<-lp_dt4$gender
AMI<-lp_dt4$AMI
ICD_DM<-lp_dt4$ICD_DM
ICD_HT<-lp_dt4$ICD_HT
CKD_new<-lp_dt4$CKD_new
CHF_new<-lp_dt4$CHF_new
PCI<-lp_dt4$PCI
HGB<-lp_dt4$HGB
WBC<-lp_dt4$WBC
CHOL2<-lp_dt4$CHOL2
TRIG2<-lp_dt4$TRIG2
APOA2<-lp_dt4$APOA2
APOB2<-lp_dt4$APOB2
LDLC<-lp_dt4$LDLC
HDLC<-lp_dt4$HDLC
Drug_B_blocker<-lp_dt4$Drug_B_blocker
Drug_Statins<-lp_dt4$Drug_Statins
ACEI_ARB<-ifelse(lp_dt4$Drug_ACEI==1|lp_dt4$Drug_ARB==1,1,0)

Sur2<-Surv(time=Follow_up_date,event=lp_dt4$Follow_up_death)

data_RCD<-data.frame(LPa2,Follow_up_date,Follow_up_death,
                     CHOL2,LDLC,HDLC,
                     ACEI_ARB,Drug_B_blocker,Drug_Statins)
dd<-datadist(data_RCD)
options(datadist="dd")
fit<-coxph(Sur2~rcs(LPa2,3),data =lp_dt4)

#hist(lp_dt4$LPa2,100)
fit<- cph(Sur2 ~ rcs(LPa2,3)+age+gender+AMI+ICD_DM+CHF_new+CKD_new+ICD_HT+PCI+
            CHOL2+TRIG2+APOA2+LDLC+HDLC+
            +ACEI_ARB+Drug_B_blocker+Drug_Statins,data=lp_dt4) 

fit<- cph(Sur2 ~ rcs(LPa2,3),data=lp_dt4) 

anova(fit)

LPa2_rcs<-Predict(fit,LPa2,fun=exp,ref.zero = TRUE)

fit=update(fit)
ggplot(LPa2_rcs, ylab='Odds Ratio',xlim=c(0,50),ylim=c(0.9,1.10)) +
  geom_line(linetype="solid",size=1,alpha = 0.7,colour="red")+
  geom_ribbon(data=LPa2_rcs, aes(LPa2,ymin = lower, ymax = upper),alpha = 0.1,fill="red")+
  theme_classic()+
  geom_hline(yintercept=1, linetype=2,size=1)+
  labs(x="Lp(a) concentration", y="Hazard ratio") 
