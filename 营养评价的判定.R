#2.分组
########################################################################################################

#CONUT 
mydata6$alb_2num1 <- as.numeric((ifelse(mydata6$alb_2num>=35,0,
                                        ifelse(mydata6$alb_2num<35&mydata6$alb_2num>=30,2,
                                               ifelse(mydata6$alb_2num<30&mydata6$alb_2num>=25,4,6)))))
mydata6$lym_2num1<-as.numeric((ifelse(mydata6$lym_2num>=1.6,0,
                                      ifelse(mydata6$lym_2num<1.6&mydata6$lym_2num>=1.2,1,
                                             ifelse(mydata6$lym_2num<1.2&mydata6$lym_2num>=0.80,2,3)))))

mydata6$tc_2num1<-as.numeric((ifelse(mydata6$tc_2num>=(180/38.67),0,
                                     ifelse(mydata6$tc_2num<(180/38.67)&mydata6$tc_2num>=(140/38.67),1,
                                            ifelse(mydata6$tc_2num<(140/38.67)&mydata6$tc_2num>=(100/38.67),2,3)))))
mydata6$CONUT<-as.numeric(mydata6$alb_2num1+mydata6$lym_2num1+mydata6$tc_2num1)
mydata6$CONUT_group<-as.factor(ifelse(mydata6$CONUT<=1,0,1))
mydata6$CONUT_group1<-as.factor(ifelse(mydata6$CONUT<=1,0,
                                       ifelse(mydata6$CONUT>1 & mydata6$CONUT<=4,1,2)))

#PNI
mydata6$PNI<-mydata6$alb_2num+(5*mydata6$lym_2num)
mydata6$PNI_group<-as.factor(ifelse(mydata6$PNI>44.580,0,1))

#TCBI
mydata6$TCBI<-mydata6$tg_2num*mydata6$tc_2num*mydata6$weight_2num/1000
mydata6$TCBI_group<-as.factor(ifelse(mydata6$TCBI>0.167,0,1))
mydata6$TCBI_group1<-as.factor(ifelse(mydata6$TCBI<0.2499,0,
                                      ifelse(mydata6$TCBI<0.39813&mydata6$TCBI>=0.2499,1,
                                             ifelse(mydata6$TCBI<0.6257&mydata6$TCBI>=0.39813,2,3))))

mydata6$iabp_2cat <- ifelse(mydata6$iabp_2num==0,0,1)

#cutoff
library(OptimalCutpoints) #安装OptimalCutpoints包
#cutoff
optimal.cutpoint.Youden <- optimal.cutpoints(X = "TCBI", status = "CIND", tag.healthy = 0, 
                                             methods = "Youden", data = mydata6, pop.prev = NULL, 
                                             #categorical.cov = "gender", 
                                             control = control.cutpoints(), ci.fit = FALSE, conf.level = 0.95, trace = FALSE)

summary(optimal.cutpoint.Youden)

mydata6$TCBI_group_CINA<-as.factor(ifelse(mydata6$TCBI>0.8974350,0,1))
mydata6$TCBI_group_CINB<-as.factor(ifelse(mydata6$TCBI>0.6639360,0,1))
mydata6$TCBI_group_CINC<-as.factor(ifelse(mydata6$TCBI>0.8974350,0,1))
mydata6$TCBI_group_CIND<-as.factor(ifelse(mydata6$TCBI>1.3886100,0,1))