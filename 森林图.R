library(grid)
library(magrittr)
library(checkmate)
library(forestplot)
mydata <- read.csv('森林图.csv',sep = ",", header = F)
# mydata <- mydata[-22,]
#View(mydata)
attach(mydata)

#Hazard Ratio
forestplot(#labeltext=tabletext,
           as.matrix(mydata[,c(1:3,7)]), #展示哪几列
           mean = V4, #HR或OR的那列
           is.summary = c(T,T,F,F,F,F,T,F,F,F,
                          F,T,F,F,F,F,T,F,F,F,
                          F,T,F,F,F,F,T,F,F,F,
                          F
                        ),
           #第几行加粗
           lower = V5, #95%CI下界
           upper = V6, #95%CI上界
           #title = "Hazard Ratio", #表头
           xticks = seq(from = 0.5, to = 2, by = 0.5), #X轴刻度
           lwd.xaxis = 1, #X轴粗细
           clip = c(0.5,2), #置信区间范围，超过会变成箭头
           zero = 1, #参照系取值
           lwd.zero = 1, #参照线粗细
           lineheight = "auto", #行距
           colgap = unit(8,"mm"), #列宽
           graphwidth = unit(80,"mm"), #森林图宽度
           boxsize = 0.2, #线中间方块大小
           lwd.ci = 1, #置信区间线的粗细
           graph.pos=4, #森林图放在第几列
           col = fpColors(all.elements = "black"),
           hrzl_lines=list("2" = gpar(lwd=1,lineend="butt",columns=c(1:5), col="black")),
           xlab="<-Lower risk            Higher risk->                               " 
           
)

#Variable  Patients
