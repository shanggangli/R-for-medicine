#金字塔图
#install.packages("DescTools")
library(DescTools)
#lp_dt<-fread("~/Desktop/LPa2/data/LPa2_data.csv")
#str(lp_dt)
lp_dt6<-as.data.frame(lp_dt4[,c("gender","age","LPa2","ICD_CAD")])
lp_dt6<-na.omit(lp_dt6)
lp_dt6<-subset(lp_dt6,lp_dt6$ICD_CAD==1)
lp_dt6$age<-as.numeric(lp_dt6$age)
lp_dt6$gender<-as.factor(lp_dt6$gender)
str(lp_dt6)
table(is.na(lp_dt6))
lp_dt6<-data.frame(lp_dt6)
lp_dt6<-na.omit(lp_dt6)

lp_dt6$age_2<-as.factor(ifelse(lp_dt6$age<=40,1,
                               ifelse(lp_dt6$age>40 & lp_dt6$age<=45,2,
                                      ifelse(lp_dt6$age>45 & lp_dt6$age<=50,3,
                                             ifelse(lp_dt6$age>50 & lp_dt6$age<=55,4,
                                                    ifelse(lp_dt6$age>55 & lp_dt6$age<=60,5,
                                                           ifelse( lp_dt6$age>60 & lp_dt6$age<=65,6,
                                                                   ifelse(lp_dt6$age>65 & lp_dt6$age<=70,7,
                                                                          ifelse(lp_dt6$age>70 & lp_dt6$age<=75,8,
                                                                                 ifelse(lp_dt6$age>75 & lp_dt6$age<=80,9,0))))))))))

str(lp_dt6)
table(is.na(lp_dt6))
length(lp_dt6$gender)
length(lp_dt6$age_2)
length(lp_dt6$LPa2)
LPa2_m<-subset(lp_dt6,lp_dt6$gender==1)
table(LPa2_m$age_2)
table(LPa2_f$age_2)
LPa2_f<-subset(lp_dt6,lp_dt6$gender==2)
round(tapply(LPa2_m[,c(4)],LPa2_m[,c(5)],FUN = "mean"),2)
round(tapply(LPa2_f[,c(4)],LPa2_f[,c(5)],FUN = "mean"),2)


m <- c(25.58,26.32,26.63,26.67,26.01,26.47,27.44,27.48,27.86,25.52 )
f <- c(27.11,19.93,24.99,26.61,29.23,28.36,28.61,29.38,28.56,28.17 )
class <- c('<40', '40-45', '46-50', '51-55','56-60', '61-65', '66-70', '71-75', '76-80',">80")

library(stringr)
library(data.table)
library(highcharter)

highchart() %>%
  hc_xAxis(
    
    list(
      categories = class,
      #颠倒坐标轴
      reversed = FALSE,
      labels = list(step = 1)),
    list(
      #调整坐标轴位置至右侧
      opposite = TRUE,
      categories = class,
      reversed = FALSE,
      #关联第一个坐标轴
      linkedTo = 0,
      labels = list(step = 1))
  ) %>%
  hc_plotOptions(series = list(#图内柱子边界线宽，设置为0，取消显示边界线
    borderWidth = 0))%>%
  hc_yAxis(
    #坐标轴显示值的格式设置，负值取绝对???
    labels = list(formatter = JS("function () { return (Math.abs(this.value));}")),
    #设置坐标轴范???
    min = -40,max = 40)%>%
  hc_tooltip(#设置提示框的格式
    formatter = JS("function () {
                   return '<b>' + this.series.name + ', age ' + this.point.category + ' </b><br/>' +
                   'LPa2 level: ' + Highcharts.numberFormat(Math.abs(this.point.y), 0);}"))%>%
  hc_title(text = "The Distribution of LPa2 in different age and gender",align="center")%>%
  hc_plotOptions(series= list(
    #设置堆叠柱状图，
    stacking = "normal")) %>%
  hc_add_series(name = "Male",
                #其中一类取负值，对称显示数据
                data = -m, 
                type = "bar") %>%
  hc_add_series(name = "Female",data = f,type = "bar") %>%
  hc_add_theme(hc_theme_538())
