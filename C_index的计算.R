# C值计算的五种方法
data <- read.csv("")
dd <- datadist(data)
options(datadist="dd")

fornula <- as.formula(分组~年龄+BMI指数+手术时间+术中失血)
fit <- lrm(fornula,data = data, x=T,y=T)

# 1.
data$predvalue <- predict(fit)
library(pROC)
modelROC <- roc(data$分组,data$predvalue)
auc(modelROC)# C index
ci(auc(modelROC)) # confidence interval C index

#2.
library(Hmisc)
somer2(data$predvalue,data$分组)

#3.
v <- validate(fit,method="boot",B=1000,dxy=T)
Dxy <- v[rownames(v)=="Dxy",colnames(v)=="index.corrected"]
orig_Dxy <- v[rownames(v)=="Dxy",colnames(v)=="index.orig"]
bias_corrected_c_index <- abs(Dxy)/2+0.5 # adjusted C index
orig_c_index <- abs(orig_Dxy)/2+0.5 # orign C index
bias_corrected_c_index
orig_c_index

#4.
c <- rcorrcens(分组~predict(fit,newdata=data),data=data)
c[1,1]
c[1,1]-1.96*c[1,4]/2
c[1,1]+1.96*c[1,4]/2


#5.
fit

