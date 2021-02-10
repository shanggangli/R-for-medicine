#K-M图
# 30d
library(survival)
library(survminer)
fit_30d <- survfit(Surv(followup_date_30d, death_30d) ~ CONUT_group,
                   data = mydata6)
coxph(Surv(followup_date_30d, death_30d) ~  CONUT_group , data = mydata6)


png("CKD和DM人群-CONUT-30d-KM.png",width = 800,height = 800)
#累积事件发生风险
ggsurvplot(fit_30d,
           #fun = "event",
           fun = "cumhaz",
           data = mydata6,
           size = 0.5,
           risk.table=TRUE,
           palette = c("blue", "red"),
           pval = T, #P值
           pval.coord = c(0.2,0.3), #p值位置
           pval.size = 5, #p值文本大小的数字
           conf.int = F , #置信区间
           censor= F , #删失
           xlab ='Time in days',
           break.time.by=3, # 将x轴按照3为间隔进行切分
           risk.table.col = "strata",# 设置风险表的文字颜色
           #legend.labs = c("LDLC>=1.8","LDLC<1.8"),
           legend.title = "Legend", #表头
           #legend = c(0.9,0.2), # 指定图例位置
           xlim = c(0,30) , #横坐标间隔
           ylim = c(0,0.2)
)
dev.off()
