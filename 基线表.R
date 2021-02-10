library(tableone)
# 3.1.1基线表-结果与分组
myVARs <- c("CINA","CINB","CINC","CIND","death_30d","death_90d","death_365d",
            "death_follow_liu","dialysis_inhospitalevt")

catvars <- c("CINA","CINB","CINC","CIND","death_30d","death_90d","death_365d",
             "death_follow_liu","dialysis_inhospitalevt") 
tableone_groups <- CreateTableOne(vars = myVARs, #指定纳入的变量
                                  strata = "CONUT_group", #指定分组变量#若不指定则对总体分析做表#
                                  data = mydata6, #指定数据集
                                  factorVars = catvars) #指定分类变量
table1_groups <- print(x = tableone_groups, #指定表格
                       
                       contDigits = 2, #连续变量保留2位小数
                       catDigits =2,
                       pDigits = 3,                        
                       #catDigits、contDigits、pDigits三个参数设置分类变量、连续变量和P值保留几位小数位#
                       #nonnormal = nonnormalvars, #必不可少
                       showAllLevels = TRUE, #TRUE则显示所有分类变量水平的频数和百分比
                       noSpaces = TRUE, #删除用于对齐的空格
                       printToggle = FALSE) #不展示输出结果
tableone_groups 
write.xlsx(table1_groups,"CAD人群-TCBI.xlsx",sheetName="CIN和结局-基线表",col.name=T,
           row.names=T,append=T)
