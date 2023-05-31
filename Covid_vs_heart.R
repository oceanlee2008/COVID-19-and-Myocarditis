library(ggplot2)
library (readxl) 
library(tidyverse)
library(showtext)
library(limma)
library(ggrepel)
library(ggpubr)
library(ggsignif) 
library(limma)
library(Rtsne)
library(palmerpenguins)
library(ggpointdensity) 
library(viridis)
#绘图
library(ggExtra)

font_files()

showtext_auto(enable = TRUE)

font_add('Arial', 'Arial.ttc')

quartz(family='Arial')


dat000 <- read_excel("Health.xlsx")
dat001 <- read_excel("./XC/positive2.xlsx")
dat002 <- read_excel("./XC/positive2.xlsx",sheet="心肌炎 ")
# dat003 <- read_excel("positive1.xlsx",sheet="阳康")

dat0 <- dat001
dat1 <- dat002
# dat2 <- dat003
dat00 <- dat000

bio_select <- c('PATIENTAGE','白蛋白','总蛋白','肌酸激酶','血红蛋白','乳酸脱氢酶','超敏CRP','肌红蛋白','超敏肌钙蛋白I','肌钙蛋白I','C反应蛋白')
bio_index <- c('白蛋白','总蛋白','肌酸激酶','血红蛋白','乳酸脱氢酶')
bio_index2 <- c('超敏CRP','肌红蛋白','超敏肌钙蛋白I','肌钙蛋白I','C反应蛋白')
#bio_en <- (PATIENTAGE	ALB	TP	LHD	HB  PLT)

l0 <- c(dat000$PATIENTAGE)
x1 <- c(dat000$白蛋白)
x2 <- c(dat000$总蛋白)
x3 <-  c(dat000$肌酸激酶)
x4 <-  c(dat000$超敏CRP)
x5 <- c( dat000$C反应蛋白)
s1 <-  c(dat000$血红蛋白)
s2 <- c( dat000$肌红蛋白)
s3 <- c( dat000$超敏肌钙蛋白I)
s4 <-  c(dat000$乳酸脱氢酶)
# s5 <-  c(dat000$嗜碱性粒细胞比率)

name <- c(rep("Healthy", times=length(l0)))
gg0 <- data.frame(l0,x1,x2,x3,x4,x5,s1,s2,s3,s4)
names(gg0) <- c('PATIENTAGE','白蛋白','总蛋白','肌酸激酶','超敏CRP','C反应蛋白','血红蛋白','肌红蛋白','超敏肌钙蛋白I','乳酸脱氢酶')
PP0 <- data.frame(l0,x1,x2,x3,x4,x5,s1,s2,s3,s4)
nn <- min(c(length(l0),length(x1),length(x2),length(x3),length(x4),length(x5),length(s1),length(s2),length(s3),length(s4)))
label <- c(rep("Healthy", times=nn))
PP0 <- data.frame(na.omit(l0)[0:nn],na.omit(x1)[0:nn],na.omit(x2)[0:nn],na.omit(x3)[0:nn],na.omit(x4)[0:nn],na.omit(x5)[0:nn],na.omit(s1)[0:nn],na.omit(s2)[0:nn],na.omit(s3)[0:nn],na.omit(s4)[0:nn],label)

names(PP0) <- c('PATIENTAGE','白蛋白','总蛋白','肌酸激酶','超敏CRP','C反应蛋白','血红蛋白','肌红蛋白','超敏肌钙蛋白I','乳酸脱氢酶',"label")



l0 <- c(dat001$PATIENTAGE)
x1 <- c(dat001$白蛋白)
x2 <- c(dat001$总蛋白)
x3 <-  c(dat001$肌酸激酶)
x4 <-  c(dat001$超敏CRP)
x5 <- c( dat001$C反应蛋白)
s1 <-  c(dat001$血红蛋白)
s2 <- c( dat001$肌红蛋白)
s3 <- c( dat001$超敏肌钙蛋白I)
s4 <-  c(dat001$乳酸脱氢酶)
# s5 <-  c(dat000$嗜碱性粒细胞比率)

name <- c(rep("Healthy", times=length(l0)))
gg1 <- data.frame(l0,x1,x2,x3,x4,x5,s1,s2,s3,s4)
names(gg1) <- c('PATIENTAGE','白蛋白','总蛋白','肌酸激酶','超敏CRP','C反应蛋白','血红蛋白','肌红蛋白','超敏肌钙蛋白I','乳酸脱氢酶')
PP1 <- data.frame(l0,x1,x2,x3,x4,x5,s1,s2,s3,s4)
nn <- min(c(length(l0),length(x1),length(x2),length(x3),length(x4),length(x5),length(s1),length(s2),length(s3),length(s4)))
label <- c(rep("Healthy", times=nn))
PP1 <- data.frame(na.omit(l0)[0:nn],na.omit(x1)[0:nn],na.omit(x2)[0:nn],na.omit(x3)[0:nn],na.omit(x4)[0:nn],na.omit(x5)[0:nn],na.omit(s1)[0:nn],na.omit(s2)[0:nn],na.omit(s3)[0:nn],na.omit(s4)[0:nn],label)

names(PP1) <- c('PATIENTAGE','白蛋白','总蛋白','肌酸激酶','超敏CRP','C反应蛋白','血红蛋白','肌红蛋白','超敏肌钙蛋白I','乳酸脱氢酶',"label")


l0 <- c(dat002$PATIENTAGE)
x1 <- c(dat002$白蛋白)
x2 <- c(dat002$总蛋白)
x3 <-  c(dat002$肌酸激酶)
x4 <-  c(dat002$超敏CRP)
x5 <- c( dat002$C反应蛋白)
s1 <-  c(dat002$血红蛋白)
s2 <- c( dat002$肌红蛋白)
s3 <- c( dat002$超敏肌钙蛋白I)
s4 <-  c(dat002$乳酸脱氢酶)
# s5 <-  c(dat000$嗜碱性粒细胞比率)

name <- c(rep("Healthy", times=length(l0)))
gg2 <- data.frame(l0,x1,x2,x3,x4,x5,s1,s2,s3,s4,name)
names(gg2) <- c('PATIENTAGE','白蛋白','总蛋白','肌酸激酶','超敏CRP','C反应蛋白','血红蛋白','肌红蛋白','超敏肌钙蛋白I','乳酸脱氢酶')
PP2 <- data.frame(l0,x1,x2,x3,x4,x5,s1,s2,s3,s4)
nn <- min(c(length(l0),length(x1),length(x2),length(x3),length(x4),length(x5),length(s1),length(s2),length(s3),length(s4)))
label <- c(rep("Healthy", times=nn))
PP2 <- data.frame(na.omit(l0)[0:nn],na.omit(x1)[0:nn],na.omit(x2)[0:nn],na.omit(x3)[0:nn],na.omit(x4)[0:nn],na.omit(x5)[0:nn],na.omit(s1)[0:nn],na.omit(s2)[0:nn],na.omit(s3)[0:nn],na.omit(s4)[0:nn],label)

names(PP2) <- c('PATIENTAGE','白蛋白','总蛋白','肌酸激酶','超敏CRP','C反应蛋白','血红蛋白','肌红蛋白','超敏肌钙蛋白I','乳酸脱氢酶',"label")



gg <-rbind(gg0,gg1,gg2)
PP <-rbind(PP0,PP1,PP2)
bio_gg <- cc('PATIENTAGE','白蛋白','总蛋白','肌酸激酶','超敏CRP','C反应蛋白','血红蛋白','肌红蛋白','超敏肌钙蛋白I','乳酸脱氢酶',"label")
names(gg) <- bio_gg
write.table(gg,file="./Covid/pairdata_good.csv",quote=F,col.names = bio_gg,row.names = F,sep= "\t")

write.table(gg,file="./Covid/pairdata_222.csv",quote=F,col.names = bio_gg,row.names = F,sep= "\t")





pdf('./Covid/covid_vs2_heart_2023.3.24_summary.pdf')
# 
# ##############heatmap loop
# dat0 <- dat000
# dat1 <- dat001
# colnames(dat0)    # 返回列名
# loop <- intersect(colnames(dat0) ,colnames(dat1)) 
# new_loop <- c()
# testdata1 <- data.frame ()
# for (var in loop[10:length(loop)]){
#   cc <-  na.omit (dat1[var])
#   dd <-  na.omit (dat0[var])
#   print(lengths(cc))
#   if(lengths(cc) > 2000 ){if(lengths(dd) > 1000){
#     
#     new_loop <- c(new_loop, var)    
#   }
#   }
# }
# 
# 
# #  单独cancer into
# colnames(dat0)    # 返回列名
# loop <- colnames(dat1)    # 返回列名
# new_loop <- c()
# testdata1 <- data.frame ()
# for (var in loop[10:length(loop)]){
#   cc <-  na.omit (dat1[var])
#   print(lengths(cc))
#   if(lengths(cc) > 800 ){
#     
#     new_loop <- c(new_loop, var)    
#     
#   }
# }
# 
# 
# 
# new_loop
# testdata0 <- c()
# testdata1 <- c()
# testdata2 <- data.frame()
# for (var in new_loop){
#   print(var)
#   h1 <- na.omit (dat1[var])   ##清除空行
#   # if(h1[1,1] > 0){
#   var
#   #health patient
#   na.omit(dat0[var])
#   as.numeric(dat0$var)
#   testdata0 <- c(testdata0,na.omit(dat0[var]))
#   #cancer patient
#   na.omit(dat1[var])
#   as.numeric(dat1$var)
#   testdata1 <- c(testdata1,na.omit(dat1[var]))
#   
# }
# 
# #health patient
# testdata0 <- do.call(rbind,testdata0)
# testdata0 <- t(testdata0)
# testdata0 <- as.data.frame(testdata0)
# testdata0 <- as.data.frame(lapply(testdata0,as.numeric))
# #cancer patient
# testdata1 <- do.call(rbind,testdata1)
# testdata1 <- t(testdata1)
# testdata1 <- as.data.frame(testdata1)
# testdata1 <- as.data.frame(lapply(testdata1,as.numeric))
# 
# 
# 
# cor_data1 <- cor(testdata1, use = 'pairwise.complete.obs',method="kendall")
# cor_data0 <- cor(testdata0,method="kendall")
# 
# library(ComplexHeatmap)
# cor_data0 <- abs(cor(testdata0, use = 'pairwise.complete.obs',method="kendall"))
# n <- length(colnames(cor_data0))
# mat00  <- cor_data1[1:n-1,1:n-1]
# a <- 1:n
# for (ra in a){mat00[ra,ra]=0}
# mat00
# Heatmap3D(mat00, name = "mat", column_title = "Healthy heatmap")
# 
# library(ComplexHeatmap)
# cor_data1 <- abs(cor(testdata1, use = 'pairwise.complete.obs',method="kendall"))
# n <- length(colnames(cor_data1))
# mat11  <- cor_data1[1:n-1,1:n-1]
# a <- 1:n
# for (ra in a){mat11[ra,ra]=0}
# mat11
# Heatmap3D(mat11, name = "mat", column_title = "Covid19 heatmap")
# 
# 
# 
# library(ggcorrplot)
# 
# ggcorrplot(cor_data0,outline.color="white",title = "Health", 
#            type="upper",colors = c("#06DDF9", "#F7F7F7", "#E46726"),
#            lab = TRUE,lab_size = 2, tl.cex = 10, ggtheme = ggplot2::theme_void())
# 
# ggcorrplot(cor_data1,outline.color="white",title = "Lung Cancer", 
#            type="lower",colors = c("#06DDF9", "#F7F7F7", "#E46726"),
#            lab = TRUE,lab_size = 0.8, tl.cex = 5, ggtheme = ggplot2::theme_void())
# 
# 



P_number <- dat000["PATIENTID"]
P_number1 <- dat001["PATIENTID"]
P_number2 <- dat002["PATIENTID"]
number <- lengths(unique(P_number))+lengths(unique(P_number2))
cat("Patient number is",'Healthy',lengths(unique(P_number)),"Covid-19",lengths(unique(P_number1)),"心肌炎",lengths(unique(P_number2)))
cat("Data including patient number is",lengths(unique(P_number1))+lengths(unique(P_number2))+lengths(unique(P_number3)))





ip <- dat000[7000:12900,]
m_sex_num <- subset (ip,ip$PATIENTSEX=="男")
male_num <-lengths(unique(m_sex_num["PATIENTID"]))
cat("Health Male number is",lengths(unique(m_sex_num["PATIENTID"])))
f_sex_num <- subset (ip,ip$PATIENTSEX=="女")
female_num <-lengths(unique(f_sex_num["PATIENTID"]))
cat("Health Female number is",lengths(unique(f_sex_num["PATIENTID"])))

barplot(c(male_num,female_num),              # 柱子的高度
        main = male_num+female_num,
        names.arg = c('Male', 'Female'),    # 柱子的名称
        col =c( "#5A78F9","#CF6BF9"),          # 柱子的填充颜色
        border = '#E18CF2',  # 柱子的轮廓颜色
        xlab = "Sex",         # X轴标签
        ylab = "Health Cases",         # Y轴标签
        xlim = NULL,         # X轴取值范围
        cex.axis=1.5,cex.lab=1.5,
        ylim = c(0,2000),        # Y轴取值范围
        horiz = FALSE,       # 柱子是否为水平
        legend.text = NULL,  # 图例文本
        beside = FALSE,      # 柱子是否为平行放置
)



age <- c(ip["PATIENTID"],ip["PATIENTAGE"],ip["白蛋白"],ip["C反应蛋白"])
age20 <- subset (ip,ip$PATIENTAGE <= 20)
biochem20 <- age20["白蛋白"]
biochem20<-na.omit (biochem20)
age30 <- subset (ip,ip$PATIENTAGE > 20 & PATIENTAGE <= 30)
biochem30 <- age30["白蛋白"]
biochem30<-na.omit (biochem30)
age40 <- subset (ip,ip$PATIENTAGE > 30 & PATIENTAGE <= 40)
biochem40 <- age40["白蛋白"]
biochem40<-na.omit (biochem40)
age50 <- subset (ip,ip$PATIENTAGE > 40 & PATIENTAGE <= 50)
biochem50 <- age50["白蛋白"]
biochem50<-na.omit (biochem50)
age60 <- subset (ip,ip$PATIENTAGE > 50 & PATIENTAGE <= 60)
biochem60 <- age60["白蛋白"]
biochem60<-na.omit (biochem60)
age70 <- subset (ip,ip$PATIENTAGE > 60 & PATIENTAGE <= 70)
biochem70 <- age70["白蛋白"]
biochem70<-na.omit (biochem70)
age80 <- subset (ip,ip$PATIENTAGE > 70 & PATIENTAGE <= 80)
biochem80 <- age80["白蛋白"]
biochem80<-na.omit (biochem80)
age90 <- subset (ip,ip$PATIENTAGE > 80)
biochem90 <- age90["白蛋白"]
biochem90<-na.omit (biochem90)


biochem_data <- c(biochem20,biochem30,biochem40,biochem50,biochem60,biochem70,biochem80,biochem90)
as.data.frame(biochem20)
do.call(cbind, lapply(lapply(biochem_data, unlist), `length<-`, max(lengths(biochem_data))))
names(biochem_data)=c("<20",30,40,50,60,70,80,">90")
biochem_data
a <- lengths(unique(age20["PATIENTID"]))
b <- lengths(unique(age30["PATIENTID"]))
c <- lengths(unique(age40["PATIENTID"]))
d <- lengths(unique(age50["PATIENTID"]))
e <- lengths(unique(age60["PATIENTID"]))
f <- lengths(unique(age70["PATIENTID"]))
g <- lengths(unique(age80["PATIENTID"]))
h <- lengths(unique(age90["PATIENTID"]))


x <-c("<20",30,40,50,60,70,80,">90")
height <- c(a,b,c,d,e,f,g,h)



barplot(height,              # 柱子的高度
        names.arg = c('<20', '20~30', '30~40','40~50','50~60','60~70','70~80','>90'),    # 柱子的名称
        col = '#DBC41D',          # 柱子的填充颜色
        border = '#DBC41D',  # 柱子的轮廓颜色
        main = NULL,         # 柱状图主标题
        xlab = "Patient age",         # X轴标签
        ylab = "Cases",         # Y轴标签
        xlim = NULL,         # X轴取值范围
        ylim = c(0,1000),         # Y轴取值范围
        horiz = FALSE,       # 柱子是否为水平
        legend.text = NULL,  # 图例文本
        beside = FALSE,      # 柱子是否为平行放置
)




ip <- dat001
m_sex_num <- subset (ip,ip$PATIENTSEX=="男")
male_num1 <-lengths(m_sex_num["PATIENTID"])
cat("Covid Male number is",lengths(unique(m_sex_num["PATIENTID"])))
f_sex_num <- subset (ip,ip$PATIENTSEX=="女")
female_num1 <-lengths(f_sex_num["PATIENTID"])
cat("Covid Female number is",lengths(unique(f_sex_num["PATIENTID"])))

barplot(c(male_num1,female_num1),              # 柱子的高度
        main = lengths(P_number1),
        names.arg = c('Male', 'Female'),    # 柱子的名称
        col =c( "#5A78F9","#CF6BF9"),          # 柱子的填充颜色
        border = '#E18CF2',  # 柱子的轮廓颜色
        xlab = "Sex",         # X轴标签
        ylab = "Covid-19 Cases",         # Y轴标签
        xlim = NULL,         # X轴取值范围
        cex.axis=1.5,cex.lab=1.5,
        ylim =c(0,3000),         # Y轴取值范围
        horiz = FALSE,       # 柱子是否为水平
        legend.text = NULL,  # 图例文本
        beside = FALSE,      # 柱子是否为平行放置
)
age <- c(ip["PATIENTID"],ip["PATIENTAGE"],ip["白蛋白"],ip["C反应蛋白"])
age20 <- subset (ip,ip$PATIENTAGE <= 20)
biochem20 <- age20["白蛋白"]
biochem20<-na.omit (biochem20)
age30 <- subset (ip,ip$PATIENTAGE > 20 & PATIENTAGE <= 30)
biochem30 <- age30["白蛋白"]
biochem30<-na.omit (biochem30)
age40 <- subset (ip,ip$PATIENTAGE > 30 & PATIENTAGE <= 40)
biochem40 <- age40["白蛋白"]
biochem40<-na.omit (biochem40)
age50 <- subset (ip,ip$PATIENTAGE > 40 & PATIENTAGE <= 50)
biochem50 <- age50["白蛋白"]
biochem50<-na.omit (biochem50)
age60 <- subset (ip,ip$PATIENTAGE > 50 & PATIENTAGE <= 60)
biochem60 <- age60["白蛋白"]
biochem60<-na.omit (biochem60)
age70 <- subset (ip,ip$PATIENTAGE > 60 & PATIENTAGE <= 70)
biochem70 <- age70["白蛋白"]
biochem70<-na.omit (biochem70)
age80 <- subset (ip,ip$PATIENTAGE > 70 & PATIENTAGE <= 80)
biochem80 <- age80["白蛋白"]
biochem80<-na.omit (biochem80)
age90 <- subset (ip,ip$PATIENTAGE > 80)
biochem90 <- age90["白蛋白"]
biochem90<-na.omit (biochem90)


biochem_data <- c(biochem20,biochem30,biochem40,biochem50,biochem60,biochem70,biochem80,biochem90)
as.data.frame(biochem20)
do.call(cbind, lapply(lapply(biochem_data, unlist), `length<-`, max(lengths(biochem_data))))
names(biochem_data)=c("<20",30,40,50,60,70,80,">90")
biochem_data
a <- lengths( (age20["PATIENTID"]))
b <- lengths( (age30["PATIENTID"]))
c <- lengths( (age40["PATIENTID"]))
d <- lengths( (age50["PATIENTID"]))
e <- lengths( (age60["PATIENTID"]))
f <- lengths( (age70["PATIENTID"]))
g <- lengths( (age80["PATIENTID"]))
h <- lengths( (age90["PATIENTID"]))


x <-c("<20",30,40,50,60,70,80,">90")
height <- c(a,b,c,d,e,f,g,h)



barplot(height,              # 柱子的高度
        names.arg = c('<20', '20~30', '30~40','40~50','50~60','60~70','70~80','>90'),    # 柱子的名称
        col = '#DBC41D',          # 柱子的填充颜色
        border = '#DBC41D',  # 柱子的轮廓颜色
        main = NULL,         # 柱状图主标题
        xlab = "Patient age",         # X轴标签
        ylab = "Cases",         # Y轴标签
        xlim = NULL,         # X轴取值范围
        ylim = c(0,1000),         # Y轴取值范围
        horiz = FALSE,       # 柱子是否为水平
        legend.text = NULL,  # 图例文本
        beside = FALSE,      # 柱子是否为平行放置
)



ip <- dat002
m_sex_num <- subset (ip,ip$PATIENTSEX=="男")
male_num2 <-lengths(unique(m_sex_num["PATIENTID"]))
cat("Male patient number is",lengths(unique(m_sex_num["PATIENTID"])))
f_sex_num <- subset (ip,ip$PATIENTSEX=="女")
female_num2 <-lengths(unique(f_sex_num["PATIENTID"]))
cat("Female patient number is",lengths(unique(f_sex_num["PATIENTID"])))

barplot(c(male_num2,female_num2),              # 柱子的高度
        main = lengths(unique(P_number2)),
        names.arg = c('Male', 'Female'),    # 柱子的名称
        col =c( "#5A78F9","#CF6BF9"),          # 柱子的填充颜色
        border = '#E18CF2',  # 柱子的轮廓颜色
        xlab = "Sex",         # X轴标签
        ylab = "Myocarditis Cases",         # Y轴标签
        xlim = NULL,         # X轴取值范围
        ylim = c(0,300),          # Y轴取值范围
        cex.axis=1.5,cex.lab=1.5,
        horiz = FALSE,       # 柱子是否为水平
        legend.text = NULL,  # 图例文本
        beside = FALSE,      # 柱子是否为平行放置
)

age <- c(ip["PATIENTID"],ip["PATIENTAGE"],ip["白蛋白"],ip["C反应蛋白"])
age20 <- subset (ip,ip$PATIENTAGE <= 20)
biochem20 <- age20["白蛋白"]
biochem20<-na.omit (biochem20)
age30 <- subset (ip,ip$PATIENTAGE > 20 & PATIENTAGE <= 30)
biochem30 <- age30["白蛋白"]
biochem30<-na.omit (biochem30)
age40 <- subset (ip,ip$PATIENTAGE > 30 & PATIENTAGE <= 40)
biochem40 <- age40["白蛋白"]
biochem40<-na.omit (biochem40)
age50 <- subset (ip,ip$PATIENTAGE > 40 & PATIENTAGE <= 50)
biochem50 <- age50["白蛋白"]
biochem50<-na.omit (biochem50)
age60 <- subset (ip,ip$PATIENTAGE > 50 & PATIENTAGE <= 60)
biochem60 <- age60["白蛋白"]
biochem60<-na.omit (biochem60)
age70 <- subset (ip,ip$PATIENTAGE > 60 & PATIENTAGE <= 70)
biochem70 <- age70["白蛋白"]
biochem70<-na.omit (biochem70)
age80 <- subset (ip,ip$PATIENTAGE > 70 & PATIENTAGE <= 80)
biochem80 <- age80["白蛋白"]
biochem80<-na.omit (biochem80)
age90 <- subset (ip,ip$PATIENTAGE > 80)
biochem90 <- age90["白蛋白"]
biochem90<-na.omit (biochem90)


biochem_data <- c(biochem20,biochem30,biochem40,biochem50,biochem60,biochem70,biochem80,biochem90)
as.data.frame(biochem20)
do.call(cbind, lapply(lapply(biochem_data, unlist), `length<-`, max(lengths(biochem_data))))
names(biochem_data)=c("<20",30,40,50,60,70,80,">90")
biochem_data
a <- lengths(unique(age20["PATIENTID"]))
b <- lengths(unique(age30["PATIENTID"]))
c <- lengths(unique(age40["PATIENTID"]))
d <- lengths(unique(age50["PATIENTID"]))
e <- lengths(unique(age60["PATIENTID"]))
f <- lengths(unique(age70["PATIENTID"]))
g <- lengths(unique(age80["PATIENTID"]))
h <- lengths(unique(age90["PATIENTID"]))


x <-c("<20",30,40,50,60,70,80,">90")
height <- c(a,b,c,d,e,f,g,h)



barplot(height,              # 柱子的高度
        names.arg = c('<20', '20~30', '30~40','40~50','50~60','60~70','70~80','>90'),    # 柱子的名称
        col = '#DBC41D',          # 柱子的填充颜色
        border = '#DBC41D',  # 柱子的轮廓颜色
        main = NULL,         # 柱状图主标题
        xlab = "Patient age",         # X轴标签
        ylab = "Cases",         # Y轴标签
        xlim = NULL,         # X轴取值范围
        ylim = c(0,200),         # Y轴取值范围
        horiz = FALSE,       # 柱子是否为水平
        legend.text = NULL,  # 图例文本
        beside = FALSE,      # 柱子是否为平行放置
)






# dat001 <- read_excel("baby_Health.xlsx")


library(LSD)
library(hablar)



a=plot(1:5, 1:5, type = "n",ann = F, bty = "n", xaxt = "n", yaxt ="n")
text(3,3, "白蛋白vs球蛋白",cex=3.5)

#_____________________ALB and GLB relationship_________________________________________________________________________
ip <- dat000
biochem=data.frame(ip["PATIENTAGE"],ip["总蛋白"],ip["PATIENTSEX"],ip["白蛋白"])
biochem<-biochem[complete.cases(biochem),]

d2 <-  as.numeric(na.omit(biochem$白蛋白))
d1 <-  as.numeric(na.omit(biochem$总蛋白))
xx  <- d1-d2
yy = as.numeric(biochem$白蛋白)
heatscatter(xx,yy,colpal=c(  "purple","#5A78F9", "#46B1F2" ,"#FFF800","red"),main="Health",
            cex=2,add.contour=TRUE, cex.axis=1.5,cex.lab=1.5, 
            cor=FALSE,xlab="GLB (mg/mL)",ylab = "ALB (mg/mL)")

boxplot(yy
        ,xlab = "Patient",         # X轴标签
        col="white",
        border="#40E6D5",
        ylab = "白蛋白"        # Y轴标签
)
boxplot(xx
        ,xlab = "Patient",  
        horizontal = TRUE,
        col="white",
        border="#FFC09F",
        
        ylab = "mg/mL"        # Y轴标签
        
)


index = yy
Group= xx
dat = data.frame(Group,index)


p4 <- ggplot(data = dat, mapping = aes(x=Group, y=index)) +
  geom_pointdensity(cex=5) +
  stat_smooth(method=loess)+  
  scale_color_distiller(palette = "Spectral") +
  
  geom_density2d(size = 0.3,color="pink") +
  geom_abline(intercept=0,slope=1.2,color = "red" )+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent",colour = "black"),legend.position="none",
        plot.title=element_text(size = 12),axis.text.x=element_text(size=12,angle=0),axis.text.y=element_text(size=12),axis.title.x=element_text(size = 16),axis.title.y=element_text(size=16))+
  labs(main = 'Heathy',x="GLB (mg/mL)", y= 'ALB-healthy')
ggplot()+theme_classic()
#在散点图上添加密度曲线
# ggExtra::ggMarginal(p4, type = "density",  #指定添加类型
#                     xparams=list(fill = "green",alpha=0.5),  #指定颜色
#                     yparams = list(fill="orange",alpha=0.5),  #指定颜色
# )

ggMarginal(p4, type="densigram",xparams=list(fill = "green",color="#54DB05",alpha=0.5,size=0.5),  #指定颜色
           yparams = list(fill="orange",color="#F4A116",alpha=0.5,size=0.5))






ip <- dat001
biochem=data.frame(ip["PATIENTAGE"],ip["总蛋白"],ip["PATIENTSEX"],ip["白蛋白"])
biochem<-biochem[complete.cases(biochem),]

d2 <-  as.numeric(na.omit(biochem$白蛋白))
d1 <-  as.numeric(na.omit(biochem$总蛋白))
xx  <- d1-d2
yy = as.numeric(biochem$白蛋白)
heatscatter(xx,yy,colpal=c(  "purple","#5A78F9", "#46B1F2" ,"#FFF800","red"),main="Covid-19",
            cex=2,add.contour=TRUE, cex.axis=1.5,cex.lab=1.5, 
            cor=FALSE,xlab="GLB (mg/mL)",ylab = "ALB (mg/mL)")

boxplot(yy
        ,xlab = "Patient",         # X轴标签
        col="white",
        border="#40E6D5",
        ylab = "白蛋白"        # Y轴标签
)
boxplot(xx
        ,xlab = "Patient",  
        horizontal = TRUE,
        col="white",
        border="#FFC09F",
        
        ylab = "mg/mL"        # Y轴标签
        
)


index = yy
Group= xx
dat = data.frame(Group,index)


p4 <- ggplot(data = dat, mapping = aes(x=Group, y=index)) +
  geom_pointdensity( cex=5) +
  stat_smooth(method=loess)+  geom_rug(color='pink')+
  scale_color_distiller(palette = "Spectral", direction = -1) +
  
  geom_density2d(size = 0.3,color="pink") +
  geom_abline(intercept=0,slope=1.2,color = "red" )+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent",colour = "black"),legend.position="none",
        plot.title=element_text(size = 12),axis.text.x=element_text(size=12,angle=0),axis.text.y=element_text(size=12),axis.title.x=element_text(size = 16),axis.title.y=element_text(size=16))+
  labs(x="GLB (mg/mL)", y= 'ALB-Covid')
ggplot()+theme_classic()
#在散点图上添加密度曲线
# ggExtra::ggMarginal(p4, type = "density",  #指定添加类型
#                     xparams=list(fill = "green",alpha=0.5),  #指定颜色
#                     yparams = list(fill="orange",alpha=0.5),  #指定颜色
# )

ggMarginal(p4, type="densigram",xparams=list(fill = "green",color="#54DB05",alpha=0.5,size=0.5),  #指定颜色
           yparams = list(fill="orange",color="#F4A116",alpha=0.5,size=0.5))






ip <- dat002
biochem=data.frame(ip["PATIENTAGE"],ip["总蛋白"],ip["PATIENTSEX"],ip["白蛋白"])
biochem<-biochem[complete.cases(biochem),]

d2 <-  as.numeric(na.omit(biochem$白蛋白))
d1 <-  as.numeric(na.omit(biochem$总蛋白))
xx  <- d1-d2
yy = as.numeric(biochem$白蛋白)
p <- heatscatter(xx,yy,colpal=c(  "purple","#5A78F9", "#46B1F2" ,"#FFF800","red"),main="Myocarditis",
                 cex=2,add.contour=TRUE, cex.axis=1.5,cex.lab=1.5, 
                 cor=FALSE,xlab="GLB (mg/mL)",ylab = "ALB (mg/mL)")



boxplot(yy
        ,xlab = "Patient",         # X轴标签
        col="white",
        border="#40E6D5",
        ylab = "白蛋白"        # Y轴标签
)
boxplot(xx
        ,xlab = "Patient",  
        horizontal = TRUE,
        col="white",
        border="#FFC09F",
        
        ylab = "mg/mL"        # Y轴标签
        
)



index = yy
Group= xx
dat = data.frame(Group,index)

p4 <- ggplot(data = dat, mapping = aes(x=Group, y=index)) +
  geom_pointdensity(cex=5) +
  stat_smooth(method=loess)+  
  scale_color_viridis() +geom_rug(color='red')+
  
  geom_density2d(size = 0.3,color="pink") +
  geom_abline(intercept=0,slope=1.2,color = "red" )+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent",colour = "black"),legend.position="none",
        plot.title=element_text(size = 12),axis.text.x=element_text(size=12,angle=0),axis.text.y=element_text(size=12),axis.title.x=element_text(size = 16),axis.title.y=element_text(size=16))+
  labs(x="GLB (mg/mL)", y= 'ALB-ILP')


ggplot()+theme_classic()
ggMarginal(p4, type="densigram",xparams=list(fill = "green",color="#54DB05",alpha=0.5,size=0.5),  #指定颜色
           yparams = list(fill="orange",color="#F4A116",alpha=0.5,size=0.5))







ip <- dat003
biochem=data.frame(ip["PATIENTAGE"],ip["总蛋白"],ip["PATIENTSEX"],ip["白蛋白"])
biochem<-biochem[complete.cases(biochem),]

d2 <-  as.numeric(na.omit(biochem$白蛋白))
d1 <-  as.numeric(na.omit(biochem$总蛋白))
xx  <- d1-d2
yy = as.numeric(biochem$白蛋白)
heatscatter(xx,yy,colpal=c(  "purple","#5A78F9", "#46B1F2" ,"#FFF800","red"),main="Covid-recovery",
            cex=2,add.contour=TRUE, cex.axis=1.5,cex.lab=1.5, 
            cor=FALSE,xlab="GLB (mg/mL)",ylab = "ALB (mg/mL)")

boxplot(yy
        ,xlab = "Patient",         # X轴标签
        col="white",
        border="#40E6D5",
        ylab = "白蛋白"        # Y轴标签
)
boxplot(xx
        ,xlab = "Patient",  
        horizontal = TRUE,
        col="white",
        border="#FFC09F",
        
        ylab = "mg/mL"        # Y轴标签
        
)


index = yy
Group= xx
dat = data.frame(Group,index)


par()
p4 <- ggplot(data = dat, mapping = aes(x=Group, y=index)) +
  geom_pointdensity(cex=5) +
  stat_smooth(method=loess)+   
  scale_color_viridis() +
  
  geom_density2d(size = 0.3,color="pink") +
  geom_abline(intercept=0,slope=1.2,color = "red" )+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent",colour = "black"),legend.position="none",
        plot.title=element_text(size = 12),axis.text.x=element_text(size=12,angle=0),axis.text.y=element_text(size=12),axis.title.x=element_text(size = 16),axis.title.y=element_text(size=16))+
  labs(x="GLB (mg/mL)", y= 'ALB(Covid-ILP)')

#在散点图上添加密度曲线
# ggExtra::ggMarginal(p4, type = "density",  #指定添加类型
#                     xparams=list(fill = "green",alpha=0.5),  #指定颜色
#                     yparams = list(fill="orange",alpha=0.5),  #指定颜色
# )
ggplot()+theme_classic()
ggMarginal(p4, type="densigram",xparams=list(fill = "green",color="#54DB05",alpha=0.5,size=0.5),  #指定颜色
           yparams = list(fill="orange",color="#F4A116",alpha=0.5,size=0.5))







M1 <-dat003
biochem=data.frame(M1["PATIENTAGE"],M1["总蛋白"],M1["白蛋白"])
biochem<-biochem[complete.cases(biochem),]

d2 <-  as.numeric(na.omit(biochem$白蛋白))
d1 <-  as.numeric(na.omit(biochem$总蛋白))
xx  <- d1-d2
yy = as.numeric(biochem$白蛋白)
p <- heatscatter(xx,yy,colpal=c(  "purple","#5A78F9", "#46B1F2" ,"#FFF800","red"),main="Myocarditis",
                 cex=2,add.contour=TRUE,cex.axis=1.5,cex.lab=1.5, 
                 cor=FALSE,xlab="TP (mg/mL)",ylab = "ALB (mg/mL)")



index = yy
Group= xx
dat = data.frame(Group,index)

p4 <- ggplot(data = dat, mapping = aes(x=Group, y=index)) +
  geom_pointdensity(cex=5) +
  stat_smooth(method=loess)+  
  scale_color_viridis() +
  
  geom_density2d(size = 0.3,color="pink") +
  geom_abline(intercept=0,slope=1.2,color = "red" )+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent",colour = "black"),legend.position="none",
        plot.title=element_text(size = 12),axis.text.x=element_text(size=12,angle=0),axis.text.y=element_text(size=12),axis.title.x=element_text(size = 16),axis.title.y=element_text(size=16))+
  labs(x='TP (mg/mL)', y= 'ALB (mg/mL)')

ggplot()+theme_classic()
#在散点图上添加密度曲线
ggExtra::ggMarginal(p, type = "density",  #指定添加类型
                    xparams=list(fill = "green"),  #指定颜色
                    yparams = list(fill="orange"),  #指定颜色
)


index = yy
Group= xx
dat = data.frame(Group,index)

p4 <- ggplot(data = dat, mapping = aes(x=Group, y=index)) +
  geom_pointdensity( cex=5) +
  stat_smooth(method=loess)+  
  scale_color_viridis() +
  
  geom_density2d(size = 0.3,color="pink") +
  geom_abline(intercept=0,slope=1.2,color = "red" )+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent",colour = "black"),legend.position="none",
        plot.title=element_text(size = 12),axis.text.x=element_text(size=12,angle=0),axis.text.y=element_text(size=12),axis.title.x=element_text(size = 16),axis.title.y=element_text(size=16))+
  labs(x="GLB (mg/mL)", y= 'ALB')

#在散点图上添加密度曲线
# ggExtra::ggMarginal(p4, type = "density",  #指定添加类型
#                     xparams=list(fill = "green",alpha=0.5),  #指定颜色
#                     yparams = list(fill="orange",alpha=0.5),  #指定颜色
# )
ggplot()+theme_classic()
ggMarginal(p4, type="densigram",xparams=list(fill = "green",color="#54DB05",alpha=0.5,size=0.5),  #指定颜色
           yparams = list(fill="orange",color="#F4A116",alpha=0.5,size=0.5))

a=plot(1:5, 1:5, type = "n",ann = F, bty = "n", xaxt = "n", yaxt ="n")
text(3,3, "总蛋白vs年龄",cex=5)


#_____________________总蛋白 and Age relationship_________________________________________________________________________
ip <- dat000
biochem=data.frame(ip["PATIENTAGE"],ip["总蛋白"],ip["PATIENTSEX"],ip["白蛋白"])
biochem<-biochem[complete.cases(biochem),]

d2 <-  as.numeric(na.omit(biochem$总蛋白))
d1 <-  as.numeric(na.omit(biochem$PATIENTAGE))
xx  <- d1
yy = as.numeric(biochem$总蛋白)
heatscatter(xx,yy,colpal=c(  "purple","#5A78F9", "#46B1F2" ,"#FFF800","red"),main="Health",
            cex=2,add.contour=TRUE, cex.axis=1.5,cex.lab=1.5, 
            cor=FALSE,xlab="Age",ylab = "TP (mg/mL)")

boxplot(yy
        ,xlab = "Patient",         # X轴标签
        col="white",
        border="#40E6D5",
        ylab = "总蛋白"         # Y轴标签
)
boxplot(xx
        ,xlab = "Patient",  
        horizontal = TRUE,
        col="white",
        border="#FFC09F",
        
        ylab = "mg/mL"        # Y轴标签
        
)


index = yy
Group= xx
dat = data.frame(Group,index)


p4 <- ggplot(data = dat, mapping = aes(x=Group, y=index)) +
  geom_pointdensity(cex=5) +
  stat_smooth(method=loess)+  
  scale_color_viridis() +
  
  geom_density2d(size = 0.3,color="pink") +
  geom_abline(intercept=0,slope=1.2,color = "red" )+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent",colour = "black"),legend.position="none",
        plot.title=element_text(size = 12),axis.text.x=element_text(size=12,angle=0),axis.text.y=element_text(size=12),axis.title.x=element_text(size = 16),axis.title.y=element_text(size=16))+
  labs(main = 'Heathy',x="Age", y= 'TP-healthy')
ggplot()+theme_classic()
#在散点图上添加密度曲线
# ggExtra::ggMarginal(p4, type = "density",  #指定添加类型
#                     xparams=list(fill = "green",alpha=0.5),  #指定颜色
#                     yparams = list(fill="orange",alpha=0.5),  #指定颜色
# )

ggMarginal(p4, type="densigram",xparams=list(fill = "green",color="#54DB05",alpha=0.5,size=0.5),  #指定颜色
           yparams = list(fill="orange",color="#F4A116",alpha=0.5,size=0.5))






ip <- dat001
biochem=data.frame(ip["PATIENTAGE"],ip["总蛋白"],ip["PATIENTSEX"],ip["白蛋白"])
biochem<-biochem[complete.cases(biochem),]

d2 <-  as.numeric(na.omit(biochem$总蛋白))
d1 <-  as.numeric(na.omit(biochem$PATIENTAGE))
xx  <- d1
yy = as.numeric(biochem$总蛋白)
heatscatter(xx,yy,colpal=c(  "purple","#5A78F9", "#46B1F2" ,"#FFF800","red"),main="Covid-19",
            cex=2,add.contour=TRUE, cex.axis=1.5,cex.lab=1.5, 
            cor=FALSE,xlab="Age",ylab = "TP (mg/mL)")

boxplot(yy
        ,xlab = "Patient",         # X轴标签
        col="white",
        border="#40E6D5",
        ylab = "总蛋白"         # Y轴标签
)
boxplot(xx
        ,xlab = "Patient",  
        horizontal = TRUE,
        col="white",
        border="#FFC09F",
        
        ylab = "mg/mL"        # Y轴标签
        
)


index = yy
Group= xx
dat = data.frame(Group,index)


p4 <- ggplot(data = dat, mapping = aes(x=Group, y=index)) +
  geom_pointdensity( cex=5) +
  stat_smooth(method=loess)+  
  scale_color_viridis() +
  
  geom_density2d(size = 0.3,color="pink") +
  geom_abline(intercept=0,slope=1.2,color = "red" )+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent",colour = "black"),legend.position="none",
        plot.title=element_text(size = 12),axis.text.x=element_text(size=12,angle=0),axis.text.y=element_text(size=12),axis.title.x=element_text(size = 16),axis.title.y=element_text(size=16))+
  labs(x="Age", y= 'TP-Covid')
ggplot()+theme_classic()
#在散点图上添加密度曲线
# ggExtra::ggMarginal(p4, type = "density",  #指定添加类型
#                     xparams=list(fill = "green",alpha=0.5),  #指定颜色
#                     yparams = list(fill="orange",alpha=0.5),  #指定颜色
# )

ggMarginal(p4, type="densigram",xparams=list(fill = "green",color="#54DB05",alpha=0.5,size=0.5),  #指定颜色
           yparams = list(fill="orange",color="#F4A116",alpha=0.5,size=0.5))






ip <- dat002
biochem=data.frame(ip["PATIENTAGE"],ip["总蛋白"],ip["PATIENTSEX"],ip["白蛋白"])
biochem<-biochem[complete.cases(biochem),]

d2 <-  as.numeric(na.omit(biochem$总蛋白))
d1 <-  as.numeric(na.omit(biochem$PATIENTAGE))
xx  <- d1
yy = as.numeric(biochem$总蛋白)
p <- heatscatter(xx,yy,colpal=c(  "purple","#5A78F9", "#46B1F2" ,"#FFF800","red"),main="Myocarditis",
                 cex=2,add.contour=TRUE, cex.axis=1.5,cex.lab=1.5, 
                 cor=FALSE,xlab="Age",ylab = "TP (mg/mL)")



boxplot(yy
        ,xlab = "Patient",         # X轴标签
        col="white",
        border="#40E6D5",
        ylab = "总蛋白"         # Y轴标签
)
boxplot(xx
        ,xlab = "Patient",  
        horizontal = TRUE,
        col="white",
        border="#FFC09F",
        
        ylab = "mg/mL"        # Y轴标签
        
)



index = yy
Group= xx
dat = data.frame(Group,index)

p4 <- ggplot(data = dat, mapping = aes(x=Group, y=index)) +
  geom_pointdensity(cex=5) +
  stat_smooth(method=loess)+  
  scale_color_viridis() +
  
  geom_density2d(size = 0.3,color="pink") +
  geom_abline(intercept=0,slope=1.2,color = "red" )+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent",colour = "black"),legend.position="none",
        plot.title=element_text(size = 12),axis.text.x=element_text(size=12,angle=0),axis.text.y=element_text(size=12),axis.title.x=element_text(size = 16),axis.title.y=element_text(size=16))+
  labs(x="Age", y= 'TP-Myocarditis')


ggplot()+theme_classic()
ggMarginal(p4, type="densigram",xparams=list(fill = "green",color="#54DB05",alpha=0.5,size=0.5),  #指定颜色
           yparams = list(fill="orange",color="#F4A116",alpha=0.5,size=0.5))




a=plot(1:5, 1:5, type = "n",ann = F, bty = "n", xaxt = "n", yaxt ="n")
text(3,3, "白蛋白vs年龄",cex=5)


#_____________________白蛋白 and Age relationship_________________________________________________________________________
ip <- dat000
biochem=data.frame(ip["PATIENTAGE"],ip["白蛋白"],ip["PATIENTSEX"],ip["白蛋白"])
biochem<-biochem[complete.cases(biochem),]

d2 <-  as.numeric(na.omit(biochem$白蛋白))
d1 <-  as.numeric(na.omit(biochem$PATIENTAGE))
xx  <- d1
yy = as.numeric(biochem$白蛋白)
heatscatter(xx,yy,colpal=c(  "purple","#5A78F9", "#46B1F2" ,"#FFF800","red"),main="Health",
            cex=2,add.contour=TRUE, cex.axis=1.5,cex.lab=1.5, 
            cor=FALSE,xlab="Age",ylab = "ALB (mg/mL)")

boxplot(yy
        ,xlab = "Patient",         # X轴标签
        col="white",
        border="#40E6D5",
        ylab = "白蛋白"         # Y轴标签
)
boxplot(xx
        ,xlab = "Patient",  
        horizontal = TRUE,
        col="white",
        border="#FFC09F",
        
        ylab = "mg/mL"        # Y轴标签
        
)


index = yy
Group= xx
dat = data.frame(Group,index)


p4 <- ggplot(data = dat, mapping = aes(x=Group, y=index)) +
  geom_pointdensity(cex=5) +
  stat_smooth(method=loess)+  
  scale_color_viridis() +
  
  geom_density2d(size = 0.3,color="pink") +
  geom_abline(intercept=0,slope=1.2,color = "red" )+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent",colour = "black"),legend.position="none",
        plot.title=element_text(size = 12),axis.text.x=element_text(size=12,angle=0),axis.text.y=element_text(size=12),axis.title.x=element_text(size = 16),axis.title.y=element_text(size=16))+
  labs(main = 'Heathy',x="Age", y= 'ALB-healthy')
ggplot()+theme_classic()
#在散点图上添加密度曲线
# ggExtra::ggMarginal(p4, type = "density",  #指定添加类型
#                     xparams=list(fill = "green",alpha=0.5),  #指定颜色
#                     yparams = list(fill="orange",alpha=0.5),  #指定颜色
# )

ggMarginal(p4, type="densigram",xparams=list(fill = "green",color="#54DB05",alpha=0.5,size=0.5),  #指定颜色
           yparams = list(fill="orange",color="#F4A116",alpha=0.5,size=0.5))






ip <- dat001
biochem=data.frame(ip["PATIENTAGE"],ip["白蛋白"],ip["PATIENTSEX"],ip["白蛋白"])
biochem<-biochem[complete.cases(biochem),]

d2 <-  as.numeric(na.omit(biochem$白蛋白))
d1 <-  as.numeric(na.omit(biochem$PATIENTAGE))
xx  <- d1
yy = as.numeric(biochem$白蛋白)
heatscatter(xx,yy,colpal=c(  "purple","#5A78F9", "#46B1F2" ,"#FFF800","red"),main="Covid-19",
            cex=2,add.contour=TRUE, cex.axis=1.5,cex.lab=1.5, 
            cor=FALSE,xlab="Age",ylab = "ALB (mg/mL)")

boxplot(yy
        ,xlab = "Patient",         # X轴标签
        col="white",
        border="#40E6D5",
        ylab = "白蛋白"         # Y轴标签
)
boxplot(xx
        ,xlab = "Patient",  
        horizontal = TRUE,
        col="white",
        border="#FFC09F",
        
        ylab = "mg/mL"        # Y轴标签
        
)


index = yy
Group= xx
dat = data.frame(Group,index)


p4 <- ggplot(data = dat, mapping = aes(x=Group, y=index)) +
  geom_pointdensity( cex=5) +
  stat_smooth(method=loess)+  
  scale_color_viridis() +
  
  geom_density2d(size = 0.3,color="pink") +
  geom_abline(intercept=0,slope=1.2,color = "red" )+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent",colour = "black"),legend.position="none",
        plot.title=element_text(size = 12),axis.text.x=element_text(size=12,angle=0),axis.text.y=element_text(size=12),axis.title.x=element_text(size = 16),axis.title.y=element_text(size=16))+
  labs(x="Age", y= 'ALB-Covid')
ggplot()+theme_classic()
#在散点图上添加密度曲线
# ggExtra::ggMarginal(p4, type = "density",  #指定添加类型
#                     xparams=list(fill = "green",alpha=0.5),  #指定颜色
#                     yparams = list(fill="orange",alpha=0.5),  #指定颜色
# )

ggMarginal(p4, type="densigram",xparams=list(fill = "green",color="#54DB05",alpha=0.5,size=0.5),  #指定颜色
           yparams = list(fill="orange",color="#F4A116",alpha=0.5,size=0.5))






ip <- dat002
biochem=data.frame(ip["PATIENTAGE"],ip["白蛋白"],ip["PATIENTSEX"],ip["白蛋白"])
biochem<-biochem[complete.cases(biochem),]

d2 <-  as.numeric(na.omit(biochem$白蛋白))
d1 <-  as.numeric(na.omit(biochem$PATIENTAGE))
xx  <- d1
yy = as.numeric(biochem$白蛋白)
p <- heatscatter(xx,yy,colpal=c(  "purple","#5A78F9", "#46B1F2" ,"#FFF800","red"),main="Myocarditis",
                 cex=2,add.contour=TRUE, cex.axis=1.5,cex.lab=1.5, 
                 cor=FALSE,xlab="Age",ylab = "ALB (mg/mL)")



boxplot(yy
        ,xlab = "Patient",         # X轴标签
        col="white",
        border="#40E6D5",
        ylab = "白蛋白"         # Y轴标签
)
boxplot(xx
        ,xlab = "Patient",  
        horizontal = TRUE,
        col="white",
        border="#FFC09F",
        
        ylab = "mg/mL"        # Y轴标签
        
)



index = yy
Group= xx
dat = data.frame(Group,index)

p4 <- ggplot(data = dat, mapping = aes(x=Group, y=index)) +
  geom_pointdensity(cex=5) +
  stat_smooth(method=loess)+  
  scale_color_viridis() +
  
  geom_density2d(size = 0.3,color="pink") +
  geom_abline(intercept=0,slope=1.2,color = "red" )+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent",colour = "black"),legend.position="none",
        plot.title=element_text(size = 12),axis.text.x=element_text(size=12,angle=0),axis.text.y=element_text(size=12),axis.title.x=element_text(size = 16),axis.title.y=element_text(size=16))+
  labs(x="Age", y= 'ALB-Myocarditis')


ggplot()+theme_classic()
ggMarginal(p4, type="densigram",xparams=list(fill = "green",color="#54DB05",alpha=0.5,size=0.5),  #指定颜色
           yparams = list(fill="orange",color="#F4A116",alpha=0.5,size=0.5))



a=plot(1:5, 1:5, type = "n",ann = F, bty = "n", xaxt = "n", yaxt ="n")
text(3,3, "淋巴细胞比率vs中心粒细胞比率",cex=2)


#_____________________淋巴细胞比率 and 中性粒细胞比率 relationship_________________________________________________________________________
ip <- dat000
biochem=data.frame(ip["中性粒细胞比率"],ip["淋巴细胞比率"],ip["PATIENTSEX"],ip["淋巴细胞比率"])
biochem<-biochem[complete.cases(biochem),]

d2 <-  as.numeric(na.omit(biochem$淋巴细胞比率))
d1 <-  as.numeric(na.omit(biochem$中性粒细胞比率))
xx  <- d1
yy = as.numeric(biochem$淋巴细胞比率)
heatscatter(xx,yy,colpal=c(  "purple","#5A78F9", "#46B1F2" ,"#FFF800","red"),main="Health",
            cex=2,add.contour=TRUE, cex.axis=1.5,cex.lab=1.5, 
            cor=FALSE,xlab="中性粒细胞比率",ylab = "淋巴细胞比率")

boxplot(yy
        ,xlab = "Patient",         # X轴标签
        col="white",
        border="#40E6D5",
        ylab = "淋巴细胞比率"         # Y轴标签
)
boxplot(xx
        ,xlab = "Patient",  
        horizontal = TRUE,
        col="white",
        border="#FFC09F",
        
        ylab = "mg/mL"        # Y轴标签
        
)


index = yy
Group= xx
dat = data.frame(Group,index)


p4 <- ggplot(data = dat, mapping = aes(x=Group, y=index)) +
  geom_pointdensity(cex=5) +
  stat_smooth(method=lm)+  
  scale_color_viridis() +
  
  geom_density2d(size = 0.3,color="pink") +
  geom_abline(intercept=0,slope=1.2,color = "red" )+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent",colour = "black"),legend.position="none",
        plot.title=element_text(size = 12),axis.text.x=element_text(size=12,angle=0),axis.text.y=element_text(size=12),axis.title.x=element_text(size = 16),axis.title.y=element_text(size=16))+
  labs(main = 'Heathy',x="中性粒细胞比率", y= '淋巴细胞比率-healthy')
ggplot()+theme_classic()
#在散点图上添加密度曲线
# ggExtra::ggMarginal(p4, type = "density",  #指定添加类型
#                     xparams=list(fill = "green",alpha=0.5),  #指定颜色
#                     yparams = list(fill="orange",alpha=0.5),  #指定颜色
# )

ggMarginal(p4, type="densigram",xparams=list(fill = "green",color="#54DB05",alpha=0.5,size=0.5),  #指定颜色
           yparams = list(fill="orange",color="#F4A116",alpha=0.5,size=0.5))






ip <- dat001
biochem=data.frame(ip["中性粒细胞比率"],ip["淋巴细胞比率"],ip["PATIENTSEX"],ip["淋巴细胞比率"])
biochem<-biochem[complete.cases(biochem),]

d2 <-  as.numeric(na.omit(biochem$淋巴细胞比率))
d1 <-  as.numeric(na.omit(biochem$中性粒细胞比率))
xx  <- d1
yy = as.numeric(biochem$淋巴细胞比率)
heatscatter(xx,yy,colpal=c(  "purple","#5A78F9", "#46B1F2" ,"#FFF800","red"),main="Covid-19",
            cex=2,add.contour=TRUE, cex.axis=1.5,cex.lab=1.5, 
            cor=FALSE,xlab="中性粒细胞比率",ylab = "淋巴细胞比率")

boxplot(yy
        ,xlab = "Patient",         # X轴标签
        col="white",
        border="#40E6D5",
        ylab = "淋巴细胞比率"         # Y轴标签
)
boxplot(xx
        ,xlab = "Patient",  
        horizontal = TRUE,
        col="white",
        border="#FFC09F",
        
        ylab = "mg/mL"        # Y轴标签
        
)


index = yy
Group= xx
dat = data.frame(Group,index)


p4 <- ggplot(data = dat, mapping = aes(x=Group, y=index)) +
  geom_pointdensity( cex=5) +
  stat_smooth(method=lm)+  
  scale_color_viridis() +
  
  geom_density2d(size = 0.3,color="pink") +
  geom_abline(intercept=0,slope=1.2,color = "red" )+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent",colour = "black"),legend.position="none",
        plot.title=element_text(size = 12),axis.text.x=element_text(size=12,angle=0),axis.text.y=element_text(size=12),axis.title.x=element_text(size = 16),axis.title.y=element_text(size=16))+
  labs(x="中性粒细胞比率", y= '淋巴细胞比率-Covid')
ggplot()+theme_classic()
#在散点图上添加密度曲线
# ggExtra::ggMarginal(p4, type = "density",  #指定添加类型
#                     xparams=list(fill = "green",alpha=0.5),  #指定颜色
#                     yparams = list(fill="orange",alpha=0.5),  #指定颜色
# )

ggMarginal(p4, type="densigram",xparams=list(fill = "green",color="#54DB05",alpha=0.5,size=0.5),  #指定颜色
           yparams = list(fill="orange",color="#F4A116",alpha=0.5,size=0.5))






ip <- dat002
biochem=data.frame(ip["中性粒细胞比率"],ip["淋巴细胞比率"],ip["PATIENTSEX"],ip["淋巴细胞比率"])
biochem<-biochem[complete.cases(biochem),]

d2 <-  as.numeric(na.omit(biochem$淋巴细胞比率))
d1 <-  as.numeric(na.omit(biochem$中性粒细胞比率))
xx  <- d1
yy = as.numeric(biochem$淋巴细胞比率)
p <- heatscatter(xx,yy,colpal=c(  "purple","#5A78F9", "#46B1F2" ,"#FFF800","red"),main="Myocarditis",
                 cex=2,add.contour=TRUE, cex.axis=1.5,cex.lab=1.5, 
                 cor=FALSE,xlab="中性粒细胞比率",ylab = "淋巴细胞比率")



boxplot(yy
        ,xlab = "Patient",         # X轴标签
        col="white",
        border="#40E6D5",
        ylab = "淋巴细胞比率"         # Y轴标签
)
boxplot(xx
        ,xlab = "Patient",  
        horizontal = TRUE,
        col="white",
        border="#FFC09F",
        
        ylab = "mg/mL"        # Y轴标签
        
)



index = yy
Group= xx
dat = data.frame(Group,index)

p4 <- ggplot(data = dat, mapping = aes(x=Group, y=index)) +
  geom_pointdensity(cex=5) +
  stat_smooth(method=lm)+  
  scale_color_viridis() +
  
  geom_density2d(size = 0.3,color="pink") +
  geom_abline(intercept=0,slope=1.2,color = "red" )+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent",colour = "black"),legend.position="none",
        plot.title=element_text(size = 12),axis.text.x=element_text(size=12,angle=0),axis.text.y=element_text(size=12),axis.title.x=element_text(size = 16),axis.title.y=element_text(size=16))+
  labs(x="中性粒细胞比率", y= '淋巴细胞比率-Myocarditis')


ggplot()+theme_classic()
ggMarginal(p4, type="densigram",xparams=list(fill = "green",color="#54DB05",alpha=0.5,size=0.5),  #指定颜色
           yparams = list(fill="orange",color="#F4A116",alpha=0.5,size=0.5))




a=plot(1:5, 1:5, type = "n",ann = F, bty = "n", xaxt = "n", yaxt ="n")
text(3,3, "淋巴细胞数vs超敏CRP",cex=2)


#_____________________淋巴细胞数 and 超敏CRP relationship_________________________________________________________________________
ip <- dat000
biochem <-data.frame(ip["超敏CRP"],ip["淋巴细胞数"])
biochem<-biochem[complete.cases(biochem),]

biochem1 <-data.frame(ip["C反应蛋白"],ip["淋巴细胞数"])

biochem1 <-biochem1[complete.cases(biochem1),]

d2 <-  as.numeric(na.omit(biochem$淋巴细胞数))
d22 <-  as.numeric(na.omit(biochem1$淋巴细胞数))
d1 <-  as.numeric(na.omit(biochem$超敏CRP))
d11 <-  as.numeric(na.omit(biochem1$C反应蛋白))
xx  <- c(d1,d11)
yy = c(d2,d22)
heatscatter(xx,yy,colpal=c(  "purple","#5A78F9", "#46B1F2" ,"#FFF800","red"),main="Health",
            cex=2,add.contour=TRUE, cex.axis=1.5,cex.lab=1.5, 
            cor=FALSE,xlab="超敏CRP",ylab = "淋巴细胞数", ylim=c(0,5))

boxplot(yy
        ,xlab = "Patient",         # X轴标签
        col="white",
        border="#40E6D5",
        ylab = "淋巴细胞数", ylim=c(0,5)         # Y轴标签
)
boxplot(xx
        ,xlab = "Patient",  
        horizontal = TRUE,
        col="white",
        border="#FFC09F",
        
        ylab = "mg/mL"        # Y轴标签
        
)


index = yy
Group= xx
dat = data.frame(Group,index)


p4 <- ggplot(data = dat, mapping = aes(x=Group, y=index)) +
  geom_pointdensity(cex=5) +
  stat_smooth(method=loess)+  
  scale_color_viridis() +
  
  geom_density2d(size = 0.3,color="pink") +
  geom_abline(intercept=0,slope=1.2,color = "red" )+
  theme_classic()+ylim(0,5)+
  theme(panel.background = element_rect(fill = "transparent",colour = "black"),legend.position="none",
        plot.title=element_text(size = 12),axis.text.x=element_text(size=12,angle=0),axis.text.y=element_text(size=12),axis.title.x=element_text(size = 16),axis.title.y=element_text(size=16))+
  labs(main = 'Heathy',x="超敏CRP", y= '淋巴细胞数-healthy')
ggplot()+theme_classic()
#在散点图上添加密度曲线
# ggExtra::ggMarginal(p4, type = "density",  #指定添加类型
#                     xparams=list(fill = "green",alpha=0.5),  #指定颜色
#                     yparams = list(fill="orange",alpha=0.5),  #指定颜色
# )

ggMarginal(p4, type="densigram",xparams=list(fill = "green",color="#54DB05",alpha=0.5,size=0.5),  #指定颜色
           yparams = list(fill="orange",color="#F4A116",alpha=0.5,size=0.5))






ip <- dat001
biochem <-data.frame(ip["超敏CRP"],ip["淋巴细胞数"])
biochem<-biochem[complete.cases(biochem),]

biochem1 <-data.frame(ip["C反应蛋白"],ip["淋巴细胞数"])

biochem1 <-biochem1[complete.cases(biochem1),]

d2 <-  as.numeric(na.omit(biochem$淋巴细胞数))
d22 <-  as.numeric(na.omit(biochem1$淋巴细胞数))
d1 <-  as.numeric(na.omit(biochem$超敏CRP))
d11 <-  as.numeric(na.omit(biochem1$C反应蛋白))
xx  <- c(d1,d11)
yy = c(d2,d22)
heatscatter(xx,yy,colpal=c(  "purple","#5A78F9", "#46B1F2" ,"#FFF800","red"),main="Covid-19",
            cex=2,add.contour=TRUE, cex.axis=1.5,cex.lab=1.5, 
            cor=FALSE,xlab="超敏CRP",ylab = "淋巴细胞数", ylim=c(0,5))

boxplot(yy
        ,xlab = "Patient",         # X轴标签
        col="white",
        border="#40E6D5",
        ylab = "淋巴细胞数", ylim=c(0,5)         # Y轴标签
)
boxplot(xx
        ,xlab = "Patient",  
        horizontal = TRUE,
        col="white",
        border="#FFC09F",
        
        ylab = "mg/mL"        # Y轴标签
        
)


index = yy
Group= xx
dat = data.frame(Group,index)


p4 <- ggplot(data = dat, mapping = aes(x=Group, y=index)) +
  geom_pointdensity( cex=5) +
  stat_smooth(method=loess)+  
  scale_color_viridis() +
  
  geom_density2d(size = 0.3,color="pink") +
  geom_abline(intercept=0,slope=1.2,color = "red" )+
  theme_classic()+ylim(0,5)+
  theme(panel.background = element_rect(fill = "transparent",colour = "black"),legend.position="none",
        plot.title=element_text(size = 12),axis.text.x=element_text(size=12,angle=0),axis.text.y=element_text(size=12),axis.title.x=element_text(size = 16),axis.title.y=element_text(size=16))+
  labs(x="超敏CRP", y= '淋巴细胞数-Covid')
ggplot()+theme_classic()
#在散点图上添加密度曲线
# ggExtra::ggMarginal(p4, type = "density",  #指定添加类型
#                     xparams=list(fill = "green",alpha=0.5),  #指定颜色
#                     yparams = list(fill="orange",alpha=0.5),  #指定颜色
# )

ggMarginal(p4, type="densigram",xparams=list(fill = "green",color="#54DB05",alpha=0.5,size=0.5),  #指定颜色
           yparams = list(fill="orange",color="#F4A116",alpha=0.5,size=0.5))





ip <- dat002
biochem <-data.frame(ip["超敏CRP"],ip["淋巴细胞数"])
biochem<-biochem[complete.cases(biochem),]

biochem1 <-data.frame(ip["C反应蛋白"],ip["淋巴细胞数"])

biochem1 <-biochem1[complete.cases(biochem1),]

d2 <-  as.numeric(na.omit(biochem$淋巴细胞数))
d22 <-  as.numeric(na.omit(biochem1$淋巴细胞数))
d1 <-  as.numeric(na.omit(biochem$超敏CRP))
d11 <-  as.numeric(na.omit(biochem1$C反应蛋白))
xx  <- c(d1,d11)
yy = c(d2,d22)
p <- heatscatter(xx,yy,colpal=c(  "purple","#5A78F9", "#46B1F2" ,"#FFF800","red"),main="Myocarditis",
                 cex=2,add.contour=TRUE, cex.axis=1.5,cex.lab=1.5, 
                 cor=FALSE,xlab="超敏CRP",ylab = "淋巴细胞数", ylim=c(0,5))
plot(d1,d2,xlim=c(0,100),ylim=c(0,60))
plot(d11,d22,xlim=c(0,100),ylim=c(0,60))
plot(xx,yy)
boxplot(yy
        ,xlab = "Patient",         # X轴标签
        col="white",
        border="#40E6D5",
        ylab = "淋巴细胞数", ylim=c(0,5)         # Y轴标签
)
boxplot(xx
        ,xlab = "Patient",  
        horizontal = TRUE,
        col="white",
        border="#FFC09F",
        
        ylab = "mg/mL"        # Y轴标签
        
)



index = yy
Group= xx
dat = data.frame(Group,index)

p4 <- ggplot(data = dat, mapping = aes(x=Group, y=index)) +
  geom_pointdensity(cex=5) +
  stat_smooth(method=loess)+  
  scale_color_viridis() +
  
  geom_density2d(size = 0.3,color="pink") +
  geom_abline(intercept=0,slope=1.2,color = "red" )+
  theme_classic()+ylim(0,5)+
  theme(panel.background = element_rect(fill = "transparent",colour = "black"),legend.position="none",
        plot.title=element_text(size = 12),axis.text.x=element_text(size=12,angle=0),axis.text.y=element_text(size=12),axis.title.x=element_text(size = 16),axis.title.y=element_text(size=16))+
  labs(x="超敏CRP", y= '淋巴细胞数-Myocarditis')


ggplot()+theme_classic()
ggMarginal(p4, type="densigram",xparams=list(fill = "green",color="#54DB05",alpha=0.5,size=0.5),  #指定颜色
           yparams = list(fill="orange",color="#F4A116",alpha=0.5,size=0.5))




a=plot(1:5, 1:5, type = "n",ann = F, bty = "n", xaxt = "n", yaxt ="n")
text(3,3, "乳酸脱氢酶",cex=6)

#_____________________乳酸脱氢酶 and GLB relationship_________________________________________________________________________
ip <- dat000
biochem=data.frame(ip["PATIENTAGE"],ip["乳酸脱氢酶"],ip["PATIENTSEX"],ip["淋巴细胞比率"])
biochem<-biochem[complete.cases(biochem),]

d2 <-  as.numeric(na.omit(biochem$乳酸脱氢酶))
d1 <-  as.numeric(na.omit(biochem$PATIENTAGE))
xx  <- d1
yy = as.numeric(biochem$乳酸脱氢酶)
heatscatter(xx,yy,colpal=c(  "purple","#5A78F9", "#46B1F2" ,"#FFF800","red"),main="Health",
            cex=2,add.contour=TRUE, cex.axis=1.5,cex.lab=1.5, ylim = c(0,300),
            cor=FALSE,xlab="Age",ylab = "TP (mg/mL)")

boxplot(yy
        ,xlab = "Patient",         # X轴标签
        col="white",
        border="#40E6D5",
        ylab = "乳酸脱氢酶"         # Y轴标签
)
boxplot(xx
        ,xlab = "Patient",  
        horizontal = TRUE,
        col="white",
        border="#FFC09F",
        
        ylab = "mg/mL"        # Y轴标签
        
)


index = yy
Group= xx
dat = data.frame(Group,index)


p4 <- ggplot(data = dat, mapping = aes(x=Group, y=index)) +
  geom_pointdensity(cex=5) +
  stat_smooth(method=loess)+  
  scale_color_viridis() +ylim(0,300)+
  
  geom_density2d(size = 0.3,color="pink") +
  geom_abline(intercept=0,slope=1.2,color = "red" )+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent",colour = "black"),legend.position="none",
        plot.title=element_text(size = 12),axis.text.x=element_text(size=12,angle=0),axis.text.y=element_text(size=12),axis.title.x=element_text(size = 16),axis.title.y=element_text(size=16))+
  labs(main = 'Heathy',x="Age", y= 'TP-healthy')
ggplot()+theme_classic()
#在散点图上添加密度曲线
# ggExtra::ggMarginal(p4, type = "density",  #指定添加类型
#                     xparams=list(fill = "green",alpha=0.5),  #指定颜色
#                     yparams = list(fill="orange",alpha=0.5),  #指定颜色
# )

ggMarginal(p4, type="densigram",xparams=list(fill = "green",color="#54DB05",alpha=0.5,size=0.5),  #指定颜色
           yparams = list(fill="orange",color="#F4A116",alpha=0.5,size=0.5))






ip <- dat001
biochem=data.frame(ip["PATIENTAGE"],ip["乳酸脱氢酶"],ip["PATIENTSEX"],ip["淋巴细胞比率"])
biochem<-biochem[complete.cases(biochem),]

d2 <-  as.numeric(na.omit(biochem$乳酸脱氢酶))
d1 <-  as.numeric(na.omit(biochem$PATIENTAGE))
xx  <- d1
yy = as.numeric(biochem$乳酸脱氢酶)
heatscatter(xx,yy,colpal=c(  "purple","#5A78F9", "#46B1F2" ,"#FFF800","red"),main="Covid-19",
            cex=2,add.contour=TRUE, cex.axis=1.5,cex.lab=1.5, ylim = c(0,300),
            cor=FALSE,xlab="Age",ylab = "TP (mg/mL)")

boxplot(yy
        ,xlab = "Patient",         # X轴标签
        col="white",
        border="#40E6D5",
        ylab = "乳酸脱氢酶"         # Y轴标签
)
boxplot(xx
        ,xlab = "Patient",  
        horizontal = TRUE,
        col="white",
        border="#FFC09F",
        
        ylab = "mg/mL"        # Y轴标签
        
)


index = yy
Group= xx
dat = data.frame(Group,index)


p4 <- ggplot(data = dat, mapping = aes(x=Group, y=index)) +
  geom_pointdensity( cex=5) +
  stat_smooth(method=loess)+  
  scale_color_viridis() +ylim(0,300)+
  
  geom_density2d(size = 0.3,color="pink") +
  geom_abline(intercept=0,slope=1.2,color = "red" )+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent",colour = "black"),legend.position="none",
        plot.title=element_text(size = 12),axis.text.x=element_text(size=12,angle=0),axis.text.y=element_text(size=12),axis.title.x=element_text(size = 16),axis.title.y=element_text(size=16))+
  labs(x="Age", y= 'ALB-Covid')
ggplot()+theme_classic()
#在散点图上添加密度曲线
# ggExtra::ggMarginal(p4, type = "density",  #指定添加类型
#                     xparams=list(fill = "green",alpha=0.5),  #指定颜色
#                     yparams = list(fill="orange",alpha=0.5),  #指定颜色
# )

ggMarginal(p4, type="densigram",xparams=list(fill = "green",color="#54DB05",alpha=0.5,size=0.5),  #指定颜色
           yparams = list(fill="orange",color="#F4A116",alpha=0.5,size=0.5))






ip <- dat002
biochem=data.frame(ip["PATIENTAGE"],ip["乳酸脱氢酶"],ip["PATIENTSEX"],ip["淋巴细胞比率"])
biochem<-biochem[complete.cases(biochem),]

d2 <-  as.numeric(na.omit(biochem$乳酸脱氢酶))
d1 <-  as.numeric(na.omit(biochem$PATIENTAGE))
xx  <- d1
yy = as.numeric(biochem$乳酸脱氢酶)
p <- heatscatter(xx,yy,colpal=c(  "purple","#5A78F9", "#46B1F2" ,"#FFF800","red"),main="Myocarditis",
                 cex=2,add.contour=TRUE, cex.axis=1.5,cex.lab=1.5, ylim = c(0,300),
                 cor=FALSE,xlab="Age",ylab = "TP (mg/mL)")



boxplot(yy
        ,xlab = "Patient",         # X轴标签
        col="white",
        border="#40E6D5",
        ylab = "乳酸脱氢酶"         # Y轴标签
)
boxplot(xx
        ,xlab = "Patient",  
        horizontal = TRUE,
        col="white",
        border="#FFC09F",
        
        ylab = "mg/mL"        # Y轴标签
        
)



index = yy
Group= xx
dat = data.frame(Group,index)

p4 <- ggplot(data = dat, mapping = aes(x=Group, y=index)) +
  geom_pointdensity(cex=5) +
  stat_smooth(method=loess)+  
  scale_color_distiller(palette = "Spectral", direction = -1) +ylim(0,300)+
  
  geom_density2d(size = 0.3,color="pink") +
  geom_abline(intercept=0,slope=1.2,color = "red" )+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent",colour = "black"),legend.position="none",
        plot.title=element_text(size = 12),axis.text.x=element_text(size=12,angle=0),axis.text.y=element_text(size=12),axis.title.x=element_text(size = 16),axis.title.y=element_text(size=16))+
  labs(x="Age", y= 'ALB-ILP')


ggplot()+theme_classic()
ggMarginal(p4, type="densigram",xparams=list(fill = "green",color="#54DB05",alpha=0.5,size=0.5),  #指定颜色
           yparams = list(fill="orange",color="#F4A116",alpha=0.5,size=0.5))












#++++++++++++==============各个指标的======boxplot=========================================================================
a=plot(1:5, 1:5, type = "n",ann = F, bty = "n", xaxt = "n", yaxt ="n")
text(3,3, "前指标豆荚图",cex=3)
text(3,2, "'白蛋白','总蛋白','肌酸激酶','血红蛋白','乳酸脱氢酶'",cex=1)

boxplot(yy
        ,xlab = "Patient",         # X轴标签
        col="white",
        border="#40E6D5",
        ylab = "白蛋白"        # Y轴标签
)
boxplot(xx
        ,xlab = "Patient",  
        horizontal = TRUE,
        col="white",
        border="#FFC09F",
        ylab = "mg/mL"        # Y轴标签
        
)


##########  tSNE
dat0 <- dat000
dat1 <- dat001
dat2 <- dat002





dat <- subset (dat0,dat0$PATIENTSEX=="男")
h1 <- (as.numeric(dat$白蛋白))
h2 <- (as.numeric(dat$总蛋白))
h3 <- (as.numeric(dat$肌酸激酶))
h4 <- (as.numeric(dat$血红蛋白))
h5 <- (as.numeric(dat$乳酸脱氢酶))

dat <- subset (dat0,dat0$PATIENTSEX=="女")
fh1 <- (as.numeric(dat$白蛋白))
fh2 <- (as.numeric(dat$总蛋白))
fh3 <- (as.numeric(dat$肌酸激酶))
fh4 <- (as.numeric(dat$血红蛋白))
fh5 <- (as.numeric(dat$乳酸脱氢酶))

dat1 <- subset (dat11,dat11$PATIENTSEX=="男")
l0 <- na.omit (as.numeric(dat1$PATIENTAGE)) 
x1 <- na.omit (as.numeric(dat1$白蛋白))
x2 <- na.omit (as.numeric(dat1$总蛋白))
x3 <- na.omit (as.numeric(dat1$肌酸激酶))
x4 <- na.omit (as.numeric(dat1$血红蛋白))
x5 <- na.omit (as.numeric(dat1$乳酸脱氢酶))

dat1 <- subset (dat11,dat11$PATIENTSEX=="女")
fl0 <- na.omit (as.numeric(dat1$PATIENTAGE)) 
fx1 <- na.omit (as.numeric(dat1$白蛋白))
fx2 <- na.omit (as.numeric(dat1$总蛋白))
fx3 <- na.omit (as.numeric(dat1$肌酸激酶))
fx4 <- na.omit (as.numeric(dat1$血红蛋白))
fx5 <- na.omit (as.numeric(dat1$乳酸脱氢酶))

dat2 <- subset (dat12,dat12$PATIENTSEX=="男")
s0 <- na.omit (as.numeric(dat2$PATIENTAGE)) 
s1 <- na.omit (as.numeric(dat2$白蛋白))
s2 <- na.omit (as.numeric(dat2$总蛋白))
s3 <- na.omit (as.numeric(dat2$肌酸激酶))
s4 <- na.omit (as.numeric(dat2$血红蛋白))
s5 <- na.omit (as.numeric(dat2$乳酸脱氢酶))


dat2 <- subset (dat12,dat12$PATIENTSEX=="女")
fs0 <- na.omit (as.numeric(dat2$PATIENTAGE)) 
fs1 <- na.omit (as.numeric(dat2$白蛋白))
fs2 <- na.omit (as.numeric(dat2$总蛋白))
fs3 <- na.omit (as.numeric(dat2$肌酸激酶))
fs4 <- na.omit (as.numeric(dat2$血红蛋白))
fs5 <- na.omit (as.numeric(dat2$乳酸脱氢酶))

beanplot(h1,fh1,x1,fx1,s1,fs1,
         ll = 0.001,
         main = bio_index[1], ylab = "", side= "both",
         border = 'gray', 
         xaxt="n",cex.axis =2.2,
         col = list(c("#5A78F9", "#5A78F9"),c("#CF6BF9","#CF6BF9")))
legend("topleft", fill =c( "#5A78F9","#CF6BF9"),bty="n",
       legend = c("Male", "Female"))
axis(side=1,#表示在底部建立轴（x轴）
     at=1:3,#我要标志6个tickmarks
     labels=c('Healthy',"Covid","Myocarditis","Covid-recovery"),
     cex.axis =1.8,font.axis=1.5
     #6个tickmarks分别对应的标识(labels)就是这六个字符
)


beanplot(h2,fh2,x2,fx2,s2,fs2,
         ll = 0.001,
         main = bio_index[2], ylab = "", side= "both",
         border = 'gray', 
         xaxt="n",cex.axis =2.2,
         col = list(c("#5A78F9", "#5A78F9"),c("#CF6BF9","#CF6BF9")))
axis(side=1,#表示在底部建立轴（x轴）
     at=1:3,#我要标志6个tickmarks
     labels=c('Healthy',"Covid","Myocarditis","Covid-recovery"),
     cex.axis =1.8,font.axis=1.5
     #6个tickmarks分别对应的标识(labels)就是这六个字符
)



beanplot(h3,fh3,x3,fx3, s3,fs3,
         ll = 0.001,ylim = c(0,200),
         main = bio_index[3], ylab = "", side= "both",
         border = 'gray', 
         xaxt="n",cex.axis =2.2,
         col = list(c("#5A78F9", "#5A78F9"),c("#CF6BF9","#CF6BF9")))
axis(side=1,#表示在底部建立轴（x轴）
     at=1:3,#我要标志6个tickmarks
     labels=c('Healthy',"Covid","Myocarditis","Covid-recovery"),
     cex.axis =1.8,font.axis=1.5
     #6个tickmarks分别对应的标识(labels)就是这六个字符
)



beanplot(h4,fh4,x4,fx4, s4,fs4,
         ll = 0.001,
         main =bio_index[4], ylab = "", side= "both",
         border = 'gray', 
         xaxt="n",cex.axis =2.2,
         col = list(c("#5A78F9", "#5A78F9"),c("#CF6BF9","#CF6BF9")))
axis(side=1,#表示在底部建立轴（x轴）
     at=1:3,#我要标志6个tickmarks
     labels=c('Healthy',"Covid","Myocarditis","Covid-recovery"),
     cex.axis =1.8,font.axis=1.5
     #6个tickmarks分别对应的标识(labels)就是这六个字符
)


beanplot(h5,fh5,x5,fx5, s5,fs5,
         ll = 0.001,
         main = '乳酸脱氢酶', ylab = "", side= "both",
         border = 'gray', 
         xaxt="n",cex.axis =2.2,
         col = list(c("#5A78F9", "#5A78F9"),c("#CF6BF9","#CF6BF9")))
axis(side=1,#表示在底部建立轴（x轴）
     at=1:3,#我要标志6个tickmarks
     labels=c('Healthy',"Covid","Myocarditis","Covid-recovery"),
     cex.axis =1.8,font.axis=1.5
     #6个tickmarks分别对应的标识(labels)就是这六个字符
)



boxplot(h1,x1,s1,    # X轴标签
        main ='白蛋白',
        notch = TRUE,         xlab = "mg/mL",          # Y轴标签
        col = c("#6AF993","#A4B9FC","#F894C2"))
boxplot(h2,x2,s2, 
        main ='总蛋白',
        notch = TRUE,         xlab = "mg/mL",          # Y轴标签
        col =c("#6AF993","#A4B9FC","#F894C2"))
boxplot(h3,x3,s3,     # X轴标签
        main ='肌酸激酶',
        notch = TRUE,         xlab = "mg/mL",          # Y轴标签
        col =c("#6AF993","#A4B9FC","#F894C2"))
boxplot(h4,x4,s4,      # X轴标签
        main ='血红蛋白',
        notch = TRUE,         xlab = "mg/mL",          # Y轴标签
        col =c("#6AF993","#A4B9FC","#F894C2"))
boxplot(h5,x5,s5,   # X轴标签
        main ='乳酸脱氢酶',
        notch = TRUE,         xlab = "mg/mL",          # Y轴标签
        col =c("#6AF993","#A4B9FC","#F894C2"))



###whole Covid-19 compare with subtype Covid-19


library(beanplot)

beanplot(h1,x1,s1,ll = 0.00,         # X轴标签
         main ='白蛋白',
         notch = TRUE,         xlab = "mg/mL",          # Y轴标签
         col = list("#6AF993","#A4B9FC","#F894C2"))
beanplot(h2,x2,s2,ll = 0.00,       # X轴标签
         main ='总蛋白',
         notch = TRUE,         xlab = "mg/mL",          # Y轴标签
         col = list("#6AF993","#A4B9FC","#F894C2"))
beanplot(h3,x3,s3,ll = 0.00,         # X轴标签
         main ='肌酸激酶',
         notch = TRUE,         xlab = "mg/mL",          # Y轴标签
         col = list("#6AF993","#A4B9FC","#F894C2"))
beanplot(h4,x4,s4,ll = 0.00,        # X轴标签
         main ='血红蛋白',
         notch = TRUE,         xlab = "mg/mL",          # Y轴标签
         col = list("#6AF993","#A4B9FC","#F894C2"))
beanplot(h5,x5,s5,ll = 0.00,        # X轴标签
         main ='乳酸脱氢酶',
         notch = TRUE,         xlab = "mg/mL",          # Y轴标签
         col = list("#6AF993","#A4B9FC","#F894C2"))





##########################################################################
a=plot(1:5, 1:5, type = "n",ann = F, bty = "n", xaxt = "n", yaxt ="n")
text(3,3, "后指标图",cex=5)
text(3,2, "'超敏CRP','肌红蛋白','超敏肌钙蛋白I','肌钙蛋白I','C反应蛋白'",cex=1)


dat0 <- dat000
dat11<- dat001
dat12 <- dat002
# dat13 <- dat003


dat <- subset (dat0,dat0$PATIENTSEX=="男")
h1 <- (as.numeric(dat$超敏CRP))
h2 <- (as.numeric(dat$肌红蛋白))
h3 <- (as.numeric(dat$超敏肌钙蛋白I))
h4 <- (as.numeric(dat$肌钙蛋白I))
h5 <- (as.numeric(dat$C反应蛋白))

dat <- subset (dat0,dat0$PATIENTSEX=="女")
fh1 <- (as.numeric(dat$超敏CRP))
fh2 <- (as.numeric(dat$肌红蛋白))
fh3 <- (as.numeric(dat$超敏肌钙蛋白I))
fh4 <- (as.numeric(dat$肌钙蛋白I))
fh5 <- (as.numeric(dat$C反应蛋白))

dat1 <- subset (dat11,dat11$PATIENTSEX=="男")
l0 <- na.omit (as.numeric(dat1$PATIENTAGE)) 
x1 <- na.omit (as.numeric(dat1$超敏CRP))
x2 <- na.omit (as.numeric(dat1$肌红蛋白))
x3 <- na.omit (as.numeric(dat1$超敏肌钙蛋白I))
x4 <- na.omit (as.numeric(dat1$肌钙蛋白I))
x5 <- na.omit (as.numeric(dat1$C反应蛋白))

dat1 <- subset (dat11,dat11$PATIENTSEX=="女")
fl0 <- na.omit (as.numeric(dat1$PATIENTAGE)) 
fx1 <- na.omit (as.numeric(dat1$超敏CRP))
fx2 <- na.omit (as.numeric(dat1$肌红蛋白))
fx3 <- na.omit (as.numeric(dat1$超敏肌钙蛋白I))
fx4 <- na.omit (as.numeric(dat1$肌钙蛋白I))
fx5 <- na.omit (as.numeric(dat1$C反应蛋白))

dat2 <- subset (dat12,dat12$PATIENTSEX=="男")
s0 <- na.omit (as.numeric(dat2$PATIENTAGE)) 
s1 <- na.omit (as.numeric(dat2$超敏CRP))
s2 <- na.omit (as.numeric(dat2$肌红蛋白))
s3 <- na.omit (as.numeric(dat2$超敏肌钙蛋白I))
s4 <- na.omit (as.numeric(dat2$肌钙蛋白I))
s5 <- na.omit (as.numeric(dat2$C反应蛋白))


dat2 <- subset (dat12,dat12$PATIENTSEX=="女")
fs0 <- na.omit (as.numeric(dat2$PATIENTAGE)) 
fs1 <- na.omit (as.numeric(dat2$超敏CRP))
fs2 <- na.omit (as.numeric(dat2$肌红蛋白))
fs3 <- na.omit (as.numeric(dat2$超敏肌钙蛋白I))
fs4 <- na.omit (as.numeric(dat2$肌钙蛋白I))
fs5 <- na.omit (as.numeric(dat2$C反应蛋白))




t.test(h1,fh1)
t.test(x1,fx1)
t.test(s1,fs1)
t.test(l1,fl1)

t.test(h2,fh2)
t.test(x2,fx2)
t.test(s2,fs2)
t.test(l2,fl2)

t.test(h3,fh3)
t.test(x3,fx3)
t.test(s3,fs3)
t.test(l3,fl3)

t.test(h4,fh4)
t.test(x4,fx4)
t.test(s4,fs4)
t.test(l4,fl4)

t.test(h5,fh5)
t.test(x5,fx5)
t.test(s5,fs5)
t.test(l5,fl5)



# beanplot(h1,fh1,x1,fx1,s1,fs1, 
#          ll = 0.001,
#          main = "mg/mL", ylab = "Glucose", side= "both",
#          border = 'gray',horizontal = F,
#          xaxt="n",
#          col = list(c("#5A78F9", "#5A78F9"),c("#CF6BF9","#CF6BF9")))
# legend("topleft", fill =c( "#5A78F9","#CF6BF9"),bty="n",
#        legend = c("Male", "Female"))
# axis(side=1,#表示在底部建立轴（x轴）
#      at=1:4,#我要标志6个tickmarks
#      labels=c('Healthy','LUAD','SCCovid','LUSC')#6个tickmarks分别对应的标识(labels)就是这六个字符
# )

beanplot(h1,fh1,x1,fx1, s1,fs1,
         ll = 0.001,
         main = bio_index2[1], ylab = "", side= "both",
         border = 'gray', 
         
         xaxt="n",cex.axis =2.2,
         col = list(c("#5A78F9", "#5A78F9"),c("#CF6BF9","#CF6BF9")))
legend("topleft", fill =c( "#5A78F9","#CF6BF9"),bty="n",
       legend = c("Male", "Female"))
axis(side=1,#表示在底部建立轴（x轴）
     at=1:3,#我要标志6个tickmarks
     labels=c('Healthy',"Covid","Myocarditis","Covid-recovery"),
     cex.axis =1.8,font.axis=1.5
     #6个tickmarks分别对应的标识(labels)就是这六个字符
)


beanplot(h2,fh2,x2,fx2, s2,fs2,
         ll = 0.001,
         main =x[2], ylab = "", side= "both",
         border = 'gray', 
         
         xaxt="n",cex.axis =2.2,
         col = list(c("#5A78F9", "#5A78F9"),c("#CF6BF9","#CF6BF9")))
axis(side=1,#表示在底部建立轴（x轴）
     at=1:3,#我要标志6个tickmarks
     labels=c('Healthy',"Covid","Myocarditis","Covid-recovery"),
     cex.axis =1.8,font.axis=1.5
     #6个tickmarks分别对应的标识(labels)就是这六个字符
)



beanplot(h3,fh3,x3,fx3, s3,fs3,
         ll = 0.001,ylim=c(0,200),
         main = bio_index2[3], ylab = "", side= "both",
         border = 'gray', 
         
         xaxt="n",cex.axis =2.2,
         col = list(c("#5A78F9", "#5A78F9"),c("#CF6BF9","#CF6BF9")))
axis(side=1,#表示在底部建立轴（x轴）
     at=1:3,#我要标志6个tickmarks
     labels=c('Healthy',"Covid","Myocarditis","Covid-recovery"),
     cex.axis =1.8,font.axis=1.5
     #6个tickmarks分别对应的标识(labels)就是这六个字符
)



beanplot(h4,fh4,x4,fx4, s4,fs4,
         ll = 0.001,
         main =bio_index2[4], ylab = "", side= "both",
         border = 'gray', 
         
         xaxt="n",cex.axis =2.2,
         col = list(c("#5A78F9", "#5A78F9"),c("#CF6BF9","#CF6BF9")))
axis(side=1,#表示在底部建立轴（x轴）
     at=1:3,#我要标志6个tickmarks
     labels=c('Healthy',"Covid","Myocarditis","Covid-recovery"),
     cex.axis =1.8,font.axis=1.5
     #6个tickmarks分别对应的标识(labels)就是这六个字符
)


beanplot(h5,fh5,x5,fx5, s5,fs5,
         ll = 0.001,ylim=c(0,20),
         main = bio_index2[5], ylab = "", side= "both",
         border = 'gray', 
         
         xaxt="n",cex.axis =2.2,
         col = list(c("#5A78F9", "#5A78F9"),c("#CF6BF9","#CF6BF9")))
axis(side=1,#表示在底部建立轴（x轴）
     at=1:3,#我要标志6个tickmarks
     labels=c('Healthy',"Covid","Myocarditis","Covid-recovery"),
     cex.axis =1.8,font.axis=1.5
     #6个tickmarks分别对应的标识(labels)就是这六个字符
)




boxplot(h1,x1,s1,    # X轴标签
        main =bio_index2[1],
        notch = TRUE,         
        xlab = "mg/mL",          # Y轴标签
        col = list("#6AF993","#A4B9FC","#F894C2"))
boxplot(h2,x2,s2, 
        main =bio_index2[2],
        
        notch = TRUE,         xlab = "mg/mL",          # Y轴标签
        col =c("#6AF993","#A4B9FC","#F894C2"))
boxplot(h3,x3,s3,     # X轴标签
        main =bio_index2[3],
        
        notch = TRUE,         xlab = "mg/mL",          # Y轴标签
        col =c("#6AF993","#A4B9FC","#F894C2"))
boxplot(h4,x4,s4,      # X轴标签
        main =bio_index2[4],
        
        notch = TRUE,         xlab = "mg/mL",          # Y轴标签
        col =c("#6AF993","#A4B9FC","#F894C2"))
boxplot(h5,x5,s5,   # X轴标签
        main =bio_index2[5],
        
        notch = TRUE,         xlab = "mg/mL",          # Y轴标签
        col =c("#6AF993","#A4B9FC","#F894C2"))



###whole Covid-19 compare with subtype Covid-19

library(beanplot)
beanplot(h1,x1,s1,ll = 0.00,         # X轴标签
         main =bio_index2[1],
         
         notch = TRUE,         xlab = "mg/mL",          # Y轴标签
         col = list("#6AF993","#A4B9FC","#F894C2"))
beanplot(h2,x2,s2,ll = 0.00,       # X轴标签
         main =bio_index2[2],
         
         notch = TRUE,         xlab = "mg/mL",          # Y轴标签
         col = list("#6AF993","#A4B9FC","#F894C2"))
beanplot(h3,x3,s3,ll = 0.00,         # X轴标签
         main =bio_index2[3],ylim = c(0,200),
         
         notch = TRUE,         xlab = "mg/mL",          # Y轴标签
         col = list("#6AF993","#A4B9FC","#F894C2"))
beanplot(h4,x4,s4,ll = 0.00,        # X轴标签
         main =bio_index2[4],
         
         notch = TRUE,         xlab = "mg/mL",          # Y轴标签
         col = list("#6AF993","#A4B9FC","#F894C2"))
beanplot(h5,x5,s5,ll = 0.00,        # X轴标签
         main =bio_index2[5],
         notch = TRUE,         xlab = "mg/mL",          # Y轴标签
         col = list("#6AF993","#A4B9FC","#F894C2"))



# 
# dat0 <- read_excel("baby_Health.xlsx",sheet = "Myocarditis")
# dat11 <- read_excel("baby_leukemia.xlsx",sheet = "Myocarditis")
# dat12 <- read_excel("baby_leukemia.xlsx",sheet = "Covid-recovery")

dat <- dat000
sh1 <- na.omit(as.numeric(dat$白蛋白))
sh2 <- na.omit(as.numeric(dat$总蛋白))
sh3 <- na.omit(as.numeric(dat$肌酸激酶))
sh4 <- na.omit(as.numeric(dat$血红蛋白))
sh5 <- na.omit(as.numeric(dat$乳酸脱氢酶))


dat <- dat001
h1 <- na.omit(as.numeric(dat$白蛋白))
h2 <- na.omit(as.numeric(dat$总蛋白))
h3 <- na.omit(as.numeric(dat$肌酸激酶))
h4 <- na.omit(as.numeric(dat$血红蛋白))
h5 <- na.omit(as.numeric(dat$乳酸脱氢酶))


dat1 <- dat002
l0 <- na.omit (as.numeric(dat1$PATIENTAGE)) 
x1 <- na.omit (as.numeric(dat1$白蛋白))
x2 <- na.omit (as.numeric(dat1$总蛋白))
x3 <- na.omit (as.numeric(dat1$肌酸激酶))
x4 <- na.omit (as.numeric(dat1$血红蛋白))
x5 <- na.omit (as.numeric(dat1$乳酸脱氢酶))




a=plot(1:5, 1:5, type = "n",ann = F, bty = "n", xaxt = "n", yaxt ="n")
text(3,3, "指标差异显著性图",cex=3)




index = c( sh1,h1,x1)
my_comparisons <- list(c("Healthy","Covid"),c("Myocarditis","Covid"),c("Healthy","Myocarditis"))
Group= c(rep("Healthy", times=length(sh1)),rep("Covid", times=length(h1)),rep("Myocarditis", times=length(x1)))
dat = data.frame(Group,index)
dat$Group <- factor(dat$Group, levels = c("Healthy","Covid","Myocarditis"))
ggplot(dat, aes(x=Group, y=index))+geom_boxplot(outlier.shape = 21,width=0.6,aes(fill = Group))+scale_fill_manual(values = c("#6BF993","#A4B9FC","#F794C1","#F49984" ))+geom_dotplot(binaxis = "index", stackdir = "center",
                                                                                                                                                                                     position = position_dodge(1))+
  # geom_jitter(width = 0.25, size=0.01,color="red",alpha=0.3)+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent",colour = NA),legend.position="none",
        plot.title=element_text(size = 25),axis.text.x=element_text(size=24,angle=0),axis.text.y=element_text(size=24),axis.title.x=element_text(size = 24),axis.title.y=element_text(size=36))+
  labs(x=' ', y= 'ALB')+
  
  geom_signif(comparisons = my_comparisons,step_increase = 0.1,map_signif_level = T,textsize=12,test = t.test)+ 
  guides(fill="none")


index = c(sh2,h2,x2)
Group= c(rep("Healthy", times=length(sh2)),rep("Covid", times=length(h2)),rep("Myocarditis", times=length(x2)))
dat = data.frame(Group,index)
dat$Group <- factor(dat$Group, levels = c("Healthy","Covid","Myocarditis"))
ggplot(dat, aes(x=Group, y=index))+geom_boxplot(outlier.shape = 21,width=0.6,aes(fill = Group))+scale_fill_manual(values = c("#6BF993","#A4B9FC","#F794C1","#F49984" ))+geom_dotplot(binaxis = "index", stackdir = "center",
                                                                                                                                                                                     position = position_dodge(1))+
  # geom_jitter(width = 0.25, size=0.01,color="red",alpha=0.3)+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent",colour = NA),legend.position="none",
        plot.title=element_text(size = 25),axis.text.x=element_text(size=24,angle=0),axis.text.y=element_text(size=24),axis.title.x=element_text(size = 24),axis.title.y=element_text(size=36))+
  labs(x=' ', y= 'TP')+
  
  geom_signif(comparisons = my_comparisons,step_increase = 0.1,map_signif_level = T,textsize=12,test = t.test)+ 
  guides(fill="none")


index = c(sh3,h3,x3)
Group= c(rep("Healthy", times=length(sh3)),rep("Covid", times=length(h3)),rep("Myocarditis",times=length(x3)))
dat = data.frame(Group,index)
dat$Group <- factor(dat$Group, levels = c("Healthy","Covid","Myocarditis"))
ggplot(dat, aes(x=Group, y=index))+geom_boxplot(outlier.shape = 21,width=0.6,aes(fill = Group))+scale_fill_manual(values = c("#6BF993","#A4B9FC","#F794C1","#F49984" ))+geom_dotplot(binaxis = "index", stackdir = "center",
                                                                                                                                                                                     position = position_dodge(1))+
  # geom_jitter(width = 0.25, size=0.01,color="red",alpha=0.3)+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent",colour = NA),legend.position="none",
        plot.title=element_text(size = 25),axis.text.x=element_text(size=24,angle=0),axis.text.y=element_text(size=24),axis.title.x=element_text(size = 24),axis.title.y=element_text(size=36))+
  labs(x=' ', y= '肌酸激酶')+ 
  
  geom_signif(comparisons = my_comparisons,step_increase = 0.1,map_signif_level = T,textsize=12,test = t.test)+ 
  guides(fill="none")

index = c(sh3,h3,x3)
Group= c(rep("Healthy", times=length(sh3)),rep("Covid", times=length(h3)),rep("Myocarditis",times=length(x3)))
dat = data.frame(Group,index)
dat$Group <- factor(dat$Group, levels = c("Healthy","Covid","Myocarditis"))
ggplot(dat, aes(x=Group, y=index))+geom_boxplot(outlier.shape = 21,width=0.6,aes(fill = Group))+scale_fill_manual(values = c("#6BF993","#A4B9FC","#F794C1","#F49984" ))+geom_dotplot(binaxis = "index", stackdir = "center",
                                                                                                                                                                                     position = position_dodge(1))+
  # geom_jitter(width = 0.25, size=0.01,color="red",alpha=0.3)+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent",colour = NA),legend.position="none",
        plot.title=element_text(size = 25),axis.text.x=element_text(size=24,angle=0),axis.text.y=element_text(size=24),axis.title.x=element_text(size = 24),axis.title.y=element_text(size=36))+
  labs(x=' ', y= '肌酸激酶')+ ylim(0,500)+
  
  geom_signif(comparisons = my_comparisons,step_increase = 0.1,map_signif_level = T,textsize=12,test = t.test)+ 
  guides(fill="none")



index = c(sh4,h4,x4)
Group= c(rep("Healthy", times=length(sh4)),rep("Covid", times=length(h4)),rep("Myocarditis", times=length(x4)))
dat = data.frame(Group,index)
dat$Group <- factor(dat$Group, levels = c("Healthy","Covid","Myocarditis"))
ggplot(dat, aes(x=Group, y=index))+geom_boxplot(outlier.shape = 21,width=0.6,aes(fill = Group))+scale_fill_manual(values = c("#6BF993","#A4B9FC","#F794C1","#F49984" ))+geom_dotplot(binaxis = "index", stackdir = "center",
                                                                                                                                                                                     position = position_dodge(1))+
  # geom_jitter(width = 0.25, size=0.01,color="red",alpha=0.3)+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent",colour = NA),legend.position="none",
        plot.title=element_text(size = 25),axis.text.x=element_text(size=24,angle=0),axis.text.y=element_text(size=24),axis.title.x=element_text(size = 24),axis.title.y=element_text(size=36))+
  labs(x=' ', y= '血红蛋白')+
  
  geom_signif(comparisons = my_comparisons,step_increase = 0.1,map_signif_level = T,textsize=12,test = t.test)+ 
  guides(fill="none")


index = c(sh5,h5,x5)
Group= c(rep("Healthy", times=length(sh5)),rep("Covid", times=length(h5)),rep("Myocarditis", times=length(x5)))
dat = data.frame(Group,index)
dat$Group <- factor(dat$Group, levels = c("Healthy","Covid","Myocarditis"))
ggplot(dat, aes(x=Group, y=index))+geom_boxplot(outlier.shape = 21,width=0.6,aes(fill = Group))+scale_fill_manual(values = c("#6BF993","#A4B9FC","#F794C1","#F49984" ))+geom_dotplot(binaxis = "index", stackdir = "center",
                                                                                                                                                                                     position = position_dodge(1))+
  # geom_jitter(width = 0.25, size=0.01,color="red",alpha=0.3)+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent",colour = NA),legend.position="none",
        plot.title=element_text(size = 25),axis.text.x=element_text(size=24,angle=0),axis.text.y=element_text(size=24),axis.title.x=element_text(size = 24),axis.title.y=element_text(size=36))+
  labs(x=' ', y= '乳酸脱氢酶')+
  
  geom_signif(comparisons = my_comparisons,step_increase = 0.1,map_signif_level = T,textsize=12,test = t.test)+ 
  guides(fill="none")





dat <- dat000
sh1 <- na.omit(as.numeric(dat$C反应蛋白))
sh2 <- na.omit(as.numeric(dat$肌钙蛋白I))
sh3 <- na.omit(as.numeric(dat$超敏肌钙蛋白I))
sh4 <- na.omit(as.numeric(dat$肌红蛋白))
sh5 <- na.omit(as.numeric(dat$超敏CRP))



dat <- dat001
h1 <- na.omit(as.numeric(dat$C反应蛋白))
h2 <- na.omit(as.numeric(dat$肌钙蛋白I))
h3 <- na.omit(as.numeric(dat$超敏肌钙蛋白I))
h4 <- na.omit(as.numeric(dat$肌红蛋白))
h5 <- na.omit(as.numeric(dat$超敏CRP))



dat1 <- dat002
l0 <- na.omit (as.numeric(dat1$PATIENTAGE)) 
x1 <- na.omit (as.numeric(dat1$C反应蛋白))
x2 <- na.omit (as.numeric(dat1$肌钙蛋白I))
x3 <- na.omit (as.numeric(dat1$超敏肌钙蛋白I))
x4 <- na.omit (as.numeric(dat1$肌红蛋白))
x5 <- na.omit (as.numeric(dat1$超敏CRP))




index = c( sh1,h1,x1)
my_comparisons <- list(c("Healthy","Covid"),c("Myocarditis","Covid"),c("Healthy","Myocarditis"))
Group= c(rep("Healthy", times=length(sh1)),rep("Covid", times=length(h1)),rep("Myocarditis", times=length(x1)))
dat = data.frame(Group,index)
dat$Group <- factor(dat$Group, levels = c("Healthy","Covid","Myocarditis"))
ggplot(dat, aes(x=Group, y=index))+geom_boxplot(outlier.shape = 21,width=0.6,aes(fill = Group))+scale_fill_manual(values = c("#6BF993","#A4B9FC","#F794C1","#F49984" ))+geom_dotplot(binaxis = "index", stackdir = "center",
                                                                                                                                                                                     position = position_dodge(1))+
  # geom_jitter(width = 0.25, size=0.01,color="red",alpha=0.3)+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent",colour = NA),legend.position="none",
        plot.title=element_text(size = 25),axis.text.x=element_text(size=24,angle=0),axis.text.y=element_text(size=24),axis.title.x=element_text(size = 24),axis.title.y=element_text(size=36))+
  labs(x=' ', y= 'C反应蛋白')+
  
  geom_signif(comparisons = my_comparisons,step_increase = 0.1,map_signif_level = T,textsize=12,test = t.test)+ 
  guides(fill="none")

ggplot(dat, aes(x=Group, y=index))+geom_boxplot(outlier.shape = 21,width=0.6,aes(fill = Group))+scale_fill_manual(values = c("#6BF993","#A4B9FC","#F794C1","#F49984" ))+geom_dotplot(binaxis = "index", stackdir = "center",
                                                                                                                                                                                     position = position_dodge(1))+
  # geom_jitter(width = 0.25, size=0.01,color="red",alpha=0.3)+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent",colour = NA),legend.position="none",
        plot.title=element_text(size = 25),axis.text.x=element_text(size=24,angle=0),axis.text.y=element_text(size=24),axis.title.x=element_text(size = 24),axis.title.y=element_text(size=36))+
  labs(x=' ', y= 'C反应蛋白')+ylim(0,20)+
  
  geom_signif(comparisons = my_comparisons,step_increase = 0.1,map_signif_level = T,textsize=12,test = t.test)+ 
  guides(fill="none")


index = c(sh2,h2,x2)
Group= c(rep("Healthy", times=length(sh2)),rep("Covid", times=length(h2)),rep("Myocarditis", times=length(x2)))
dat = data.frame(Group,index)
dat$Group <- factor(dat$Group, levels = c("Healthy","Covid","Myocarditis"))
ggplot(dat, aes(x=Group, y=index))+geom_boxplot(outlier.shape = 21,width=0.6,aes(fill = Group))+scale_fill_manual(values = c("#6BF993","#A4B9FC","#F794C1","#F49984" ))+geom_dotplot(binaxis = "index", stackdir = "center",
                                                                                                                                                                                     position = position_dodge(1))+
  # geom_jitter(width = 0.25, size=0.01,color="red",alpha=0.3)+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent",colour = NA),legend.position="none",
        plot.title=element_text(size = 25),axis.text.x=element_text(size=24,angle=0),axis.text.y=element_text(size=24),axis.title.x=element_text(size = 24),axis.title.y=element_text(size=36))+
  labs(x=' ', y= '肌钙蛋白I')+
  
  geom_signif(comparisons = my_comparisons,step_increase = 0.1,map_signif_level = T,textsize=12,test = t.test)+ 
  guides(fill="none")


index = c(sh3,h3,x3)
Group= c(rep("Healthy", times=length(sh3)),rep("Covid", times=length(h3)),rep("Myocarditis", times=length(x3)))
dat = data.frame(Group,index)
dat$Group <- factor(dat$Group, levels = c("Healthy","Covid","Myocarditis"))
ggplot(dat, aes(x=Group, y=index))+geom_boxplot(outlier.shape = 21,width=0.6,aes(fill = Group))+scale_fill_manual(values = c("#6BF993","#A4B9FC","#F794C1","#F49984" ))+geom_dotplot(binaxis = "index", stackdir = "center",
                                                                                                                                                                                     position = position_dodge(1))+
  # geom_jitter(width = 0.25, size=0.01,color="red",alpha=0.3)+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent",colour = NA),legend.position="none",
        plot.title=element_text(size = 25),axis.text.x=element_text(size=24,angle=0),axis.text.y=element_text(size=24),axis.title.x=element_text(size = 24),axis.title.y=element_text(size=36))+
  labs(x=' ', y= '超敏肌钙蛋白I')+
  
  geom_signif(comparisons = my_comparisons,step_increase = 0.1,map_signif_level = T,textsize=12,test = t.test)+ 
  guides(fill="none")


ggplot(dat, aes(x=Group, y=index))+geom_boxplot(outlier.shape = 21,width=0.6,aes(fill = Group))+scale_fill_manual(values = c("#6BF993","#A4B9FC","#F794C1","#F49984" ))+geom_dotplot(binaxis = "index", stackdir = "center",
                                                                                                                                                                                     position = position_dodge(1))+
  # geom_jitter(width = 0.25, size=0.01,color="red",alpha=0.3)+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent",colour = NA),legend.position="none",
        plot.title=element_text(size = 25),axis.text.x=element_text(size=24,angle=0),axis.text.y=element_text(size=24),axis.title.x=element_text(size = 24),axis.title.y=element_text(size=36))+
  labs(x=' ', y= '超敏肌钙蛋白I')+ylim(0,50)+
  
  geom_signif(comparisons = my_comparisons,step_increase = 0.1,map_signif_level = T,textsize=12,test = t.test)+ 
  guides(fill="none")


index = c(sh4,h4,x4)
Group= c(rep("Healthy", times=length(sh4)),rep("Covid", times=length(h4)),rep("Myocarditis", times=length(x4)))
dat = data.frame(Group,index)
dat$Group <- factor(dat$Group, levels = c("Healthy","Covid","Myocarditis"))
ggplot(dat, aes(x=Group, y=index))+geom_boxplot(outlier.shape = 21,width=0.6,aes(fill = Group))+scale_fill_manual(values = c("#6BF993","#A4B9FC","#F794C1","#F49984" ))+geom_dotplot(binaxis = "index", stackdir = "center",
                                                                                                                                                                                     position = position_dodge(1))+
  # geom_jitter(width = 0.25, size=0.01,color="red",alpha=0.3)+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent",colour = NA),legend.position="none",
        plot.title=element_text(size = 25),axis.text.x=element_text(size=24,angle=0),axis.text.y=element_text(size=24),axis.title.x=element_text(size = 24),axis.title.y=element_text(size=36))+
  labs(x=' ', y= '肌红蛋白')+
  
  geom_signif(comparisons = my_comparisons,step_increase = 0.1,map_signif_level = T,textsize=12,test = t.test)+ 
  guides(fill="none")


ggplot(dat, aes(x=Group, y=index))+geom_boxplot(outlier.shape = 21,width=0.6,aes(fill = Group))+scale_fill_manual(values = c("#6BF993","#A4B9FC","#F794C1","#F49984" ))+geom_dotplot(binaxis = "index", stackdir = "center",
                                                                                                                                                                                     position = position_dodge(1))+
  # geom_jitter(width = 0.25, size=0.01,color="red",alpha=0.3)+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent",colour = NA),legend.position="none",
        plot.title=element_text(size = 25),axis.text.x=element_text(size=24,angle=0),axis.text.y=element_text(size=24),axis.title.x=element_text(size = 24),axis.title.y=element_text(size=36))+
  labs(x=' ', y= '肌红蛋白')+ylim(0,200)+
  
  geom_signif(comparisons = my_comparisons,step_increase = 0.1,map_signif_level = T,textsize=12,test = t.test)+ 
  guides(fill="none")



index = c(sh5,h5,x5)
Group= c(rep("Healthy", times=length(sh5)),rep("Covid", times=length(h5)),rep("Myocarditis", times=length(x5)))
dat = data.frame(Group,index)
dat$Group <- factor(dat$Group, levels = c("Healthy","Covid","Myocarditis"))
ggplot(dat, aes(x=Group, y=index))+geom_boxplot(outlier.shape = 21,width=0.6,aes(fill = Group))+scale_fill_manual(values = c("#6BF993","#A4B9FC","#F794C1","#F49984" ))+geom_dotplot(binaxis = "index", stackdir = "center",
                                                                                                                                                                                     position = position_dodge(1))+
  # geom_jitter(width = 0.25, size=0.01,color="red",alpha=0.3)+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent",colour = NA),legend.position="none",
        plot.title=element_text(size = 25),axis.text.x=element_text(size=24,angle=0),axis.text.y=element_text(size=24),axis.title.x=element_text(size = 24),axis.title.y=element_text(size=36))+
  labs(x=' ', y= '超敏CRP')+
  
  geom_signif(comparisons = my_comparisons,step_increase = 0.1,map_signif_level = T,textsize=12,test = t.test)+ 
  guides(fill="none")


ggplot(dat, aes(x=Group, y=index))+geom_boxplot(outlier.shape = 21,width=0.6,aes(fill = Group))+scale_fill_manual(values = c("#6BF993","#A4B9FC","#F794C1","#F49984" ))+geom_dotplot(binaxis = "index", stackdir = "center",
                                                                                                                                                                                     position = position_dodge(1))+
  # geom_jitter(width = 0.25, size=0.01,color="red",alpha=0.3)+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent",colour = NA),legend.position="none",
        plot.title=element_text(size = 25),axis.text.x=element_text(size=24,angle=0),axis.text.y=element_text(size=24),axis.title.x=element_text(size = 24),axis.title.y=element_text(size=36))+
  labs(x=' ', y= '超敏CRP')+ylim(0,100)+
  
  geom_signif(comparisons = my_comparisons,step_increase = 0.1,map_signif_level = T,textsize=12,test = t.test)+ 
  guides(fill="none")






plot(0,0)
index = yy
Group= xx
dat = data.frame(Group,index)



p4 <- ggplot(data = dat, mapping = aes(x=Group, y=index)) +
  geom_pointdensity(cex=5) +
  stat_smooth(method=loess)+  
  scale_color_distiller(palette = "Spectral", direction = -1) +
  
  geom_density2d(size = 0.3,color="pink") +
  geom_abline(intercept=0,slope=1.2,color = "red" )+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent",colour = "black"),legend.position="none",
        plot.title=element_text(size = 12),axis.text.x=element_text(size=12,angle=0),axis.text.y=element_text(size=12),axis.title.x=element_text(size = 16),axis.title.y=element_text(size=16))+
  labs(x="GLB (mg/mL)", y= 'ALB')

#在散点图上添加密度曲线
# ggExtra::ggMarginal(p4, type = "density",  #指定添加类型
#                     xparams=list(fill = "green",alpha=0.5),  #指定颜色
#                     yparams = list(fill="orange",alpha=0.5),  #指定颜色
# )
ggplot()+theme_classic()
ggMarginal(p4, type="densigram",xparams=list(fill = "green",color="#54DB05",alpha=0.5,size=0.5),  #指定颜色
           yparams = list(fill="orange",color="#F4A116",alpha=0.5,size=0.5))

# ggMarginal(p4, type="boxplot", size=5, xparams = list(color="green",alpha=0.5,size=0.5), yparams = list(color="orange",alpha=0.5,size=0.5))



a=plot(1:5, 1:5, type = "n",ann = F, bty = "n", xaxt = "n", yaxt ="n")
text(3,3, "三维热图",cex=3)


dat_heat <- read_excel("Health_heatmap.xlsx")

#cancer patient
testdata1 <- do.call(rbind,dat_heat)
testdata1 <- t(testdata1)
testdata1 <- as.data.frame(testdata1)
testdata1 <- as.data.frame(lapply(testdata1,as.numeric))
testdata1
library(ComplexHeatmap)
cor_data1 <- abs(cor(testdata1, use = 'pairwise.complete.obs',method="kendall"))
n <- length(colnames(cor_data1))
mat  <- cor_data1[2:n,2:n]
a <- 1:n
for (ra in a){mat[ra,ra]=0}
mat
Heatmap3D(mat, name = "mat", column_title = "healthy heatmap")
library(ggcorrplot)
ggcorrplot(cor_data1,outline.color="white",
           type="lower",colors = c("#06DDF9", "#F7F7F7", "#E46726"),
           lab = TRUE,lab_size = 3, tl.cex = 10, ggtheme = ggplot2::theme_void())

dat_heat <- read_excel("heatmap.xlsx")

#cancer patient
testdata1 <- do.call(rbind,dat_heat)
testdata1 <- t(testdata1)
testdata1 <- as.data.frame(testdata1)
testdata1 <- as.data.frame(lapply(testdata1,as.numeric))
testdata1
library(ComplexHeatmap)
cor_data1 <- abs(cor(testdata1, use = 'pairwise.complete.obs',method="kendall"))
n <- length(colnames(cor_data1))
mat  <- cor_data1[2:n,2:n]
a <- 1:n
for (ra in a){mat[ra,ra]=0}
mat
Heatmap3D(mat, name = "mat", column_title = "Covid_heatmap")
library(ggcorrplot)
ggcorrplot(cor_data1,outline.color="white",
           type="lower",colors = c("#06DDF9", "#F7F7F7", "#E46726"),
           lab = TRUE,lab_size = 3, tl.cex = 10, ggtheme = ggplot2::theme_void())



dat_heat <- read_excel("m_heatmap.xlsx")

#cancer patient
testdata1 <- do.call(rbind,dat_heat)
testdata1 <- t(testdata1)
testdata1 <- as.data.frame(testdata1)
testdata1 <- as.data.frame(lapply(testdata1,as.numeric))
testdata1
library(ComplexHeatmap)
cor_data1 <- abs(cor(testdata1, use = 'pairwise.complete.obs',method="kendall"))
n <- length(colnames(cor_data1))
mat  <- cor_data1[2:n,2:n]
a <- 1:n
for (ra in a){mat[ra,ra]=0}
mat
Heatmap3D(mat, name = "mat", column_title = "myocarditis heatmap")
library(ggcorrplot)
ggcorrplot(cor_data1,outline.color="white",
           type="lower",colors = c("#06DDF9", "#F7F7F7", "#E46726"),
           lab = TRUE,lab_size = 3, tl.cex = 10, ggtheme = ggplot2::theme_void())






dat_heat <- gg0
#cancer patient
testdata1 <- do.call(rbind,dat_heat)
testdata1 <- t(testdata1)
testdata1 <- as.data.frame(testdata1)
testdata1 <- as.data.frame(lapply(testdata1,as.numeric))
testdata1

ggcorrplot(cor_data1,outline.color="white",
           type="lower",colors = c("#06DDF9", "#F7F7F7", "#E46726"),
           lab = TRUE,lab_size = 3, tl.cex = 10, ggtheme = ggplot2::theme_void())


cor_data1 <- abs(cor(testdata1, use = 'pairwise.complete.obs',method="kendall"))
n <- length(colnames(cor_data1))
mat  <- cor_data1[1:n-1,1:n-1]
a <- 1:n
for (ra in a){mat[ra,ra]=0}
mat
Heatmap3D(mat, name = "mat", column_title = "healthy heatmap")


dat_heat <-gg1

#cancer patient
testdata1 <- do.call(rbind,dat_heat)
testdata1 <- t(testdata1)
testdata1 <- as.data.frame(testdata1)
testdata1 <- as.data.frame(lapply(testdata1,as.numeric))
testdata1

ggcorrplot(cor_data1,outline.color="white",
           type="lower",colors = c("#06DDF9", "#F7F7F7", "#E46726"),
           lab = TRUE,lab_size = 3, tl.cex = 10, ggtheme = ggplot2::theme_void())


cor_data1 <- abs(cor(testdata1, use = 'pairwise.complete.obs',method="kendall"))
n <- length(colnames(cor_data1))
mat  <- cor_data1[1:n-1,1:n-1]
a <- 1:n
for (ra in a){mat[ra,ra]=0}
mat
Heatmap3D(mat, name = "mat", column_title = "Covid19_heatmap")




dat_heat <- gg2

#cancer patient
testdata1 <- do.call(rbind,dat_heat)
testdata1 <- t(testdata1)
testdata1 <- as.data.frame(testdata1)
testdata1 <- as.data.frame(lapply(testdata1,as.numeric))
testdata1

ggcorrplot(cor_data1,outline.color="white",
           type="lower",colors = c("#06DDF9", "#F7F7F7", "#E46726"),
           lab = TRUE,lab_size = 3, tl.cex = 10, ggtheme = ggplot2::theme_void())


cor_data1 <- abs(cor(testdata1, use = 'pairwise.complete.obs',method="kendall"))
n <- length(colnames(cor_data1))
mat  <- cor_data1[1:n-1,1:n-1]
a <- 1:n
for (ra in a){mat[ra,ra]=0}
mat
Heatmap3D(mat, name = "mat", column_title = "myocarditis heatmap")





dev.off()

