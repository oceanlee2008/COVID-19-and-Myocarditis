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
dat001 <- read_csv("./COVID-19 patients.csv")
dat002 <- read_csv("./Myocarditis patients.csv")

 
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

