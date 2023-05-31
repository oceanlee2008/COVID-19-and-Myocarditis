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
