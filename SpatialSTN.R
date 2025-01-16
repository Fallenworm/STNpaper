library(ggforce)
library(ggExtra)
library(ggpubr)
###############################################################################depression
data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/AlphaData.csv",header = TRUE,
                   sep = ",",na.strings = "na") 

p1<-ggplot(data = data, aes(x = x, fill =group, color= group))+  
  geom_density(alpha = 0.5)+
  scale_color_manual(name = "Group", values = c("#5CAAD7","#BB1E38"),guide =F)+
  scale_fill_manual(name = "Group", values = c("#5CAAD7","#BB1E38"))+
  theme_bw()+
  theme(panel.grid  = element_blank(),
        legend.position = "none", legend.text = element_text(size = 12), legend.title = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title =  element_text(size = 12))+
  labs(x = "medial <---> lateral", y = "Density")

p2<-ggplot(data = data, aes(x = y, fill =group, color= group))+  
  geom_density(alpha = 0.5)+
  scale_color_manual(name = "Group", values = c("#5CAAD7","#BB1E38"),guide =F)+
  scale_fill_manual(name = "Group", values = c("#5CAAD7","#BB1E38"))+
  theme_bw()+
  theme(panel.grid  = element_blank(),
        legend.position = "none", legend.text = element_text(size = 12), legend.title = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.title =  element_text(size = 12))+
  labs(x = "posterior <---> anterior")

p3<-ggplot(data = data, aes(x = z, fill =group, color= group))+  
  geom_density(alpha = 0.5)+
  scale_color_manual(name = "Group", values = c("#5CAAD7","#BB1E38"),guide =F)+
  scale_fill_manual(name = "Group", values = c("#5CAAD7","#BB1E38"))+
  theme_bw()+
  theme(panel.grid  = element_blank(),
        legend.position = "none", legend.text = element_text(size = 12), legend.title = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.title =  element_text(size = 12))+
  labs(x = "ventral <---> dorsal")



data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/BDIalphacor.csv",header = TRUE,
                   sep = ",",na.strings = "na")
dat1<- as.matrix(data)
dat1<-as.data.frame(dat1)
dat1$pvalue<-as.numeric(dat1$Pval)
dat1$cor<-as.numeric(dat1$Corr)
dat1$Corr <- cut(abs(dat1$cor), 
                 breaks = c(0,0.3,0.5,0.7,0.9,1),
                 labels = c("< 0.3", "0.3 - 0.5", "0.5 - 0.7", "0.7 - 0.9", "> 0.9"),
                 right = FALSE)
dat1$pvalue1 <- cut(dat1$pvalue, 
                    breaks = c(0, 0.001, 0.01, 0.05, 1),
                    labels = c("< 0.001", "< 0.01", "< 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("Peak power based", "STN based")
dat1$Variable <- factor(dat1$Class, levels = desired_order )

p4<-ggplot(dat1, aes(x = Variable, y = cor,fill = Variable))+
  geom_col(width = 0.5,alpha = 0.5)+
  scale_fill_manual(name = "Group", values = c("#5CAAD7","#BB1E38"))+
  theme_bw()+
  labs(y = "Spearman's r")+
  theme(panel.grid.minor.y  = element_blank(),
        panel.grid.major.y  = element_blank(),
        axis.title = element_blank(),
        axis.title.y =  element_text(size = 12, angle = 90),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text = element_text(size = 12, face = "plain", color = "black"),
        legend.position = "none")

data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/BDIperm.csv",header = TRUE,
                   sep = ",",na.strings = "na")

p5 <- ggplot(data = data, aes(x = r))+  
  geom_histogram(alpha = 0.5, color = "grey", fill = "grey", binwidth = 0.02)+
  geom_segment(aes(x = 0.36279446, y = 0, xend = 0.36279446, yend = 200), colour = "#BB1E38", linewidth = 2, alpha = 0.5)+
  geom_segment(aes(x = 0.17207253, y = 0, xend = 0.17207253, yend = 200), colour = "#5CAAD7", linewidth = 2, alpha = 0.5)+
  theme_bw()+
  theme(panel.grid  = element_blank(),
        legend.position = "none", legend.text = element_text(size = 12), legend.title = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title =  element_text(size = 12))+
  labs(x = "Spatial and spectral null model", y = "Counts")

library(patchwork)
library(cowplot)
(p1|p2|p3)/(p4|p5)

ggsave(
  filename = "BDIloc.png", # 保存的文件名称。通过后缀来决定生成什么格式的图片
  width = 6.5,             # 宽
  height = 4,            # 高
  units = "in",          # 单位
  dpi = 300              # 分辨率DPI
)
################################################################################Compulsivity
data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/DeltaData.csv",header = TRUE,
                   sep = ",",na.strings = "na") 

p1<-ggplot(data = data, aes(x = x, fill =group, color= group))+  
  geom_density(alpha = 0.5)+
  scale_color_manual(name = "Group", values = c("#5CAAD7","#BB1E38"),guide =F)+
  scale_fill_manual(name = "Group", values = c("#5CAAD7","#BB1E38"))+
  theme_bw()+
  theme(panel.grid  = element_blank(),
        legend.position = "none", legend.text = element_text(size = 12), legend.title = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title =  element_text(size = 12))+
  labs(x = "medial <---> lateral", y = "Density")

p2<-ggplot(data = data, aes(x = y, fill =group, color= group))+  
  geom_density(alpha = 0.5)+
  scale_color_manual(name = "Group", values = c("#5CAAD7","#BB1E38"),guide =F)+
  scale_fill_manual(name = "Group", values = c("#5CAAD7","#BB1E38"))+
  theme_bw()+
  theme(panel.grid  = element_blank(),
        legend.position = "none", legend.text = element_text(size = 12), legend.title = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.title =  element_text(size = 12))+
  labs(x = "posterior <---> anterior")

p3<-ggplot(data = data, aes(x = z, fill =group, color= group))+  
  geom_density(alpha = 0.5)+
  scale_color_manual(name = "Group", values = c("#5CAAD7","#BB1E38"),guide =F)+
  scale_fill_manual(name = "Group", values = c("#5CAAD7","#BB1E38"))+
  theme_bw()+
  theme(panel.grid  = element_blank(),
        legend.position = "none", legend.text = element_text(size = 12), legend.title = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.title =  element_text(size = 12))+
  labs(x = "ventral <---> dorsal")


data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/OCIdeltacor.csv",header = TRUE,
                   sep = ",",na.strings = "na")
dat1<- as.matrix(data)
dat1<-as.data.frame(dat1)
dat1$pvalue<-as.numeric(dat1$Pval)
dat1$cor<-as.numeric(dat1$Corr)
dat1$Corr <- cut(abs(dat1$cor), 
                 breaks = c(0,0.3,0.5,0.7,0.9,1),
                 labels = c("< 0.3", "0.3 - 0.5", "0.5 - 0.7", "0.7 - 0.9", "> 0.9"),
                 right = FALSE)
dat1$pvalue1 <- cut(dat1$pvalue, 
                    breaks = c(0, 0.001, 0.01, 0.05, 1),
                    labels = c("< 0.001", "< 0.01", "< 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("Peak power based", "STN based")
dat1$Variable <- factor(dat1$Class, levels = desired_order )

p4<-ggplot(dat1, aes(x = Variable, y = cor,fill = Variable))+
  geom_col(width = 0.5,alpha = 0.5)+
  scale_fill_manual(name = "Group", values = c("#5CAAD7","#BB1E38"))+
  theme_bw()+
  labs(y = "Spearman's r")+
  theme(panel.grid.minor.y  = element_blank(),
        panel.grid.major.y  = element_blank(),
        axis.title = element_blank(),
        axis.title.y =  element_text(size = 12, angle = 90),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text = element_text(size = 12, face = "plain", color = "black"),
        legend.position = "none")

data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/OCIperm.csv",header = TRUE,
                   sep = ",",na.strings = "na")

p5 <- ggplot(data = data, aes(x = r))+  
  geom_histogram(alpha = 0.5, color = "grey", fill = "grey", binwidth = 0.02)+
  geom_segment(aes(x = -0.3185410, y = 0, xend = -0.3185410, yend = 200), colour = "#BB1E38", linewidth = 2, alpha = 0.5)+
  geom_segment(aes(x = -0.2147139, y = 0, xend = -0.2147139, yend = 200), colour = "#5CAAD7", linewidth = 2, alpha = 0.5)+
#  geom_segment(aes(x = -0.3474393, y = 0, xend = -0.3474393, yend = 200), colour = "#5CAAD7", linewidth = 2, alpha = 0.5)+
  theme_bw()+
  theme(panel.grid  = element_blank(),
        legend.position = "none", legend.text = element_text(size = 12), legend.title = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title =  element_text(size = 12))+
  labs(x = "Spatial and spectral null model", y = "Counts")

library(patchwork)
library(cowplot)
(p1|p2|p3)/(p4|p5)

ggsave(
  filename = "OCIloc.png", # 保存的文件名称。通过后缀来决定生成什么格式的图片
  width = 6.5,             # 宽
  height = 4,            # 高
  units = "in",          # 单位
  dpi = 300              # 分辨率DPI
)
############################################################################# corrletaion and mmi distribution
library(ggforce)
library(ggExtra)
library(ggpubr)

data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/onupdrslbetacor.csv",header = TRUE,
                   sep = ",",na.strings = "na")
dat1<- as.matrix(data)
dat1<-as.data.frame(dat1)
dat1$pvalue<-as.numeric(dat1$Pval)
dat1$cor<-as.numeric(dat1$Corr)
dat1$Corr <- cut(abs(dat1$cor), 
                 breaks = c(0,0.3,0.5,0.7,0.9,1),
                 labels = c("< 0.3", "0.3 - 0.5", "0.5 - 0.7", "0.7 - 0.9", "> 0.9"),
                 right = FALSE)
dat1$pvalue1 <- cut(dat1$pvalue, 
                    breaks = c(0, 0.001, 0.01, 0.05, 1),
                    labels = c("< 0.001", "< 0.01", "< 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("Peak power based", "STN based")
dat1$Variable <- factor(dat1$Class, levels = desired_order )

p1<-ggplot(dat1, aes(x = Variable, y = cor, fill = Variable))+
  geom_col(width = 0.5,alpha = 0.5)+
  scale_fill_manual(name = "Group", values = c("#5CAAD7","#BB1E38"))+
  theme_bw()+
  labs(y = "Med on")+
  theme(panel.grid.minor.y  = element_blank(),
        panel.grid.major.y  = element_blank(),
        axis.title = element_blank(),
        axis.title.y =  element_text(size = 12, angle = 90),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text = element_text(size = 12, face = "plain", color = "black"),
        legend.position = "none")+
  scale_y_continuous(limits = c(-0.01,0.4),breaks = c(0,0.1,0.2,0.3,0.4))


data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/onupdrshbetacor.csv",header = TRUE,
                   sep = ",",na.strings = "na")
dat1<- as.matrix(data)
dat1<-as.data.frame(dat1)
dat1$pvalue<-as.numeric(dat1$Pval)
dat1$cor<-as.numeric(dat1$Corr)
dat1$Corr <- cut(abs(dat1$cor), 
                 breaks = c(0,0.3,0.5,0.7,0.9,1),
                 labels = c("< 0.3", "0.3 - 0.5", "0.5 - 0.7", "0.7 - 0.9", "> 0.9"),
                 right = FALSE)
dat1$pvalue1 <- cut(dat1$pvalue, 
                    breaks = c(0, 0.001, 0.01, 0.05, 1),
                    labels = c("< 0.001", "< 0.01", "< 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("Peak power based", "STN based")
dat1$Variable <- factor(dat1$Class, levels = desired_order )

p2<-ggplot(dat1, aes(x = Variable, y = cor, fill = Variable))+
  geom_col(width = 0.5,alpha = 0.5)+
  scale_fill_manual(name = "Group", values = c("#5CAAD7","#BB1E38"))+
  theme_bw()+
  labs(y = "Spearman's r")+
  theme(panel.grid.minor.y  = element_blank(),
        panel.grid.major.y  = element_blank(),
        legend.text = element_text(size = 12), legend.title = element_text(size = 12),
        axis.title = element_blank(),
        axis.title.y =  element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text = element_text(size = 12, face = "plain", color = "black"),
        legend.position = "none")+
  scale_y_continuous(limits = c(-0.01,0.4),breaks = c(0,0.1,0.2,0.3,0.4))



data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/offupdrslbetacor.csv",header = TRUE,
                   sep = ",",na.strings = "na")
dat1<- as.matrix(data)
dat1<-as.data.frame(dat1)
dat1$pvalue<-as.numeric(dat1$Pval)
dat1$cor<-as.numeric(dat1$Corr)
dat1$Corr <- cut(abs(dat1$cor), 
                 breaks = c(0,0.3,0.5,0.7,0.9,1),
                 labels = c("< 0.3", "0.3 - 0.5", "0.5 - 0.7", "0.7 - 0.9", "> 0.9"),
                 right = FALSE)
dat1$pvalue1 <- cut(dat1$pvalue, 
                    breaks = c(0, 0.001, 0.01, 0.05, 1),
                    labels = c("< 0.001", "< 0.01", "< 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("Peak power based","STN based")
dat1$Variable <- factor(dat1$Class, levels = desired_order )

p3<-ggplot(dat1, aes(x = Variable, y = cor, fill = Variable))+
  geom_col(width = 0.5,alpha = 0.5)+
  scale_fill_manual(name = "Group", values = c("#5CAAD7","#BB1E38"))+
  labs(x = "lβ", y = "Med off")+
  theme(panel.grid.minor.y  = element_blank(),
        panel.grid.major.y  = element_blank(),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y =  element_text(size = 12, angle = 90),
        axis.text.x = element_blank(),
        axis.text = element_text(size = 12, face = "plain", color = "black"),
        legend.position = "none")+
  scale_y_continuous(limits = c(-0.01,0.4),breaks = c(0,0.1,0.2,0.3,0.4))


data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/offupdrshbetacor.csv",header = TRUE,
                   sep = ",",na.strings = "na")
dat1<- as.matrix(data)
dat1<-as.data.frame(dat1)
dat1$pvalue<-as.numeric(dat1$Pval)
dat1$cor<-as.numeric(dat1$Corr)
dat1$Corr <- cut(abs(dat1$cor), 
                 breaks = c(0,0.3,0.5,0.7,0.9,1),
                 labels = c("< 0.3", "0.3 - 0.5", "0.5 - 0.7", "0.7 - 0.9", "> 0.9"),
                 right = FALSE)
dat1$pvalue1 <- cut(dat1$pvalue, 
                    breaks = c(0, 0.001, 0.01, 0.05, 1),
                    labels = c("< 0.001", "< 0.01", "< 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("Peak power based", "STN based")
dat1$Variable <- factor(dat1$Class, levels = desired_order )

p4<-ggplot(dat1, aes(x = Variable, y = cor, fill = Variable))+
  geom_col(width = 0.5,alpha = 0.5)+
  scale_fill_manual(name = "Group", values = c("#5CAAD7","#BB1E38"))+
  labs(x = "hβ", y = "Spearman's r")+
  theme(panel.grid.minor.y  = element_blank(),
        panel.grid.major.y  = element_blank(),
        axis.title.y =element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12, face = "plain", color = "black"),
        legend.position = "none")+
  scale_y_continuous(limits = c(-0.01,0.4),breaks = c(0,0.1,0.2,0.3,0.4))

data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/lBetaData.csv",header = TRUE,
                   sep = ",",na.strings = "na") 

p5<-ggplot(data = data, aes(x = x, fill =group, color= group))+  
  geom_density(alpha = 0.5)+
  scale_color_manual(name = "Group", values = c("#5CAAD7","#BB1E38"),guide ="none")+
  scale_fill_manual(name = "Group", values = c("#5CAAD7","#BB1E38"))+
  theme_bw()+
  theme(panel.grid  = element_blank(),
        legend.position = "none", legend.text = element_text(size = 12), legend.title = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title =  element_text(size = 12))+
  labs(x = "medial <---> lateral", y = "Density")

p6<-ggplot(data = data, aes(x = y, fill =group, color= group))+  
  geom_density(alpha = 0.5)+
  scale_color_manual(name = "Group", values = c("#5CAAD7","#BB1E38"),guide ="none")+
  scale_fill_manual(name = "Group", values = c("#5CAAD7","#BB1E38"))+
  theme_bw()+
  theme(panel.grid  = element_blank(),
        legend.position = "none", legend.text = element_text(size = 12), legend.title = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.title =  element_text(size = 12))+
  labs(x = "posterior <---> anterior")

p7<-ggplot(data = data, aes(x = z, fill =group, color= group))+  
  geom_density(alpha = 0.5)+
  scale_color_manual(name = "Group", values = c("#5CAAD7","#BB1E38"),guide ="none")+
  scale_fill_manual(name = "Group", values = c("#5CAAD7","#BB1E38"))+
  theme_bw()+
  theme(panel.grid  = element_blank(),
        legend.position = "none", legend.text = element_text(size = 12), legend.title = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.title =  element_text(size = 12))+
  labs(x = "ventral <---> dorsal")


library(patchwork)
library(cowplot)
(p5|p6|p7)/(p1|p2)/(p3|p4)+plot_layout(heights = c(1.5, 1, 1))
  #plot_annotation(
 # title = "Spearman correlation", 
  #subtitle = "between MDS-UPDRS III scores and β absolute power within the STN",
 # theme = theme(plot.title = element_text(hjust = 0.3,size = 14, face = "bold"),
  #              plot.subtitle = element_text(hjust = 0.3,size = 12)))

ggsave(
  filename = "UPDRSloc.png", # 保存的文件名称。通过后缀来决定生成什么格式的图片
  width = 7,             # 宽
  height = 5,            # 高
  units = "in",          # 单位
  dpi = 300              # 分辨率DPI
)
#################################################################################Apathy
data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/hBetaData.csv",header = TRUE,
                   sep = ",",na.strings = "na") 

p1<-ggplot(data = data, aes(x = x, fill =group, color= group))+  
  geom_density(alpha = 0.5)+
  scale_color_manual(name = "Group", values = c("#5CAAD7","#BB1E38"),guide ="none")+
  scale_fill_manual(name = "Group", values = c("#5CAAD7","#BB1E38"))+
  theme_bw()+
  theme(panel.grid  = element_blank(),
        legend.position = "none", legend.text = element_text(size = 12), legend.title = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title =  element_text(size = 12))+
  labs(x = "medial <---> lateral", y = "Density")

p2<-ggplot(data = data, aes(x = y, fill =group, color= group))+  
  geom_density(alpha = 0.5)+
  scale_color_manual(name = "Group", values = c("#5CAAD7","#BB1E38"),guide ="none")+
  scale_fill_manual(name = "Group", values = c("#5CAAD7","#BB1E38"))+
  theme_bw()+
  theme(panel.grid  = element_blank(),
        legend.position = "none", legend.text = element_text(size = 12), legend.title = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.title =  element_text(size = 12))+
  labs(x = "posterior <---> anterior")

p3<-ggplot(data = data, aes(x = z, fill =group, color= group))+  
  geom_density(alpha = 0.5)+
  scale_color_manual(name = "Group", values = c("#5CAAD7","#BB1E38"),guide ="none")+
  scale_fill_manual(name = "Group", values = c("#5CAAD7","#BB1E38"))+
  theme_bw()+
  theme(panel.grid  = element_blank(),
        legend.position = "none", legend.text = element_text(size = 12), legend.title = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.title =  element_text(size = 12))+
  labs(x = "ventral <---> dorsal")


data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/AEShbetacor.csv",header = TRUE,
                   sep = ",",na.strings = "na")
dat1<- as.matrix(data)
dat1<-as.data.frame(dat1)
dat1$pvalue<-as.numeric(dat1$Pval)
dat1$cor<-as.numeric(dat1$Corr)
dat1$Corr <- cut(abs(dat1$cor), 
                 breaks = c(0,0.3,0.5,0.7,0.9,1),
                 labels = c("< 0.3", "0.3 - 0.5", "0.5 - 0.7", "0.7 - 0.9", "> 0.9"),
                 right = FALSE)
dat1$pvalue1 <- cut(dat1$pvalue, 
                    breaks = c(0, 0.001, 0.01, 0.05, 1),
                    labels = c("< 0.001", "< 0.01", "< 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("Peak power based", "STN based")
dat1$Variable <- factor(dat1$Class, levels = desired_order )

p4<-ggplot(dat1, aes(x = Variable, y = cor,fill = Variable))+
  geom_col(width = 0.5,alpha = 0.5)+
  scale_fill_manual(name = "Group", values = c("#5CAAD7","#BB1E38"))+
  theme_bw()+
  labs(y = "Spearman's r")+
  theme(panel.grid.minor.y  = element_blank(),
        panel.grid.major.y  = element_blank(),
        axis.title = element_blank(),
        axis.title.y =  element_text(size = 12, angle = 90),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text = element_text(size = 12, face = "plain", color = "black"),
        legend.position = "none")

data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/AESperm.csv",header = TRUE,
                   sep = ",",na.strings = "na")

p5 <- ggplot(data = data, aes(x = r))+  
  geom_histogram(alpha = 0.5, color = "grey", fill = "grey", binwidth = 0.02)+
  geom_segment(aes(x = 0.3820277, y = 0, xend = 0.3820277, yend = 200), colour = "#BB1E38", linewidth = 2, alpha = 0.5)+
  geom_segment(aes(x = 0.3066937, y = 0, xend = 0.3066937, yend = 200), colour = "#5CAAD7", linewidth = 2, alpha = 0.5)+
 # geom_segment(aes(x = -0.0841013, y = 0, xend = -0.0841013, yend = 200), colour = "#5CAAD7", linewidth = 2, alpha = 0.5)+
  theme_bw()+
  theme(panel.grid  = element_blank(),
        legend.position = "none", legend.text = element_text(size = 12), legend.title = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title =  element_text(size = 12))+
  labs(x = "Spatial and spectral null model", y = "Counts")

library(patchwork)
library(cowplot)
(p1|p2|p3)/(p4|p5)

ggsave(
  filename = "AESloc.png", # 保存的文件名称。通过后缀来决定生成什么格式的图片
  width = 6.5,             # 宽
  height = 4,            # 高
  units = "in",          # 单位
  dpi = 300              # 分辨率DPI
)
##########################################################################Impulsivity
library(ggforce)
library(ggExtra)
library(ggpubr)
data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/lGammaData.csv",header = TRUE,
                   sep = ",",na.strings = "na") 

p1<-ggplot(data = data, aes(x = x, fill =group, color= group))+  
  geom_density(alpha = 0.5)+
  scale_color_manual(name = "Group", values = c("#5CAAD7","#BB1E38"),guide =F)+
  scale_fill_manual(name = "Group", values = c("#5CAAD7","#BB1E38"))+
  theme_bw()+
  theme(panel.grid  = element_blank(),
        legend.position = "none", legend.text = element_text(size = 12), legend.title = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title =  element_text(size = 12))+
  labs(x = "medial <---> lateral", y = "Density")

p2<-ggplot(data = data, aes(x = y, fill =group, color= group))+  
  geom_density(alpha = 0.5)+
  scale_color_manual(name = "Group", values = c("#5CAAD7","#BB1E38"),guide =F)+
  scale_fill_manual(name = "Group", values = c("#5CAAD7","#BB1E38"))+
  theme_bw()+
  theme(panel.grid  = element_blank(),
        legend.position = "none", legend.text = element_text(size = 12), legend.title = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.title =  element_text(size = 12))+
  labs(x = "posterior <---> anterior")

p3<-ggplot(data = data, aes(x = z, fill =group, color= group))+  
  geom_density(alpha = 0.5)+
  scale_color_manual(name = "Group", values = c("#5CAAD7","#BB1E38"),guide =F)+
  scale_fill_manual(name = "Group", values = c("#5CAAD7","#BB1E38"))+
  theme_bw()+
  theme(panel.grid  = element_blank(),
        legend.position = "none", legend.text = element_text(size = 12), legend.title = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.title =  element_text(size = 12))+
  labs(x = "ventral <---> dorsal")


data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/UPPSlgammacor.csv",header = TRUE,
                   sep = ",",na.strings = "na")
dat1<- as.matrix(data)
dat1<-as.data.frame(dat1)
dat1$pvalue<-as.numeric(dat1$Pval)
dat1$cor<-as.numeric(dat1$Corr)
dat1$Corr <- cut(abs(dat1$cor), 
                 breaks = c(0,0.3,0.5,0.7,0.9,1),
                 labels = c("< 0.3", "0.3 - 0.5", "0.5 - 0.7", "0.7 - 0.9", "> 0.9"),
                 right = FALSE)
dat1$pvalue1 <- cut(dat1$pvalue, 
                    breaks = c(0, 0.001, 0.01, 0.05, 1),
                    labels = c("< 0.001", "< 0.01", "< 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("Peak power based", "STN based")
dat1$Variable <- factor(dat1$Class, levels = desired_order )

p4<-ggplot(dat1, aes(x = Variable, y = cor,fill = Variable))+
  geom_col(width = 0.5,alpha = 0.5)+
  scale_fill_manual(name = "Group", values = c("#5CAAD7","#BB1E38"))+
  theme_bw()+
  labs(y = "Spearman's r")+
  theme(panel.grid.minor.y  = element_blank(),
        panel.grid.major.y  = element_blank(),
        axis.title = element_blank(),
        axis.title.y =  element_text(size = 12, angle = 90),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text = element_text(size = 12, face = "plain", color = "black"),
        legend.position = "none")

data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/UPPSperm.csv",header = TRUE,
                   sep = ",",na.strings = "na")

p5 <- ggplot(data = data, aes(x = r))+  
  geom_histogram(alpha = 0.5, color = "grey", fill = "grey", binwidth = 0.02)+
  geom_segment(aes(x = -0.3734047, y = 0, xend = -0.3734047, yend = 200), colour = "#BB1E38", linewidth = 2, alpha = 0.5)+
  geom_segment(aes(x = -0.4022147, y = 0, xend = -0.4022147, yend = 200), colour = "#5CAAD7", linewidth = 2, alpha = 0.5)+
#  geom_segment(aes(x = -0.1019144, y = 0, xend = -0.1019144, yend = 200), colour = "#5CAAD7", linewidth = 2, alpha = 0.5)+
  theme_bw()+
  theme(panel.grid  = element_blank(),
        legend.position = "none", legend.text = element_text(size = 12), legend.title = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title =  element_text(size = 12))+
  labs(x = "Spatial and spectral null model", y = "Counts")

library(patchwork)
library(cowplot)
(p1|p2|p3)/(p4|p5)

ggsave(
  filename = "UPPSloc.png", # 保存的文件名称。通过后缀来决定生成什么格式的图片
  width = 6.5,             # 宽
  height = 4,            # 高
  units = "in",          # 单位
  dpi = 300              # 分辨率DPI
)
