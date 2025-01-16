library(ggplot2)
data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/AESCoherence.csv",header = TRUE,
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
                    breaks = c(0, 0.0071, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "Unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")
dat1$Variable <- factor(dat1$Class, levels = desired_order )

p1<-ggplot(dat1, aes(x = Variable, y = cor, fill = pvalue1))+
  scale_fill_manual (name="Pvalue",
                     values = c("#B2446B",
                                "grey80"))+
  geom_col()+
  # geom_segment (aes(y = 0, x = Class, yend = cor, xend = Class), linewidth = 0.8)+
  # geom_point(aes(size = Corr))+
  theme_test()+
  #  geom_hline(yintercept = c(0.0) , linewidth = 0.5) +
  labs( title = "Apathy")+
  theme(axis.line = element_line(linewidth  = 0.25),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), 'cm'),
        plot.title = element_text(hjust = 0.5,size = 12,face = "bold"),
        axis.ticks = element_line(colour = "black", linewidth = 0.25),
        axis.title = element_text(size = 12),
        axis.title.y =  element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 12, face = "plain", color = "black"),
        legend.position = "none",
        legend.text = element_text(size = 12, family = "Helvetica"), legend.title = element_text(size = 12, family = "Helvetica"))+
  coord_cartesian(clip = 'off' ,ylim = c(-0.55,0.55))

data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/BAICoherence.csv",header = TRUE,
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
                    breaks = c(0, 0.0071, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "Unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")
dat1$Variable <- factor(dat1$Class, levels = desired_order )

p2<-ggplot(dat1, aes(x = Variable, y = cor, fill = pvalue1))+
  scale_fill_manual (name="Pvalue",
                     values = c("grey80"
                               ))+
  geom_col()+
  theme_test()+
  labs( title = "Anxiety",y = "Spearman's r")+
  theme(axis.line = element_line(linewidth  = 0.25),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), 'cm'),
        plot.title = element_text(hjust = 0.5,size = 12,face = "bold"),
        axis.ticks = element_line(colour = "black", linewidth = 0.25),
        axis.title = element_text(size = 12),
        axis.title.y =  element_text(size = 12),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 12, face = "plain", color = "black"),
        legend.position = "none",
        legend.text = element_text(size = 12, family = "Helvetica"), legend.title = element_text(size = 12, family = "Helvetica"))+
  coord_cartesian(clip = 'off' ,ylim = c(-0.55,0.55))

data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/OCICoherence.csv",header = TRUE,
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
                    breaks = c(0, 0.0071, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "Unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")
dat1$Variable <- factor(dat1$Class, levels = desired_order )

p3<-ggplot(dat1, aes(x = Variable, y = cor, fill = pvalue1))+
  scale_fill_manual (name="Pvalue",
                     values = c("grey80"
                     ))+
  geom_col()+
  theme_test()+
  labs(title = "Compulsivity")+
  theme(axis.line = element_line(linewidth  = 0.25),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), 'cm'),
        plot.title = element_text(hjust = 0.5,size = 12,face = "bold"),
        axis.ticks = element_line(colour = "black", linewidth = 0.25),
        axis.title = element_text(size = 12),
        axis.title.y =  element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 12, face = "plain", color = "black"),
        legend.position = "none",
        legend.text = element_text(size = 12, family = "Helvetica"), legend.title = element_text(size = 12, family = "Helvetica"))+
  coord_cartesian(clip = 'off' ,ylim = c(-0.55,0.55))

data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/BDICoherence.csv",header = TRUE,
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
                    breaks = c(0, 0.0071, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "Unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")
dat1$Variable <- factor(dat1$Class, levels = desired_order )

p4<-ggplot(dat1, aes(x = Variable, y = cor, fill = pvalue1))+
  scale_fill_manual (name="Pvalue",
                     values = c("#B2446B",
                                "grey80"))+
  geom_col()+
  theme_test()+
  labs(title = "Depression")+
  theme(axis.line = element_line(linewidth  = 0.25),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), 'cm'),
        plot.title = element_text(hjust = 0.5,size = 12,face = "bold"),
        axis.ticks = element_line(colour = "black", linewidth = 0.25),
        axis.title = element_text(size = 12),
        axis.title.y =  element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 12, face = "plain", color = "black"),
        legend.position = "none",
        legend.text = element_text(size = 12, family = "Helvetica"), legend.title = element_text(size = 12, family = "Helvetica"))+
  coord_cartesian(clip = 'off' ,ylim = c(-0.55,0.55))

data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/UPPSCoherence.csv",header = TRUE,
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
                    breaks = c(0, 0.0071, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "Unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")
dat1$Variable <- factor(dat1$Class, levels = desired_order )

p5<-ggplot(dat1, aes(x = Variable, y = cor, fill = pvalue1))+
  scale_fill_manual (name="Pvalue",
                     values = c("grey80"
                     ))+
  geom_col()+
  theme_test()+
  labs(title = "Impulsivity")+
  theme(axis.line = element_line(linewidth  = 0.25),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), 'cm'),
        plot.title = element_text(hjust = 0.5,size = 12,face="bold"),
        axis.ticks = element_line(colour = "black", linewidth = 0.25),
        axis.title = element_text(size = 12),
        axis.title.y =  element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 12, face = "plain", color = "black"),
        legend.position = "none",
        legend.text = element_text(size = 12, family = "Helvetica"), legend.title = element_text(size = 12, family = "Helvetica"))+
  coord_cartesian(clip = 'off' ,ylim = c(-0.55,0.55))

p2|p4|p1|p3|p5

ggsave(
  filename = "Coherence.png", # 保存的文件名称。通过后缀来决定生成什么格式的图片
  width = 10,             # 宽
  height = 2.5,            # 高
  units = "in",          # 单位
  dpi = 300              # 分辨率DPI
)
###############################################################################
data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/AESPAC.csv",header = TRUE,
                   sep = ",",na.strings = "na")

data$pvalue1 <- cut(data$Cpvalue, 
                    breaks = c(-0.1, 0.5, 1.1),
                    labels = c("< 0.05", "> 0.05"),
                    right = FALSE)

p1<-ggplot(data) +
  geom_tile(aes(x, y, fill = z, alpha = pvalue1))+
  scale_alpha_manual(values = c(1,0.4), guide = F)+
  scale_fill_viridis_c(name = "Spearman's r",limit = c(-0.6,0.6))+
  theme_minimal()+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5,size = 12,face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_blank())+
  labs(title = "Apathy",x = "Phase Frequency(Hz)")+
  scale_x_continuous(expand = c(0,0),limits = c(0,13),breaks = c(1,4,8,12))

data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/AESPACband.csv",header = TRUE,
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
                    breaks = c(0, 0.017, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "Unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α")
dat1$Variable <- factor(dat1$Class, levels = desired_order )

p6<-ggplot(dat1, aes(x = Variable, y = cor, fill = pvalue1))+
  scale_fill_manual (name="Pvalue",
                     values = c("#639AC8",
                                "grey80"
                               ))+
  geom_col(width = 0.5)+
  theme_bw()+
  labs( x = "Phase Frequency",y = "Spearman's r")+
  theme(panel.grid.minor.y  = element_blank(),
        panel.grid.major.y  = element_blank(),
        
        legend.text = element_text(size = 12),
        axis.title = element_blank(),
        axis.title.y =  element_blank(),
        axis.title.x = element_text(size = 12),
        axis.text = element_text(size = 12, face = "plain", color = "black"),
        legend.position = "none")+
  scale_y_continuous(limits = c(-0.5,0.3),breaks = c(-0.5,-0.2,0.2))

data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/BAIPAC.csv",header = TRUE,
                   sep = ",",na.strings = "na")

data$pvalue1 <- cut(data$Cpvalue, 
                    breaks = c(-0.1, 0.5, 1.1),
                    labels = c("< 0.05", "> 0.05"),
                    right = FALSE)

p2<-ggplot(data) +
  geom_tile(aes(x, y, fill = z, alpha = pvalue1))+
  scale_alpha_manual(values = c(0.4), guide = F)+
  scale_fill_viridis_c(name = "Spearman's r",limit = c(-0.6,0.6))+
  theme_minimal()+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5,size = 12,face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))+
  labs(title = "Anxiety",x = "Phase Frequency(Hz)", y = "Amplitude Frequency(Hz)")+
  scale_x_continuous(expand = c(0,0),limits = c(0,13),breaks = c(1,4,8,12))

data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/BAIPACband.csv",header = TRUE,
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
                    breaks = c(0, 0.017, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "Unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α")
dat1$Variable <- factor(dat1$Class, levels = desired_order )

p7<-ggplot(dat1, aes(x = Variable, y = cor, fill = pvalue1))+
  scale_fill_manual (name="Pvalue",
                     values = c(
                                "grey80"
                               ))+
  geom_col(width = 0.5)+
  theme_bw()+
  labs( x = "Phase Frequency",y = "Spearman's r")+
  theme(panel.grid.minor.y  = element_blank(),
        panel.grid.major.y  = element_blank(),
        
        legend.text = element_text(size = 12),
        axis.title = element_blank(),
        axis.title.y =  element_text(size = 12, angle = 90),
        axis.title.x = element_text(size = 12),
        axis.text = element_text(size = 12, face = "plain", color = "black"),
        legend.position = "none")+
  scale_y_continuous(limits = c(-0.5,0.3),breaks = c(-0.5,-0.2,0.2))
data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/BDIPAC.csv",header = TRUE,
                   sep = ",",na.strings = "na")

data$pvalue1 <- cut(data$Cpvalue, 
                    breaks = c(-0.1, 0.5, 1.1),
                    labels = c("< 0.05", "> 0.05"),
                    right = FALSE)

p3<-ggplot(data) +
  geom_tile(aes(x, y, fill = z, alpha = pvalue1))+
  scale_alpha_manual(values = c(0.4), guide = F)+
  scale_fill_viridis_c(name = "Spearman's r",limit = c(-0.6,0.6))+
  theme_minimal()+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5,size = 12,face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_blank())+
  labs(title = "Depression",x = "Phase Frequency(Hz)")+
  scale_x_continuous(expand = c(0,0),limits = c(0,13),breaks = c(1,4,8,12))

data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/BDIPACband.csv",header = TRUE,
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
                    breaks = c(0, 0.017, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "Unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α")
dat1$Variable <- factor(dat1$Class, levels = desired_order )

p8<-ggplot(dat1, aes(x = Variable, y = cor, fill = pvalue1))+
  scale_fill_manual (name="Pvalue",
                     values = c(
                                "grey80"
                            ))+
  geom_col(width = 0.5)+
  theme_bw()+
  labs( x = "Phase Frequency",y = "Spearman's r")+
  theme(panel.grid.minor.y  = element_blank(),
        panel.grid.major.y  = element_blank(),
        
        legend.text = element_text(size = 12),
        axis.title = element_blank(),
        axis.title.y =  element_blank(),
        axis.title.x = element_text(size = 12),
        axis.text = element_text(size = 12, face = "plain", color = "black"),
        legend.position = "none")+
  scale_y_continuous(limits = c(-0.5,0.3),breaks = c(-0.5,-0.2,0.2))
data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/OCIPAC.csv",header = TRUE,
                   sep = ",",na.strings = "na")

data$pvalue1 <- cut(data$Cpvalue, 
                    breaks = c(-0.1, 0.5, 1.1),
                    labels = c("< 0.05", "> 0.05"),
                    right = FALSE)

p4<-ggplot(data) +
  geom_tile(aes(x, y, fill = z, alpha = pvalue1))+
  scale_alpha_manual(values = c(1,0.4), guide = F)+
  scale_fill_viridis_c(name = "Spearman's r",limit = c(-0.6,0.6))+
  theme_minimal()+
  theme(legend.position = "none",
       plot.title = element_text(hjust = 0.5,size = 12,face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_blank())+
  labs(title = "Compulsivity",x = "Phase Frequency(Hz)")+
  scale_x_continuous(expand = c(0,0),limits = c(0,13),breaks = c(1,4,8,12))

data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/OCIPACband.csv",header = TRUE,
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
                    breaks = c(0, 0.017, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "Unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α")
dat1$Variable <- factor(dat1$Class, levels = desired_order )

p9<-ggplot(dat1, aes(x = Variable, y = cor, fill = pvalue1))+
  scale_fill_manual (name="Pvalue",
                     values = c(
                                "grey80"
                                ))+
  geom_col(width = 0.5)+
  theme_bw()+
  labs( x = "Phase Frequency",y = "Spearman's r")+
  theme(panel.grid.minor.y  = element_blank(),
        panel.grid.major.y  = element_blank(),
        
        legend.text = element_text(size = 12),
        axis.title = element_blank(),
        axis.title.y =  element_blank(),
        axis.title.x = element_text(size = 12),
        axis.text = element_text(size = 12, face = "plain", color = "black"),
        legend.position = "none")+
  scale_y_continuous(limits = c(-0.5,0.3),breaks = c(-0.5,-0.2,0.2))

data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/UPPSPAC.csv",header = TRUE,
                   sep = ",",na.strings = "na")

data$pvalue1 <- cut(data$Cpvalue, 
                    breaks = c(-0.1, 0.5, 1.1),
                    labels = c("< 0.05", "> 0.05"),
                    right = FALSE)

p5<-ggplot(data) +
  geom_tile(aes(x, y, fill = z, alpha = pvalue1))+
  scale_alpha_manual(values = c(0.4), guide = F)+
  scale_fill_viridis_c(name = "Spearman's r",limit = c(-0.6,0.6))+
  theme_minimal()+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5,size = 12,face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_blank())+
  labs(title = "Impulsivity",x = "Phase Frequency(Hz)")+
  scale_x_continuous(expand = c(0,0),limits = c(0,13),breaks = c(1,4,8,12))

data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/UPPSPACband.csv",header = TRUE,
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
                    breaks = c(0, 0.017, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "Unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α")
dat1$Variable <- factor(dat1$Class, levels = desired_order )

p10<-ggplot(dat1, aes(x = Variable, y = cor, fill = pvalue1))+
  scale_fill_manual (name="Pvalue",
                     values = c(
                                "grey80"
                                ))+
  geom_col(width = 0.5)+
  theme_bw()+
  labs( x = "Phase Frequency",y = "Spearman's r")+
  theme(panel.grid.minor.y  = element_blank(),
        panel.grid.major.y  = element_blank(),
        
        legend.text = element_text(size = 12),
        axis.title = element_blank(),
        axis.title.y =  element_blank(),
        axis.title.x = element_text(size = 12),
        axis.text = element_text(size = 12, face = "plain", color = "black"),
        legend.position = "none")+
  scale_y_continuous(limits = c(-0.5,0.3),breaks = c(-0.5,-0.2,0.2))

library(patchwork)
library(cowplot)
(p2|p3|p1|p4|p5)/(p7|p8|p6|p9|p10)+plot_layout(ncol = 1, heights = c(2,1))

ggsave(
  filename = "PAC.png", # 保存的文件名称。通过后缀来决定生成什么格式的图片
  width = 10,             # 宽
  height = 4,            # 高
  units = "in",          # 单位
  dpi = 300              # 分辨率DPI
)