## group frequency power
library(ggplot2)

############### BDI
data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/relBDIgroup.csv",header = TRUE,
                   sep = ",",na.strings = "na")
data$sig <- cut(data$pvalue, 
                breaks = c(0, 0.05, 1),
                labels = c("< 0.05", "> 0.05"),
                right = FALSE)

p1<-ggplot(data, aes (x = Freq)) +
  geom_ribbon(aes (ymin = GP2SD2, ymax = GP2SD1), alpha = .2,
              fill = "#3569AB", color = "transparent") +
  geom_line (aes(y = GP2mean,color = "with depression (N=33)"), lwd = 1) +
  geom_ribbon(aes(ymin = GP1SD2, ymax = GP1SD1), alpha = .2,
              fill = "#B02418", color = "transparent") +
  geom_line (aes(y = GP1mean,color = "without depression (N=14)"), lwd = 1) +
  scale_colour_manual(name = "Patient group",values = c("with depression (N=33)" = "#3569AB","without depression (N=14)" = "#B02418"))+
  labs (x = "Frequency(Hz)", y = "Rel.Pow")+
  theme_classic()+
  theme(legend.position = "right", legend.title = element_blank(),
        legend.text = element_text(size = 14, family = "Helvetica"),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(family = "Helvetica",size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(family = "Helvetica",size = 14))+
  scale_x_continuous(expand = c(0,0),limits = c(0,90),breaks = c(0,30,60,90))+
  geom_rect(data = subset(data, sig == "< 0.05"),
            aes(xmin = Freq-0.9765625/2, xmax = Freq + 0.9765625/2, ymin = -Inf, ymax = Inf),
            fill = "grey30",alpha = 0.2) 

##############################
data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/absBDIgroup.csv",header = TRUE,
                   sep = ",",na.strings = "na")
data$sig <- cut(data$pvalue, 
                breaks = c(0, 0.05, 1),
                labels = c("< 0.05", "> 0.05"),
                right = FALSE)

p2<-ggplot(data, aes (x = Freq)) +
  geom_ribbon(aes (ymin = GP2SD2, ymax = GP2SD1), alpha = .2,
              fill = "#3569AB", color = "transparent") +
  geom_line (aes(y = GP2mean,color = "with depression (N=33)"), lwd = 1) +
  geom_ribbon(aes(ymin = GP1SD2, ymax = GP1SD1), alpha = .2,
              fill = "#B02418", color = "transparent") +
  geom_line (aes(y = GP1mean,color = "without depression (N=14)"), lwd = 1) +
  scale_colour_manual(name = "Patient group",values = c("with depression (N=33)" = "#3569AB","without depression (N=14)" = "#B02418"))+
  labs (x = "Frequency(Hz)", y ="10*log10(Abs.Pow)")+
  theme_classic()+
  theme(legend.position = "none", legend.title = element_blank(),
        legend.text = element_text(size = 14, family = "Helvetica"),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(family = "Helvetica",size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(family = "Helvetica",size = 14))+
  scale_x_continuous(expand = c(0,0),limits = c(0,90),breaks = c(0,30,60,90))+
  geom_rect(data = subset(data, sig == "< 0.05"),
            aes(xmin = Freq-0.9765625/2, xmax = Freq + 0.9765625/2, ymin = -Inf, ymax = Inf),
            fill = "grey30",alpha = 0.2) 


data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/DepRelGroupBand.csv",header = TRUE,
                   sep = ",",na.strings = "na")
dat1<- as.matrix(data)
dat1<-as.data.frame(dat1)
dat1$pvalue<-as.numeric(dat1$Pval)
dat1$zval<-as.numeric(dat1$Zval)


dat1$pvalue1 <- cut(dat1$pvalue, 
                    breaks = c(0, 0.0071, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "Unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")
dat1$Variable <- factor(dat1$Class, levels = desired_order )

p3<-ggplot(dat1, aes(x = Variable, y = zval, fill = pvalue1))+
  scale_fill_manual (name="Pvalue",
                     values = c("#B2446B",
                                "#D6BEB9",
                                "grey80"))+
  geom_col()+
  theme_void()+
  theme(
        plot.margin = unit(c(0.3,0.3,0.3,0.3), 'cm'),
        plot.title = element_text(hjust = 0.5,size = 14, face = "bold"),
        axis.ticks.x = element_line(colour = "black", linewidth = 0.25),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        axis.title.y =  element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14, face = "plain", color = "black"),
        axis.text.y = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 14, family = "Helvetica"), legend.title = element_text(size = 14, family = "Helvetica"))+
  coord_cartesian(clip = 'off')


data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/DepAbsGroupBand.csv",header = TRUE,
                   sep = ",",na.strings = "na")
dat1<- as.matrix(data)
dat1<-as.data.frame(dat1)
dat1$pvalue<-as.numeric(dat1$Pval)
dat1$zval<-as.numeric(dat1$Zval)


dat1$pvalue1 <- cut(dat1$pvalue, 
                    breaks = c(0, 0.0071, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "Unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")
dat1$Variable <- factor(dat1$Class, levels = desired_order )

p4<-ggplot(dat1, aes(x = Variable, y = zval, fill = pvalue1))+
  scale_fill_manual (name="Pvalue",
                     values = c("#D6BEB9",
                                "grey80"))+
  geom_col()+
  theme_void()+
  theme(
    plot.margin = unit(c(0.3,0.3,0.3,0.3), 'cm'),
    plot.title = element_text(hjust = 0.5,size = 14, face = "bold"),
    axis.ticks.x = element_line(colour = "black", linewidth = 0.25),
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    axis.title.y =  element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 14, face = "plain", color = "black"),
    axis.text.y = element_blank(),
    legend.position = "none",
    legend.text = element_text(size = 14, family = "Helvetica"), legend.title = element_text(size = 14, family = "Helvetica"))+
  coord_cartesian(clip = 'off')

library(patchwork)
library(cowplot)
p2+p1+p4+p3+plot_layout(ncol = 2,nrow= 2,height = c(2,1))

ggsave(
  filename = "BDIbandgroup.png", # 保存的文件名称。通过后缀来决定生成什么格式的图片
  width = 12,             # 宽
  height = 4,            # 高
  units = "in",          # 单位
  dpi = 300              # 分辨率DPI
)


###############################################################################
##BAI group
data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/relBAIgroup.csv",header = TRUE,
                   sep = ",",na.strings = "na")
data$sig <- cut(data$pvalue, 
                breaks = c(0, 0.05, 1),
                labels = c("< 0.05", "> 0.05"),
                right = FALSE)

p1<-ggplot(data, aes (x = Freq)) +
  geom_ribbon(aes (ymin = GP2SD2, ymax = GP2SD1), alpha = .2,
              fill = "#639AC8", color = "transparent") +
  geom_line (aes(y = GP2mean,color = "with anxiety (N=26)"), lwd = 1) +
  geom_ribbon(aes(ymin = GP1SD2, ymax = GP1SD1), alpha = .2,
              fill = "#B02418", color = "transparent") +
  geom_line (aes(y = GP1mean,color = "without anxiety (N=17)"), lwd = 1) +
  scale_colour_manual(name = "Patient group",values = c("with anxiety (N=26)" = "#3569AB","without anxiety (N=17)" ="#B02418"))+
  labs (x = "Frequency(Hz)", y = "Rel.Pow)")+
  theme_classic()+
  theme(legend.position = "right", legend.title = element_blank(),
        legend.text = element_text(size = 14, family = "Helvetica"),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(family = "Helvetica",size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(family = "Helvetica",size = 14))+
  scale_x_continuous(expand = c(0,0),limits = c(0,90),breaks = c(0,30,60,90))+
  geom_rect(data = subset(data, sig == "< 0.05"),
            aes(xmin = Freq, xmax = Freq + 0.9765625, ymin = -Inf, ymax = Inf),
            fill = "grey30",alpha = 0.2) 


data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/absBAIgroup.csv",header = TRUE,
                   sep = ",",na.strings = "na")
data$sig <- cut(data$pvalue, 
                breaks = c(0, 0.05, 1),
                labels = c("< 0.05", "> 0.05"),
                right = FALSE)

p2<-ggplot(data, aes (x = Freq)) +
  geom_ribbon(aes (ymin = GP2SD2, ymax = GP2SD1), alpha = .2,
              fill = "#639AC8", color = "transparent") +
  geom_line (aes(y = GP2mean,color = "with anxiety (N=26)"), lwd = 1) +
  geom_ribbon(aes(ymin = GP1SD2, ymax = GP1SD1), alpha = .2,
              fill = "#B02418", color = "transparent") +
  geom_line (aes(y = GP1mean,color = "without anxiety (N=17)"), lwd = 1) +
  scale_colour_manual(name = "Patient group",values = c("with anxiety (N=26)" = "#3569AB","without anxiety (N=17)" ="#B02418"))+
  labs (x = "Frequency(Hz)", y = "10*log10(Abs.Pow)")+
  theme_classic()+
  theme(legend.position = "none", legend.title = element_blank(),
        legend.text = element_text(size = 14, family = "Helvetica"),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(family = "Helvetica",size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(family = "Helvetica",size = 14))+
  scale_x_continuous(expand = c(0,0),limits = c(0,90),breaks = c(0,30,60,90))+
  geom_rect(data = subset(data, sig == "< 0.05"),
            aes(xmin = Freq, xmax = Freq + 0.9765625, ymin = -Inf, ymax = Inf),
            fill = "grey30",alpha = 0.2) 

data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/AnxietyRelGroupBand.csv",header = TRUE,
                   sep = ",",na.strings = "na")
dat1<- as.matrix(data)
dat1<-as.data.frame(dat1)
dat1$pvalue<-as.numeric(dat1$Pval)
dat1$zval<-as.numeric(dat1$Zval)


dat1$pvalue1 <- cut(dat1$pvalue, 
                    breaks = c(0, 0.0071, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "Unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")
dat1$Variable <- factor(dat1$Class, levels = desired_order )

p3<-ggplot(dat1, aes(x = Variable, y = zval, fill = pvalue1))+
  scale_fill_manual (name="Pvalue",
                     values = c(
                                "grey80"))+
  geom_col()+
  theme_void()+
  theme(
    plot.margin = unit(c(0.3,0.3,0.3,0.3), 'cm'),
    plot.title = element_text(hjust = 0.5,size = 14, face = "bold"),
    axis.ticks.x = element_line(colour = "black", linewidth = 0.25),
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    axis.title.y =  element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 14, face = "plain", color = "black"),
    axis.text.y = element_blank(),
    legend.position = "right",
    legend.text = element_text(size = 14, family = "Helvetica"), legend.title = element_text(size = 14, family = "Helvetica"))+
  coord_cartesian(clip = 'off')


data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/AnxietyAbsGroupBand.csv",header = TRUE,
                   sep = ",",na.strings = "na")
dat1<- as.matrix(data)
dat1<-as.data.frame(dat1)
dat1$pvalue<-as.numeric(dat1$Pval)
dat1$zval<-as.numeric(dat1$Zval)


dat1$pvalue1 <- cut(dat1$pvalue, 
                    breaks = c(0, 0.0071, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "Unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")
dat1$Variable <- factor(dat1$Class, levels = desired_order )

p4<-ggplot(dat1, aes(x = Variable, y = zval, fill = pvalue1))+
  scale_fill_manual (name="Pvalue",
                     values = c(
                                "grey80"))+
  geom_col()+
  theme_void()+
  theme(
    plot.margin = unit(c(0.3,0.3,0.3,0.3), 'cm'),
    plot.title = element_text(hjust = 0.5,size = 14, face = "bold"),
    axis.ticks.x = element_line(colour = "black", linewidth = 0.25),
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    axis.title.y =  element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 14, face = "plain", color = "black"),
    axis.text.y = element_blank(),
    legend.position = "none",
    legend.text = element_text(size = 14, family = "Helvetica"), legend.title = element_text(size = 14, family = "Helvetica"))+
  coord_cartesian(clip = 'off')

library(patchwork)
library(cowplot)
p2+p1+p4+p3+plot_layout(ncol = 2,nrow= 2,height = c(2,1))


ggsave(
  filename = "BAIbandgroup.png", # 保存的文件名称。通过后缀来决定生成什么格式的图片
  width = 12,             # 宽
  height = 4,            # 高
  units = "in",          # 单位
  dpi = 300              # 分辨率DPI
)

###############################################################################
library(ggplot2)

data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/relAESgroup.csv",header = TRUE,
                   sep = ",",na.strings = "na")
data$sig <- cut(data$pvalue, 
                breaks = c(0, 0.05, 1),
                labels = c("< 0.05", "> 0.05"),
                right = FALSE)

p1<-ggplot(data, aes (x = Freq)) +
  geom_line (aes(y = GP2mean,color = "with apathy (N=11)"), lwd = 1) +
  geom_ribbon(aes (ymin = GP2SD2, ymax = GP2SD1), alpha = .2,
              fill = "#639AC8", color = "transparent") +
  geom_line (aes(y = GP1mean,color = "without apathy (N=31)"), lwd = 1) +
  geom_ribbon(aes(ymin = GP1SD2, ymax = GP1SD1), alpha = .2,
              fill = "#B02418", color = "transparent") +
  scale_colour_manual(name = "Patient group",values = c("with apathy (N=11)" = "#3569AB","without apathy (N=31)" = "#B02418"))+
  labs (x = "Frequency(Hz)", y = "Rel.Pow)")+
  theme_classic()+
  theme(legend.position = "right", legend.title = element_blank(),
        legend.text = element_text(size = 14, family = "Helvetica"),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(family = "Helvetica",size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(family = "Helvetica",size = 14))+
  scale_x_continuous(expand = c(0,0),limits = c(0,90),breaks = c(0,30,60,90))+
  geom_rect(data = subset(data, sig == "< 0.05"),
            aes(xmin = Freq-0.9765625/2, xmax = Freq + 0.9765625/2, ymin = -Inf, ymax = Inf),
            fill = "grey30",alpha = 0.2) 


data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/absAESgroup.csv",header = TRUE,
                   sep = ",",na.strings = "na")
data$sig <- cut(data$pvalue, 
                breaks = c(0, 0.05, 1),
                labels = c("< 0.05", "> 0.05"),
                right = FALSE)

p2<-ggplot(data, aes (x = Freq)) +
  geom_line (aes(y = GP2mean,color = "with apathy (N=11)"), lwd = 1) +
  geom_ribbon(aes (ymin = GP2SD2, ymax = GP2SD1), alpha = .2,
              fill = "#639AC8", color = "transparent") +
  geom_line (aes(y = GP1mean,color = "without apathy (N=31)"), lwd = 1) +
  geom_ribbon(aes(ymin = GP1SD2, ymax = GP1SD1), alpha = .2,
              fill = "#B02418", color = "transparent") +
  scale_colour_manual(name = "Patient group",values = c("with apathy (N=11)" = "#3569AB","without apathy (N=31)" = "#B02418"))+
  labs (x = "Frequency(Hz)", y = "10*log10(Abs.Pow)")+
  theme_classic()+
  theme(legend.position = "none", legend.title = element_blank(),
        legend.text = element_text(size = 14, family = "Helvetica"),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(family = "Helvetica",size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(family = "Helvetica",size = 14))+
  scale_x_continuous(expand = c(0,0),limits = c(0,90),breaks = c(0,30,60,90))+
  geom_rect(data = subset(data, sig == "< 0.05"),
            aes(xmin = Freq-0.9765625/2, xmax = Freq + 0.9765625/2, ymin = -Inf, ymax = Inf),
            fill = "grey30",alpha = 0.2) 

data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/ApathyRelGroupBand.csv",header = TRUE,
                   sep = ",",na.strings = "na")
dat1<- as.matrix(data)
dat1<-as.data.frame(dat1)
dat1$pvalue<-as.numeric(dat1$Pval)
dat1$zval<-as.numeric(dat1$Zval)


dat1$pvalue1 <- cut(dat1$pvalue, 
                    breaks = c(0, 0.0071, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "Unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")
dat1$Variable <- factor(dat1$Class, levels = desired_order )

p3<-ggplot(dat1, aes(x = Variable, y = zval, fill = pvalue1))+
  scale_fill_manual (name="Pvalue",
                     values = c(
                       "#D6BEB9",
                       "grey80"))+
  geom_col()+
  theme_void()+
  theme(
    plot.margin = unit(c(0.3,0.3,0.3,0.3), 'cm'),
    plot.title = element_text(hjust = 0.5,size = 14, face = "bold"),
    axis.ticks.x = element_line(colour = "black", linewidth = 0.25),
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    axis.title.y =  element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 14, face = "plain", color = "black"),
    axis.text.y = element_blank(),
    legend.position = "none",
    legend.text = element_text(size = 14, family = "Helvetica"), legend.title = element_text(size = 14, family = "Helvetica"))+
  coord_cartesian(clip = 'off')


data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/ApathyAbsGroupBand.csv",header = TRUE,
                   sep = ",",na.strings = "na")
dat1<- as.matrix(data)
dat1<-as.data.frame(dat1)
dat1$pvalue<-as.numeric(dat1$Pval)
dat1$zval<-as.numeric(dat1$Zval)


dat1$pvalue1 <- cut(dat1$pvalue, 
                    breaks = c(0, 0.0071, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "Unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")
dat1$Variable <- factor(dat1$Class, levels = desired_order )

p4<-ggplot(dat1, aes(x = Variable, y = zval, fill = pvalue1))+
  scale_fill_manual (name="Pvalue",
                     values = c(
                       "#B2446B",
                       "#D6BEB9",
                       "grey80"))+
  geom_col()+
  theme_void()+
  theme(
    plot.margin = unit(c(0.3,0.3,0.3,0.3), 'cm'),
    plot.title = element_text(hjust = 0.5,size = 14, face = "bold"),
    axis.ticks.x = element_line(colour = "black", linewidth = 0.25),
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    axis.title.y =  element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 14, face = "plain", color = "black"),
    axis.text.y = element_blank(),
    legend.position = "right",
    legend.text = element_text(size = 14, family = "Helvetica"), legend.title = element_text(size = 14, family = "Helvetica"))+
  coord_cartesian(clip = 'off')

library(patchwork)
library(cowplot)
p2+p1+p3+p4+plot_layout(ncol = 2,nrow= 2,height = c(2,1))

ggsave(
  filename = "AESbandgroup1.png", # 保存的文件名称。通过后缀来决定生成什么格式的图片
  width = 12,             # 宽
  height = 4,            # 高
  units = "in",          # 单位
  dpi = 300              # 分辨率DPI
)

################################################################################
data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/relQUIPgroup.csv",header = TRUE,
                   sep = ",",na.strings = "na")
data$sig <- cut(data$pvalue, 
                breaks = c(0, 0.05, 1),
                labels = c("< 0.05", "> 0.05"),
                right = FALSE)

p1<-ggplot(data, aes (x = Freq)) +
  geom_ribbon(aes (ymin = GP2SD2, ymax = GP2SD1), alpha = .2,
              fill = "#639AC8", color = "transparent") +
  geom_line (aes(y = GP2mean,color = "with ICB (N=23)"), lwd = 1) +
  geom_ribbon(aes(ymin = GP1SD2, ymax = GP1SD1), alpha = .2,
              fill = "#B02418", color = "transparent") +
  geom_line (aes(y = GP1mean,color = "without ICB (N=24)"), lwd = 1) +
  scale_colour_manual(name = "Patient group",values = c("with ICB (N=23)" = "#3569AB","without ICB (N=24)" ="#B02418"))+
  labs (x = "Frequency(Hz)", y = "Rel.Pow)")+
  theme_classic()+
  theme(legend.position = "right", legend.title = element_blank(),
        legend.text = element_text(size = 14, family = "Helvetica"),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(family = "Helvetica",size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(family = "Helvetica",size = 14))+
  scale_x_continuous(expand = c(0,0),limits = c(0,90),breaks = c(0,30,60,90))+
  geom_rect(data = subset(data, sig == "< 0.05"),
            aes(xmin = Freq, xmax = Freq + 0.9765625, ymin = -Inf, ymax = Inf),
            fill = "grey30",alpha = 0.2) 

data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/absQUIPgroup.csv",header = TRUE,
                   sep = ",",na.strings = "na")
data$sig <- cut(data$pvalue, 
                breaks = c(0, 0.05, 1),
                labels = c("< 0.05", "> 0.05"),
                right = FALSE)

p2<-ggplot(data, aes (x = Freq)) +
  geom_ribbon(aes (ymin = GP2SD2, ymax = GP2SD1), alpha = .2,
              fill = "#639AC8", color = "transparent") +
  geom_line (aes(y = GP2mean,color = "with ICB (N=23)"), lwd = 1) +
  geom_ribbon(aes(ymin = GP1SD2, ymax = GP1SD1), alpha = .2,
              fill = "#B02418", color = "transparent") +
  geom_line (aes(y = GP1mean,color = "without ICB (N=24)"), lwd = 1) +
  scale_colour_manual(name = "Patient group",values = c("with ICB (N=23)" = "#3569AB","without ICB (N=24)" = "#B02418"))+
  labs (x = "Frequency(Hz)", y = "10*log10(Abs.Pow)")+
  theme_classic()+
  theme(legend.position = "none", legend.title = element_blank(),
        legend.text = element_text(size = 14, family = "Helvetica"),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(family = "Helvetica",size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(family = "Helvetica",size = 14))+
  scale_x_continuous(expand = c(0,0),limits = c(0,90),breaks = c(0,30,60,90))+
  geom_rect(data = subset(data, sig == "< 0.05"),
            aes(xmin = Freq, xmax = Freq + 0.9765625, ymin = -Inf, ymax = Inf),
            fill = "grey30",alpha = 0.2) 

data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/ICBRelGroupBand.csv",header = TRUE,
                   sep = ",",na.strings = "na")
dat1<- as.matrix(data)
dat1<-as.data.frame(dat1)
dat1$pvalue<-as.numeric(dat1$Pval)
dat1$zval<-as.numeric(dat1$Zval)


dat1$pvalue1 <- cut(dat1$pvalue, 
                    breaks = c(0, 0.0071, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "Unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")
dat1$Variable <- factor(dat1$Class, levels = desired_order )

p3<-ggplot(dat1, aes(x = Variable, y = zval, fill = pvalue1))+
  scale_fill_manual (name="Pvalue",
                     values = c(
                       "grey80"))+
  geom_col()+
  theme_void()+
  theme(
    plot.margin = unit(c(0.3,0.3,0.3,0.3), 'cm'),
    plot.title = element_text(hjust = 0.5,size = 14, face = "bold"),
    axis.ticks.x = element_line(colour = "black", linewidth = 0.25),
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    axis.title.y =  element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 14, face = "plain", color = "black"),
    axis.text.y = element_blank(),
    legend.position = "right",
    legend.text = element_text(size = 14, family = "Helvetica"), legend.title = element_text(size = 14, family = "Helvetica"))+
  coord_cartesian(clip = 'off')


data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/ICBAbsGroupBand.csv",header = TRUE,
                   sep = ",",na.strings = "na")
dat1<- as.matrix(data)
dat1<-as.data.frame(dat1)
dat1$pvalue<-as.numeric(dat1$Pval)
dat1$zval<-as.numeric(dat1$Zval)


dat1$pvalue1 <- cut(dat1$pvalue, 
                    breaks = c(0, 0.0071, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "Unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")
dat1$Variable <- factor(dat1$Class, levels = desired_order )

p4<-ggplot(dat1, aes(x = Variable, y = zval, fill = pvalue1))+
  scale_fill_manual (name="Pvalue",
                     values = c(
                       "grey80"))+
  geom_col()+
  theme_void()+
  theme(
    plot.margin = unit(c(0.3,0.3,0.3,0.3), 'cm'),
    plot.title = element_text(hjust = 0.5,size = 14, face = "bold"),
    axis.ticks.x = element_line(colour = "black", linewidth = 0.25),
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    axis.title.y =  element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 14, face = "plain", color = "black"),
    axis.text.y = element_blank(),
    legend.position = "none",
    legend.text = element_text(size = 14, family = "Helvetica"), legend.title = element_text(size = 14, family = "Helvetica"))+
  coord_cartesian(clip = 'off')

library(patchwork)
library(cowplot)
p2+p1+p4+p3+plot_layout(ncol = 2,nrow= 2,height = c(2,1))

ggsave(
  filename = "ICBbandgroup.png", # 保存的文件名称。通过后缀来决定生成什么格式的图片
  width = 12,             # 宽
  height = 4,            # 高
  units = "in",          # 单位
  dpi = 300              # 分辨率DPI
)