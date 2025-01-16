library(corrplot)
library(Hmisc)
library(ggplot2)
library(gghalves)
library(RColorBrewer)
library(ggprism)
library(hrbrthemes)
library(ggpubr)
library(ggsignif)

## LFP correlations
############################################################################Depression
library(ggplot2)
data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/DepLFPR.csv",header = TRUE,
                   sep = ",",na.strings = "na")

dat1<- as.matrix(data[1:14,])
dat1<-as.data.frame(dat1)
dat1$pvalue<-as.numeric(dat1$Pval)
dat1$cor<-as.numeric(dat1$Corr)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")

dat1$pvalue1 <- cut(dat1$pvalue, 
                    breaks = c(0, 0.007, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
dat1$Variable <- factor(dat1$Variable, levels = desired_order)

p1<-ggplot(dat1,aes(x = Variable,y = Class))+
  geom_point(aes(size = -log10(pvalue),fill = cor, color = pvalue1,stroke = 1),
             shape = 21)+
  scale_fill_gradient2(name = "Spearman's r",
                       limit = c(-0.651,0.651),
                       breaks = c(-0.65,0,0.65),
                       low ="#142F61",
                       mid = "white",
                       high = "#8A240F",
                       midpoint = 0)+
  scale_size_continuous(name = "-Log10 Pvalue",
                        limit = c(-0.001,3.1),
                        breaks = c(0,1,2,3))+
  scale_color_manual(values = c("grey50","white"))+
  theme(axis.text.x = element_text(angle = 45, size = 12),
       legend.position ="none",axis.title.x = element_text(size = 12,face = "bold"),
       # axis.ticks.y = element_blank(),axis.text.y = element_blank())+
       axis.text.y = element_text(size = 10),axis.title.y = element_text(size = 12))+
  xlab("Depression")+
  ylab(NULL)+
  #ylab("Abs. power")+ 
  coord_fixed(ratio = 1/1)

#############################
dat2<- as.matrix(data[15:28,])
dat2<-as.data.frame(dat2)
dat2$pvalue<-as.numeric(dat2$Pval)
dat2$cor<-as.numeric(dat2$Corr)
dat2$pvalue1 <- cut(dat2$pvalue, 
                    breaks = c(0, 0.007, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")
dat2$Variable <- factor(dat2$Variable, levels = desired_order)
p2<-ggplot(dat2,aes(x = Variable,y = Class))+
  geom_point(aes(size = -log10(pvalue),fill = cor,color = pvalue1,stroke = 1),
             shape = 21)+
  scale_fill_gradient2(name = "Spearman's r",
                       limit = c(-0.651,0.651),
                       breaks = c(-0.65,0,0.65),
                       low ="#142F61",
                       mid = "white",
                       high = "#8A240F",
                       midpoint = 0)+
  scale_size_continuous(name = "-Log10 pvalue",
                        limit = c(-0.001,3.1),
                        breaks = c(0,1,2,3))+
  scale_color_manual(values = c("grey50","white"))+
  theme(legend.position = "none",axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        # axis.ticks.y = element_blank(),axis.text.y = element_blank())+
        axis.text.y = element_text(size = 10),axis.title.y = element_text(size = 12))+
  xlab(NULL)+
  ylab(NULL)+
  #ylab("Rel. power")+ 
  coord_fixed(ratio = 1/1)

##################################
dat3<- as.matrix(data[29:42,])
dat3<-as.data.frame(dat3)
dat3$pvalue<-as.numeric(dat3$Pval)
dat3$cor<-as.numeric(dat3$Corr)
dat3$pvalue1 <- cut(dat3$pvalue, 
                    breaks = c(0, 0.007, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")
dat3$Variable <- factor(dat3$Variable, levels = desired_order)
p3<-ggplot(dat3,aes(x = Variable,y = Class))+
  geom_point(aes(size = -log10(pvalue),fill = cor,color = pvalue1,stroke = 1),
             shape = 21)+
  scale_fill_gradient2(name = "Spearman's r",
                       limit = c(-0.651,0.651),
                       breaks = c(-0.65,0,0.65),
                       low ="#142F61",
                       mid = "white",
                       high = "#8A240F",
                       midpoint = 0)+
  scale_size_continuous(name = "-Log10 pvalue",
                        limit = c(-0.001,3.1),
                        breaks = c(0,1,2,3))+
  scale_color_manual(values = c("white"))+
  theme_bw()+
  theme(legend.position = "none",axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        # axis.ticks.y = element_blank(),axis.text.y = element_blank())+
        axis.text.y = element_text(size = 10),axis.title.y = element_text(size = 12))+
  xlab(NULL)+
  ylab(NULL)+
 # ylab("Abs.power")+ 
  coord_fixed(ratio = 1/1)

##############################
dat4<- as.matrix(data[43:56,])
dat4<-as.data.frame(dat4)
dat4$pvalue<-as.numeric(dat4$Pval)
dat4$cor<-as.numeric(dat4$Corr)
dat4$pvalue1 <- cut(dat4$pvalue, 
                    breaks = c(0, 0.007, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")
dat4$Variable <- factor(dat4$Variable, levels = desired_order)
library(ggplot2)
p4<-ggplot(dat4,aes(x = Variable,y = Class))+
  geom_point(aes(size = -log10(pvalue),fill = cor,color = pvalue1,stroke = 1),
             shape = 21)+
  scale_fill_gradient2(name = "Spearman's r",
                       limit = c(-0.651,0.651),
                       breaks = c(-0.65,0,0.65),
                       low ="#142F61",
                       mid = "white",
                       high = "#8A240F",
                       midpoint = 0)+
  scale_size_continuous(name = "-Log10 Pvalue",
                        limit = c(-0.001,3.1),
                        breaks = c(0,1,2,3))+
  scale_color_manual(values = c("white"), guide = F)+
  theme_bw()+
  theme(legend.title = element_text(size = 10),legend.position = "none",
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), 
        # axis.ticks.y = element_blank(),axis.text.y = element_blank())+
        axis.text.y = element_text(size = 10),axis.title.y = element_text(size = 12))+
  xlab(NULL)+
  ylab(NULL)+
 # ylab("Rel. power")+ 
  coord_fixed(ratio = 1/1)

#############################
dat5<- as.matrix(data[57:70,])
dat5<-as.data.frame(dat5)
dat5$pvalue<-as.numeric(dat5$Pval)
dat5$cor<-as.numeric(dat5$Corr)
dat5$pvalue1 <- cut(dat5$pvalue, 
                    breaks = c(0, 0.007, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")
dat5$Variable <- factor(dat5$Variable, levels = desired_order)
p21<-ggplot(dat5,aes(x = Variable,y = Class))+
  geom_point(aes(size = -log10(pvalue),fill = cor,color = pvalue1,stroke = 1),
             shape = 21)+
  scale_fill_gradient2(name = "Spearman's r",
                       limit = c(-0.651,0.651),
                       breaks = c(-0.65,0,0.65),
                       low ="#142F61",
                       mid = "white",
                       high = "#8A240F",
                       midpoint = 0)+
  scale_size_continuous(name = "-Log10 pvalue",
                        limit = c(-0.001,3.1),
                        breaks = c(0,1,2,3))+
  scale_color_manual(values = c("black","white"))+
  theme(legend.position = "none",axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        # axis.ticks.y = element_blank(),axis.text.y = element_blank())+
        axis.text.y = element_text(size = 10),axis.title.y = element_text(size = 12))+
  xlab(NULL)+
  ylab(NULL)+
  #ylab("Rel. power")+ 
  coord_fixed(ratio = 1/1)


################################################################################Anxiety
library(ggplot2)
data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/AnxietyLFPR.csv",header = TRUE,
                   sep = ",",na.strings = "na")

dat1<- as.matrix(data[1:14,])
dat1<-as.data.frame(dat1)
dat1$pvalue<-as.numeric(dat1$Pval)
dat1$cor<-as.numeric(dat1$Corr)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")

dat1$pvalue1 <- cut(dat1$pvalue, 
                    breaks = c(0, 0.007, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
dat1$Variable <- factor(dat1$Variable, levels = desired_order)

p5<-ggplot(dat1,aes(x = Variable,y = Class))+
  geom_point(aes(size = -log10(pvalue),fill = cor, color = pvalue1,stroke = 1),
             shape = 21)+
  scale_fill_gradient2(name = "Spearman's r",
                       limit = c(-0.651,0.651),
                       breaks = c(-0.65,0,0.65),
                       low ="#142F61",
                       mid = "white",
                       high = "#8A240F",
                       midpoint = 0)+
  scale_size_continuous(name = "-Log10 Pvalue",
                        limit = c(-0.001,3.1),
                        breaks = c(0,1,2,3))+
  scale_color_manual(values = c("grey50","white"))+
  theme(axis.text.x = element_text(angle = 45, size = 12),axis.title.x = element_text(size = 12,face = "bold"),legend.position ="none",
        # axis.ticks.y = element_blank(),axis.text.y = element_blank())+
        axis.text.y = element_text(size = 10),axis.title.y = element_text(size = 12))+
  xlab("Anxiety")+
 # ylab(NULL)+
  ylab("Abs.Pow")+ 
  coord_fixed(ratio = 1/1)

#############################
dat2<- as.matrix(data[15:28,])
dat2<-as.data.frame(dat2)
dat2$pvalue<-as.numeric(dat2$Pval)
dat2$cor<-as.numeric(dat2$Corr)
dat2$pvalue1 <- cut(dat2$pvalue, 
                    breaks = c(0, 0.007, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")
dat2$Variable <- factor(dat2$Variable, levels = desired_order)
p6<-ggplot(dat2,aes(x = Variable,y = Class))+
  geom_point(aes(size = -log10(pvalue),fill = cor,color = pvalue1,stroke = 1),
             shape = 21)+
  scale_fill_gradient2(name = "Spearman's r",
                       limit = c(-0.651,0.651),
                       breaks = c(-0.65,0,0.65),
                       low ="#142F61",
                       mid = "white",
                       high = "#8A240F",
                       midpoint = 0)+
  scale_size_continuous(name = "-Log10 pvalue",
                        limit = c(-0.001,3.1),
                        breaks = c(0,1,2,3))+
  scale_color_manual(values =c("white"))+
  theme(legend.position = "none",axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        # axis.ticks.y = element_blank(),axis.text.y = element_blank())+
        axis.text.y = element_text(size = 10),axis.title.y = element_text(size = 12))+
  xlab(NULL)+
  #  ylab(NULL)+
  ylab("Rel.Pow")+ 
  coord_fixed(ratio = 1/1)

##################################
dat3<- as.matrix(data[29:42,])
dat3<-as.data.frame(dat3)
dat3$pvalue<-as.numeric(dat3$Pval)
dat3$cor<-as.numeric(dat3$Corr)
dat3$pvalue1 <- cut(dat3$pvalue, 
                    breaks = c(0, 0.007, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")
dat3$Variable <- factor(dat3$Variable, levels = desired_order)
p7<-ggplot(dat3,aes(x = Variable,y = Class))+
  geom_point(aes(size = -log10(pvalue),fill = cor,color = pvalue1,stroke = 1),
             shape = 21)+
  scale_fill_gradient2(name = "Spearman's r",
                       limit = c(-0.651,0.651),
                       breaks = c(-0.65,0,0.65),
                       low ="#142F61",
                       mid = "white",
                       high = "#8A240F",
                       midpoint = 0)+
  scale_size_continuous(name = "-Log10 pvalue",
                        limit = c(-0.001,3.1),
                        breaks = c(0,1,2,3))+
  scale_color_manual(values = c("grey50","white"))+
  theme_bw()+
  theme(legend.position = "none",axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        # axis.ticks.y = element_blank(),axis.text.y = element_blank())+
        axis.text.y = element_text(size = 10),axis.title.y = element_text(size = 12))+
  xlab(NULL)+
#  ylab(NULL)+
  ylab("Abs.Pow")+ 
  coord_fixed(ratio = 1/1)

##############################
dat4<- as.matrix(data[43:56,])
dat4<-as.data.frame(dat4)
dat4$pvalue<-as.numeric(dat4$Pval)
dat4$cor<-as.numeric(dat4$Corr)
dat4$pvalue1 <- cut(dat4$pvalue, 
                    breaks = c(0, 0.007, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")
dat4$Variable <- factor(dat4$Variable, levels = desired_order)
library(ggplot2)
p8<-ggplot(dat4,aes(x = Variable,y = Class))+
  geom_point(aes(size = -log10(pvalue),fill = cor,color = pvalue1,stroke = 1),
             shape = 21)+
  scale_fill_gradient2(name = "Spearman's r",
                       limit = c(-0.651,0.651),
                       breaks = c(-0.65,0,0.65),
                       low ="#142F61",
                       mid = "white",
                       high = "#8A240F",
                       midpoint = 0,guide = F)+
  scale_size_continuous(name = "-Log10 Pvalue",
                        limit = c(-0.001,3.1),
                        breaks = c(0,1,2,3), guide = F)+
  scale_color_manual(values = c("white"), guide = F)+
  theme_bw()+
  theme(legend.title = element_text(size = 10),legend.position = "top",
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), 
        # axis.ticks.y = element_blank(),axis.text.y = element_blank())+
        axis.text.y = element_text(size = 10),axis.title.y = element_text(size = 12))+
  xlab(NULL)+
  #  ylab(NULL)+
  ylab("Rel.Pow")+ 
  coord_fixed(ratio = 1/1)

#############################
dat5<- as.matrix(data[57:70,])
dat5<-as.data.frame(dat5)
dat5$pvalue<-as.numeric(dat5$Pval)
dat5$cor<-as.numeric(dat5$Corr)
dat5$pvalue1 <- cut(dat5$pvalue, 
                    breaks = c(0, 0.007, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")
dat5$Variable <- factor(dat5$Variable, levels = desired_order)
p22<-ggplot(dat5,aes(x = Variable,y = Class))+
  geom_point(aes(size = -log10(pvalue),fill = cor,color = pvalue1,stroke = 1),
             shape = 21)+
  scale_fill_gradient2(name = "Spearman's r",
                       limit = c(-0.651,0.651),
                       breaks = c(-0.65,0,0.65),
                       low ="#142F61",
                       mid = "white",
                       high = "#8A240F",
                       midpoint = 0)+
  scale_size_continuous(name = "-Log10 pvalue",
                        limit = c(-0.001,3.1),
                        breaks = c(0,1,2,3))+
  scale_color_manual(values = c("grey50","white"))+
  theme(legend.position = "none",axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        # axis.ticks.y = element_blank(),axis.text.y = element_blank())+
        axis.text.y = element_text(size = 10),axis.title.y = element_text(size = 12))+
  xlab(NULL)+
  ylab(NULL)+
  ylab("SampEn")+ 
  coord_fixed(ratio = 1/1)

###############################################################################apathy
## LFP correlations
library(ggplot2)
data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/ApathyLFPR.csv",header = TRUE,
                   sep = ",",na.strings = "na")

dat1<- as.matrix(data[1:14,])
dat1<-as.data.frame(dat1)
dat1$pvalue<-as.numeric(dat1$Pval)
dat1$cor<-as.numeric(dat1$Corr)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")

dat1$pvalue1 <- cut(dat1$pvalue, 
                    breaks = c(0, 0.007, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
dat1$Variable <- factor(dat1$Variable, levels = desired_order)

p9<-ggplot(dat1,aes(x = Variable,y = Class))+
  geom_point(aes(size = -log10(pvalue),fill = cor, color = pvalue1,stroke = 1),
             shape = 21)+
  scale_fill_gradient2(name = "Spearman's r",
                       limit = c(-0.651,0.651),
                       breaks = c(-0.65,0,0.65),
                       low ="#142F61",
                       mid = "white",
                       high = "#8A240F",
                       midpoint = 0)+
  scale_size_continuous(name = "-Log10 Pvalue",
                        limit = c(-0.001,3.1),
                        breaks = c(0,1,2,3))+
  scale_color_manual(values = c("black","grey50","white"), guide = F)+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, size = 12),axis.title.x = element_text(size = 12,face = "bold"), 
        # axis.ticks.y = element_blank(),axis.text.y = element_blank())+
        axis.text.y = element_text(size = 10),axis.title.y = element_text(size = 12))+
  xlab("Apathy")+
  ylab(NULL)+
 #ylab("Abs.Pow")+ 
  coord_fixed(ratio = 1/1)

#############################
dat2<- as.matrix(data[15:28,])
dat2<-as.data.frame(dat2)
dat2$pvalue<-as.numeric(dat2$Pval)
dat2$cor<-as.numeric(dat2$Corr)
dat2$pvalue1 <- cut(dat2$pvalue, 
                    breaks = c(0, 0.007, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")
dat2$Variable <- factor(dat2$Variable, levels = desired_order)
p10<-ggplot(dat2,aes(x = Variable,y = Class))+
  geom_point(aes(size = -log10(pvalue),fill = cor,color = pvalue1,stroke = 1),
             shape = 21)+
  scale_fill_gradient2(name = "Spearman's r",
                       limit = c(-0.651,0.651),
                       breaks = c(-0.65,0,0.65),
                       low ="#142F61",
                       mid = "white",
                       high = "#8A240F",
                       midpoint = 0)+
  scale_size_continuous(name = "-Log10 pvalue",
                        limit = c(-0.001,3.1),
                        breaks = c(0,1,2,3))+
  scale_color_manual(values = c("black","grey50","white"),guide = F)+
  theme(legend.position = "none",axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        # axis.ticks.y = element_blank(),axis.text.y = element_blank())+
        axis.text.y = element_text(size = 10),axis.title.y = element_text(size = 12))+
  xlab(NULL)+
 ylab(NULL)+
  #ylab("Rel.Pow")+ 
  coord_fixed(ratio = 1/1)

##################################
dat3<- as.matrix(data[29:42,])
dat3<-as.data.frame(dat3)
dat3$pvalue<-as.numeric(dat3$Pval)
dat3$cor<-as.numeric(dat3$Corr)
dat3$pvalue1 <- cut(dat3$pvalue, 
                    breaks = c(0, 0.007, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")
dat3$Variable <- factor(dat3$Variable, levels = desired_order)
p11<-ggplot(dat3,aes(x = Variable,y = Class))+
  geom_point(aes(size = -log10(pvalue),fill = cor,color = pvalue1,stroke = 1),
             shape = 21)+
  scale_fill_gradient2(name = "Spearman's r",
                       limit = c(-0.651,0.651),
                       breaks = c(-0.65,0,0.65),
                       low ="#142F61",
                       mid = "white",
                       high = "#8A240F",
                       midpoint = 0)+
  scale_size_continuous(name = "-Log10 pvalue",
                        limit = c(-0.001,3.1),
                        breaks = c(0,1,2,3))+
  scale_color_manual(values = c("black","grey50","white"))+
  theme_bw()+
  theme(legend.position = "none",axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        # axis.ticks.y = element_blank(),axis.text.y = element_blank())+
        axis.text.y = element_text(size = 10),axis.title.y = element_text(size = 12))+
  xlab(NULL)+
  ylab(NULL)+
 # ylab("Abs.Pow")+ 
  coord_fixed(ratio = 1/1)

##############################
dat4<- as.matrix(data[43:56,])
dat4<-as.data.frame(dat4)
dat4$pvalue<-as.numeric(dat4$Pval)
dat4$cor<-as.numeric(dat4$Corr)
dat4$pvalue1 <- cut(dat4$pvalue, 
                    breaks = c(0, 0.007, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")
dat4$Variable <- factor(dat4$Variable, levels = desired_order)
library(ggplot2)
p12<-ggplot(dat4,aes(x = Variable,y = Class))+
  geom_point(aes(size = -log10(pvalue),fill = cor,color = pvalue1,stroke = 1),
             shape = 21)+
  scale_fill_gradient2(name = "Spearman's r",
                       limit = c(-0.651,0.651),
                       breaks = c(-0.65,0,0.65),
                       low ="#142F61",
                       mid = "white",
                       high = "#8A240F",
                       midpoint = 0, guide = F)+
  scale_size_continuous(name = "-Log10 Pvalue",
                        limit = c(-0.001,3.1),
                        breaks = c(0,1,2,3), guide = F)+
  scale_color_manual(values = c("black","grey50","white"), guide = F)+
  theme_bw()+
  theme(legend.title = element_text(size = 10),legend.position = "none",
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), 
       # axis.ticks.y = element_blank(),axis.text.y = element_blank())+
        axis.text.y = element_text(size = 10),axis.title.y = element_text(size = 12))+
  xlab(NULL)+
  ylab(NULL)+
 # ylab("Rel.Pow")+ 
  coord_fixed(ratio = 1/1)

#############################
dat5<- as.matrix(data[57:70,])
dat5<-as.data.frame(dat5)
dat5$pvalue<-as.numeric(dat5$Pval)
dat5$cor<-as.numeric(dat5$Corr)
dat5$pvalue1 <- cut(dat5$pvalue, 
                    breaks = c(0, 0.007, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")
dat5$Variable <- factor(dat5$Variable, levels = desired_order)
p23<-ggplot(dat5,aes(x = Variable,y = Class))+
  geom_point(aes(size = -log10(pvalue),fill = cor,color = pvalue1,stroke = 1),
             shape = 21)+
  scale_fill_gradient2(name = "Spearman's r",
                       limit = c(-0.651,0.651),
                       breaks = c(-0.65,0,0.65),
                       low ="#142F61",
                       mid = "white",
                       high = "#8A240F",
                       midpoint = 0)+
  scale_size_continuous(name = "-Log10 pvalue",
                        limit = c(-0.001,3.1),
                        breaks = c(0,1,2,3))+
  scale_color_manual(values = c("black","grey50","white"))+
  theme(legend.position = "none",axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        # axis.ticks.y = element_blank(),axis.text.y = element_blank())+
        axis.text.y = element_text(size = 10),axis.title.y = element_text(size = 12))+
  xlab(NULL)+
  ylab(NULL)+
  #ylab("Rel. power")+ 
  coord_fixed(ratio = 1/1)

##############################################################################Impulisivity
## LFP correlations
library(ggplot2)
data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/UPPSLFPR.csv",header = TRUE,
                   sep = ",",na.strings = "na")

dat1<- as.matrix(data[1:14,])
dat1<-as.data.frame(dat1)
dat1$pvalue<-as.numeric(dat1$Pval)
dat1$cor<-as.numeric(dat1$Corr)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")

dat1$pvalue1 <- cut(dat1$pvalue, 
                    breaks = c(0, 0.007, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
dat1$Variable <- factor(dat1$Variable, levels = desired_order)

p13<-ggplot(dat1,aes(x = Variable,y = Class))+
  geom_point(aes(size = -log10(pvalue),fill = cor, color = pvalue1,stroke = 1),
             shape = 21)+
  scale_fill_gradient2(name = "Spearman's r",
                       limit = c(-0.651,0.651),
                       breaks = c(-0.65,0,0.65),
                       low ="#142F61",
                       mid = "white",
                       high = "#8A240F",
                       midpoint = 0)+
  scale_size_continuous(name = "-Log10 Pvalue",
                        limit = c(-0.001,3.1),
                        breaks = c(0,1,2,3))+
  scale_color_manual(values = c("grey50","white"),guide = F)+
  theme(axis.text.x = element_text(angle = 45, size = 12),
      legend.position ="none",axis.title.x = element_text(size = 12,face= "bold"),
      # axis.ticks.y = element_blank(),axis.text.y = element_blank())+
      axis.text.y = element_text(size = 10),axis.title.y = element_text(size = 12))+
  xlab("Impulsivity")+
  ylab(NULL)+
  # ylab("Rel. power")+ 
  coord_fixed(ratio = 1/1)

#############################
dat2<- as.matrix(data[15:28,])
dat2<-as.data.frame(dat2)
dat2$pvalue<-as.numeric(dat2$Pval)
dat2$cor<-as.numeric(dat2$Corr)
dat2$pvalue1 <- cut(dat2$pvalue, 
                    breaks = c(0, 0.007, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")
dat2$Variable <- factor(dat2$Variable, levels = desired_order)
p14<-ggplot(dat2,aes(x = Variable,y = Class))+
  geom_point(aes(size = -log10(pvalue),fill = cor,color = pvalue1,stroke = 1),
             shape = 21)+
  scale_fill_gradient2(name = "Spearman's r",
                       limit = c(-0.651,0.651),
                       breaks = c(-0.65,0,0.65),
                       low ="#142F61",
                       mid = "white",
                       high = "#8A240F",
                       midpoint = 0)+
  scale_size_continuous(name = "-Log10 pvalue",
                        limit = c(-0.001,3.1),
                        breaks = c(0,1,2,3))+
  scale_color_manual(values = c("grey50","white"),guide = F)+
  theme(legend.position = "none",axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        # axis.ticks.y = element_blank(),axis.text.y = element_blank())+
        axis.text.y = element_text(size = 10),axis.title.y = element_text(size = 12))+
  xlab(NULL)+
  ylab(NULL)+
  # ylab("Rel. power")+ 
  coord_fixed(ratio = 1/1)

##################################
dat3<- as.matrix(data[29:42,])
dat3<-as.data.frame(dat3)
dat3$pvalue<-as.numeric(dat3$Pval)
dat3$cor<-as.numeric(dat3$Corr)
dat3$pvalue1 <- cut(dat3$pvalue, 
                    breaks = c(0, 0.007, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")
dat3$Variable <- factor(dat3$Variable, levels = desired_order)
p15<-ggplot(dat3,aes(x = Variable,y = Class))+
  geom_point(aes(size = -log10(pvalue),fill = cor,color = pvalue1,stroke = 1),
             shape = 21)+
  scale_fill_gradient2(name = "Spearman's r",
                       limit = c(-0.651,0.651),
                       breaks = c(-0.65,0,0.65),
                       low ="#142F61",
                       mid = "white",
                       high = "#8A240F",
                       midpoint = 0)+
  scale_size_continuous(name = "-Log10 pvalue",
                        limit = c(-0.001,3.1),
                        breaks = c(0,1,2,3))+
  scale_color_manual(values = c("grey50","white"),guide = F)+
  theme_bw()+
  theme(legend.position = "none",axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        # axis.ticks.y = element_blank(),axis.text.y = element_blank())+
        axis.text.y = element_text(size = 10),axis.title.y = element_text(size = 12))+
  xlab(NULL)+
  ylab(NULL)+
  # ylab("Rel. power")+ 
  coord_fixed(ratio = 1/1)

##############################
dat4<- as.matrix(data[43:56,])
dat4<-as.data.frame(dat4)
dat4$pvalue<-as.numeric(dat4$Pval)
dat4$cor<-as.numeric(dat4$Corr)
dat4$pvalue1 <- cut(dat4$pvalue, 
                    breaks = c(0, 0.007, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")
dat4$Variable <- factor(dat4$Variable, levels = desired_order)
library(ggplot2)
p16<-ggplot(dat4,aes(x = Variable,y = Class))+
  geom_point(aes(size = -log10(pvalue),fill = cor,color = pvalue1,stroke = 1),
             shape = 21)+
  scale_fill_gradient2(name = "Spearman's r",
                       limit = c(-0.651,0.651),
                       breaks = c(-0.65,0,0.65),
                       low ="#142F61",
                       mid = "white",
                       high = "#8A240F",
                       midpoint = 0)+
  scale_size_continuous(name = "-Log10 Pvalue",
                        limit = c(-0.001,3.1),
                        breaks = c(0,1,2,3))+
  scale_color_manual(values = c("grey50","white"),guide = F)+
  theme_bw()+
  theme(legend.title = element_text(size = 10),legend.position = "none",
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), 
        # axis.ticks.y = element_blank(),axis.text.y = element_blank())+
        axis.text.y = element_text(size = 10),axis.title.y = element_text(size = 12))+
  xlab(NULL)+
  ylab(NULL)+
  # ylab("Rel. power")+ 
  coord_fixed(ratio = 1/1)

#############################
dat5<- as.matrix(data[57:70,])
dat5<-as.data.frame(dat5)
dat5$pvalue<-as.numeric(dat5$Pval)
dat5$cor<-as.numeric(dat5$Corr)
dat5$pvalue1 <- cut(dat5$pvalue, 
                    breaks = c(0, 0.007, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")
dat5$Variable <- factor(dat5$Variable, levels = desired_order)
p24<-ggplot(dat5,aes(x = Variable,y = Class))+
  geom_point(aes(size = -log10(pvalue),fill = cor,color = pvalue1,stroke = 1),
             shape = 21)+
  scale_fill_gradient2(name = "Spearman's r",
                       limit = c(-0.651,0.651),
                       breaks = c(-0.65,0,0.65),
                       low ="#142F61",
                       mid = "white",
                       high = "#8A240F",
                       midpoint = 0)+
  scale_size_continuous(name = "-Log10 pvalue",
                        limit = c(-0.001,3.1),
                        breaks = c(0,1,2,3))+
  scale_color_manual(values = c("grey50","white"))+
  theme(legend.position = "none",axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        # axis.ticks.y = element_blank(),axis.text.y = element_blank())+
        axis.text.y = element_text(size = 10),axis.title.y = element_text(size = 12))+
  xlab(NULL)+
  ylab(NULL)+
  #ylab("Rel. power")+ 
  coord_fixed(ratio = 1/1)

## ############################################################ OCD
## LFP correlations
library(ggplot2)
data <- read.table("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Demographics/OCDLFPR.csv",header = TRUE,
                   sep = ",",na.strings = "na")

dat1<- as.matrix(data[1:14,])
dat1<-as.data.frame(dat1)
dat1$pvalue<-as.numeric(dat1$Pval)
dat1$cor<-as.numeric(dat1$Corr)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")

dat1$pvalue1 <- cut(dat1$pvalue, 
                    breaks = c(0, 0.007, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
dat1$Variable <- factor(dat1$Variable, levels = desired_order)

p17<-ggplot(dat1,aes(x = Variable,y = Class))+
  geom_point(aes(size = -log10(pvalue),fill = cor, color = pvalue1,stroke = 1),
             shape = 21)+
  scale_fill_gradient2(name = "Spearman's r",
                       limit = c(-0.651,0.651),
                       breaks = c(-0.65,0,0.65),
                       low ="#142F61",
                       mid = "white",
                       high = "#8A240F",
                       midpoint = 0)+
  scale_size_continuous(name = "-Log10 Pvalue",
                        limit = c(-0.001,3.1),
                        breaks = c(0,1,2,3))+
  scale_color_manual(values =c("grey50","white"),guide = F)+
  theme(axis.text.x = element_text(angle = 45, size = 12),
       legend.position ="none",axis.title.x= element_text(size = 12,face = "bold"),
       # axis.ticks.y = element_blank(),axis.text.y = element_blank())+
       axis.text.y = element_text(size = 10),axis.title.y = element_text(size = 12))+
  xlab("Compulsivity")+
  #ylab(NULL)+
   ylab("Abs.Pow")+ 
  coord_fixed(ratio = 1/1)

#############################
dat2<- as.matrix(data[15:28,])
dat2<-as.data.frame(dat2)
dat2$pvalue<-as.numeric(dat2$Pval)
dat2$cor<-as.numeric(dat2$Corr)
dat2$pvalue1 <- cut(dat2$pvalue, 
                    breaks = c(0, 0.007, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")
dat2$Variable <- factor(dat2$Variable, levels = desired_order)
p18<-ggplot(dat2,aes(x = Variable,y = Class))+
  geom_point(aes(size = -log10(pvalue),fill = cor,color = pvalue1,stroke = 1),
             shape = 21)+
  scale_fill_gradient2(name = "Spearman's r",
                       limit = c(-0.651,0.651),
                       breaks = c(-0.65,0,0.65),
                       low ="#142F61",
                       mid = "white",
                       high = "#8A240F",
                       midpoint = 0)+
  scale_size_continuous(name = "-Log10 pvalue",
                        limit = c(-0.001,3.1),
                        breaks = c(0,1,2,3))+
  scale_color_manual(values = c("grey50","white"),guide = F)+
  theme(legend.position = "none",axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        # axis.ticks.y = element_blank(),axis.text.y = element_blank())+
        axis.text.y = element_text(size = 10),axis.title.y = element_text(size = 12))+
  xlab(NULL)+
 # ylab(NULL)+
  ylab("Rel.Pow")+ 
  coord_fixed(ratio = 1/1)

##################################
dat3<- as.matrix(data[29:42,])
dat3<-as.data.frame(dat3)
dat3$pvalue<-as.numeric(dat3$Pval)
dat3$cor<-as.numeric(dat3$Corr)
dat3$pvalue1 <- cut(dat3$pvalue, 
                    breaks = c(0, 0.007, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")
dat3$Variable <- factor(dat3$Variable, levels = desired_order)
p19<-ggplot(dat3,aes(x = Variable,y = Class))+
  geom_point(aes(size = -log10(pvalue),fill = cor,color = pvalue1,stroke = 1),
             shape = 21)+
  scale_fill_gradient2(name = "Spearman's r",
                       limit = c(-0.651,0.651),
                       breaks = c(-0.65,0,0.65),
                       low ="#142F61",
                       mid = "white",
                       high = "#8A240F",
                       midpoint = 0)+
  scale_size_continuous(name = "-Log10 pvalue",
                        limit = c(-0.001,3.1),
                        breaks = c(0,1,2,3))+
  scale_color_manual(values = c("grey50","white"),guide = F)+
  theme_bw()+
  theme(legend.position = "none",axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        # axis.ticks.y = element_blank(),axis.text.y = element_blank())+
        axis.text.y = element_text(size = 10),axis.title.y = element_text(size = 12))+
  xlab(NULL)+
# ylab(NULL)+
   ylab("Abs.Pow")+ 
  coord_fixed(ratio = 1/1)

##############################
dat4<- as.matrix(data[43:56,])
dat4<-as.data.frame(dat4)
dat4$pvalue<-as.numeric(dat4$Pval)
dat4$cor<-as.numeric(dat4$Corr)
dat4$pvalue1 <- cut(dat4$pvalue, 
                    breaks = c(0, 0.007, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")
dat4$Variable <- factor(dat4$Variable, levels = desired_order)
library(ggplot2)
p20<-ggplot(dat4,aes(x = Variable,y = Class))+
  geom_point(aes(size = -log10(pvalue),fill = cor,color = pvalue1,stroke = 1),
             shape = 21)+
  scale_fill_gradient2(name = "Spearman's r",
                       limit = c(-0.651,0.651),
                       breaks = c(-0.65,0,0.65),
                       low ="#142F61",
                       mid = "white",
                       high = "#8A240F",
                       midpoint = 0)+
  scale_size_continuous(name = "-Log10 Pvalue",
                        limit = c(-0.001,3.1),
                        breaks = c(0,1,2,3))+
  scale_color_manual(values = c("grey50","white"),guide = F)+
  theme_bw()+
  theme(legend.title = element_text(size = 10),legend.position = "none",
        axis.ticks.x = element_blank(),axis.text.x = element_blank(),
        # axis.ticks.y = element_blank(),axis.text.y = element_blank())+
        axis.text.y = element_text(size = 10),axis.title.y = element_text(size = 12))+
  xlab(NULL)+
#  ylab(NULL)+
  ylab("Rel.Pow")+ 
  coord_fixed(ratio = 1/1)
#############################
dat5<- as.matrix(data[57:70,])
dat5<-as.data.frame(dat5)
dat5$pvalue<-as.numeric(dat5$Pval)
dat5$cor<-as.numeric(dat5$Corr)
dat5$pvalue1 <- cut(dat5$pvalue, 
                    breaks = c(0, 0.007, 0.05, 1),
                    labels = c("Bonf.adjusted < 0.05", "unadjusted < 0.05", "> 0.05"),
                    right = FALSE)
desired_order = c("δ","θ", "α","lβ","hβ","lγ","hγ")
dat5$Variable <- factor(dat5$Variable, levels = desired_order)
p25<-ggplot(dat5,aes(x = Variable,y = Class))+
  geom_point(aes(size = -log10(pvalue),fill = cor,color = pvalue1,stroke = 1),
             shape = 21)+
  scale_fill_gradient2(name = "Spearman's r",
                       limit = c(-0.651,0.651),
                       breaks = c(-0.65,0,0.65),
                       low ="#142F61",
                       mid = "white",
                       high = "#8A240F",
                       midpoint = 0)+
  scale_size_continuous(name = "-Log10 pvalue",
                        limit = c(-0.001,3.1),
                        breaks = c(0,1,2,3))+
  scale_color_manual(values = c("grey50","white"))+
  theme(legend.position = "none",axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        # axis.ticks.y = element_blank(),axis.text.y = element_blank())+
        axis.text.y = element_text(size = 10),axis.title.y = element_text(size = 12))+
  xlab(NULL)+
  ylab(NULL)+
  ylab("SampEn")+ 
  coord_fixed(ratio = 1/1)


library(patchwork)
library(cowplot)
p8+p4+p12+p7+p3+p11+p22+p21+p23+p6+p2+p10+p5+p1+p9+plot_layout(ncol =3,nrow = 5)

ggsave(
  filename = "CorrelationalMap1.png", # 保存的文件名称。通过后缀来决定生成什么格式的图片
  width = 10,             # 宽
  height = 5,            # 高
  units = "in",          # 单位
  dpi = 300              # 分辨率DPI
)

library(patchwork)
library(cowplot)
p20+p16+p19+p15+p25+p24+p18+p14+p17+p13+plot_layout(ncol=2,nrow = 5)

ggsave(
  filename = "CorrelationalMap2.png", # 保存的文件名称。通过后缀来决定生成什么格式的图片
  width = 7,             # 宽
  height = 5,            # 高
  units = "in",          # 单位
  dpi = 300              # 分辨率DPI
)

################################################################################
# Load required library

# Set the file path and name for the output image with 300 DPI and 10:6 aspect ratio
png("spectrum_plot_300dpi_10_6.png", width = 3000, height = 1200, res = 300)

library(stats)  # For smooth.spline

# Define frequency range
freq <- seq(1, 90, by = 0.1)  # Frequencies from 1 to 90 Hz

# Generate baseline power (1/f trend)
set.seed(123)  # For reproducibility
base_power <- 1 / freq  # Gradual decrease with frequency
random_variation <- rnorm(length(freq), mean = 0, sd = 0.01)  # Small random variations
raw_power <- base_power + random_variation  # Add variability

# Add characteristic peaks to mimic brain rhythms
add_peak <- function(freq, peak_freq, amplitude, width) {
  amplitude * exp(-((freq - peak_freq)^2) / (2 * width^2))  # Gaussian function
}

# Peaks at typical EEG frequency bands
raw_power <- raw_power +
  add_peak(freq, 3, 0.3, 1) +   # Delta peak (~4 Hz)
  add_peak(freq, 7, 0.4, 1.5) + # Theta peak (~8 Hz)
  add_peak(freq, 11, 0.5, 1.5) +# Alpha peak (~12 Hz)
  add_peak(freq, 18, 0.3, 2) +  # Beta peak (~20 Hz)
  add_peak(freq, 50, 0.2, 3)    # Gamma peak (~40 Hz)

# Ensure non-negative power values
raw_power <- pmax(raw_power, 0)

# Smooth the power spectrum
smooth_power <- smooth.spline(freq, raw_power, spar = 0.7)$y  # Cubic spline smoothing

# Plot the spectrum
plot(
  freq, smooth_power,
  type = "l", col = "#5CAAD7", lwd = 4,
  xlab = "Frequency (Hz)", ylab = "Power",
  ylim = c(0, 1),
  cex.lab = 1.2       # Enlarge axis labels
)

bands <- list(
  Delta = c(1, 4)
)


# Add square brackets for each band
for (band in names(bands)) {
  x_start <- bands[[band]][1]
  x_end <- bands[[band]][2]
  
  # Calculate the mean power across all bands
  global_max_power <- mean(smooth_power[x_start:x_end])
  
  # Position the brackets slightly above the global maximum power
  y_pos_global <- global_max_power * 1.15  # 30% above the global maximum
  
  # Horizontal line for the bracket
  segments(x_start, y_pos_global, x_end, y_pos_global, col = "grey30", lwd = 3)
  
  # Vertical lines for the ends of the bracket
  segments(x_start, y_pos_global, x_start, y_pos_global - 0.05, col = "grey30", lwd = 3)
  segments(x_end, y_pos_global, x_end, y_pos_global - 0.05, col = "grey30", lwd = 3)
}


bands <- list(
  Alpha = c(8, 12)
)


# Add square brackets for each band
for (band in names(bands)) {
  x_start <- bands[[band]][1]
  x_end <- bands[[band]][2]
  
  # Calculate the mean power across all bands
  global_max_power <- mean(smooth_power[x_start:x_end])
  
  # Position the brackets slightly above the global maximum power
  y_pos_global <- global_max_power * 1.1  # 30% above the global maximum
  
  # Horizontal line for the bracket
  segments(x_start, y_pos_global, x_end, y_pos_global, col = "grey30", lwd = 3)
  
  # Vertical lines for the ends of the bracket
  segments(x_start, y_pos_global, x_start, y_pos_global - 0.05, col = "grey30", lwd = 3)
  segments(x_end, y_pos_global, x_end, y_pos_global - 0.05, col = "grey30", lwd = 3)
}

bands <- list(
  HighBeta = c(13, 20)
)


# Add square brackets for each band
for (band in names(bands)) {
  x_start <- bands[[band]][1]
  x_end <- bands[[band]][2]
  
  # Calculate the mean power across all bands
  global_max_power <- mean(smooth_power[x_start:x_end])
  
  # Position the brackets slightly above the global maximum power
  y_pos_global <- global_max_power * 0.85  # 30% above the global maximum
  
  # Horizontal line for the bracket
  segments(x_start, y_pos_global, x_end, y_pos_global, col = "grey30", lwd = 3)
  
  # Vertical lines for the ends of the bracket
  segments(x_start, y_pos_global, x_start, y_pos_global - 0.05, col = "grey30", lwd = 3)
  segments(x_end, y_pos_global, x_end, y_pos_global - 0.05, col = "grey30", lwd = 3)
}

bands <- list(
  HighBeta = c(21, 35)
)


# Add square brackets for each band
for (band in names(bands)) {
  x_start <- bands[[band]][1]
  x_end <- bands[[band]][2]
  
  # Calculate the mean power across all bands
  global_max_power <- mean(smooth_power[x_start:x_end])
  
  # Position the brackets slightly above the global maximum power
  y_pos_global <- global_max_power * 0.85  # 30% above the global maximum
  
  # Horizontal line for the bracket
  segments(x_start, y_pos_global, x_end, y_pos_global, col = "grey30", lwd = 3)
  
  # Vertical lines for the ends of the bracket
  segments(x_start, y_pos_global, x_start, y_pos_global - 0.05, col = "grey30", lwd = 3)
  segments(x_end, y_pos_global, x_end, y_pos_global - 0.05, col = "grey30", lwd = 3)
}
bands <- list(
  LowGamma = c(40, 60)
)


# Add square brackets for each band
for (band in names(bands)) {
  x_start <- bands[[band]][1]
  x_end <- bands[[band]][2]
  
  # Calculate the mean power across all bands
  global_max_power <- mean(smooth_power[x_start:x_end])
  
  # Position the brackets slightly above the global maximum power
  y_pos_global <- global_max_power * 0.6  # 30% above the global maximum
  
  # Horizontal line for the bracket
  segments(x_start, y_pos_global, x_end, y_pos_global, col = "grey30", lwd = 3)
  
  # Vertical lines for the ends of the bracket
  segments(x_start, y_pos_global, x_start, y_pos_global - 0.05, col = "grey30", lwd = 3)
  segments(x_end, y_pos_global, x_end, y_pos_global - 0.05, col = "grey30", lwd = 3)
}
# Add gridlines for clarity
grid()


# Close the PNG device to save the image
dev.off()


#############################################################################
# Set the file path and name for the output image with 300 DPI and 15:6 aspect ratio
png("spectrum_plot_300dpi_15_6_shaded_theta.png", width = 1000, height = 1000, res = 300, bg = "transparent")

# Load required library
library(stats)  # For smooth.spline

# Define frequency range
freq <- seq(1, 90, by = 0.1)  # Frequencies from 1 to 90 Hz

# Generate baseline power (1/f trend)
set.seed(123)  # For reproducibility
base_power <- 1 / freq  # Gradual decrease with frequency
random_variation <- rnorm(length(freq), mean = 0, sd = 0.01)  # Small random variations
raw_power <- base_power + random_variation  # Add variability

# Add characteristic peaks to mimic brain rhythms
add_peak <- function(freq, peak_freq, amplitude, width) {
  amplitude * exp(-((freq - peak_freq)^2) / (2 * width^2))  # Gaussian function
}

# Peaks at typical EEG frequency bands
raw_power <- raw_power +
  add_peak(freq, 3, 0.3, 1) +   # Delta peak (~4 Hz)
  add_peak(freq, 7, 0.4, 1.5) + # Theta peak (~8 Hz)
  add_peak(freq, 13, 0.5, 1.5) +# Alpha peak (~12 Hz)
  add_peak(freq, 15, 0.3, 2) +  # Beta peak (~20 Hz)
  add_peak(freq, 50, 0.2, 3)    # Gamma peak (~40 Hz)

# Ensure non-negative power values and that power starts at 0
raw_power <- pmax(raw_power, 0)

# Smooth the power spectrum
smooth_power <- smooth.spline(freq, raw_power, spar = 0.7)$y  # Cubic spline smoothing

# Plot the spectrum with xlim from 0 to 90 Hz and ylim from 0 to 0.8
plot(
  freq, smooth_power,
  type = "l", col = "#BB1E38", lwd = 3,  # Increase line width (lwd = 4 for thicker lines)
  xlab = "Frequency (Hz)", ylab = "Power",
  xlim = c(0, 90),  # Set x-axis limits from 0 to 90 Hz
  ylim = c(0, 0.8),   # Set y-axis limits from 0 to 0.8
  cex.axis = 1,     # Enlarge axis numbers
  cex.lab = 1,      # Enlarge axis labels
)

# Define frequency bands
bands <- list(
 Theta = c(4, 8),
 LowBeta = c(13,35)
)

# Add shaded bands
for (band in names(bands)) {
  rect(
    xleft = bands[[band]][1],
    xright = bands[[band]][2],
    ybottom = 0,
    ytop = max(smooth_power),
    col = rgb(0.4, 0.4, 0.4, 0.3),  # Light gray with transparency
    border = NA
  )
}


# Define frequency bands with their boundaries
bands <- list(
  LowGamma = c(40, 60)
)

# Add square brackets for each band
for (band in names(bands)) {
  x_start <- bands[[band]][1]
  x_end <- bands[[band]][2]
  
  # Find the corresponding power values
  y_start <- smooth_power[which.min(abs(freq - x_start))]
  y_end <- smooth_power[which.min(abs(freq - x_end))]
  
  # Position the bracket slightly above the curve
  y_pos <- max(y_start, y_end) * 10  # 10% above the curve
  
  # Horizontal line for the bracket
  segments(x_start, y_pos, x_end, y_pos, col = "grey30", lwd = 3)
  
  # Vertical lines for the ends of the bracket
  segments(x_start, y_pos, x_start, y_pos - 0.05, col = "grey30", lwd = 3)
  segments(x_end, y_pos, x_end, y_pos - 0.05, col = "grey30", lwd = 3)
}

# Add gridlines for clarity
grid()

# Close the PNG device to save the image
dev.off()