library(scales)
library(nlme)
library(multcomp)
library(ggpubr)
library(tidyverse)

# For label wrapping in plots
wrap_10<-wrap_format(10)

##########################################################
# WET MASS
wmass <- read_csv("3_WetMass.csv") %>%
  mutate(TreatCode = as_factor(TreatCode) %>%
           fct_relevel("Ambient", "Cyc6", "Cyc3", "Constant")
  )

## Tests for ANOVA assumptions
shapiro.test(wmass$WM.initial)
bartlett.test(WM.initial~TreatCode,data = wmass)
shapiro.test(wmass$WMChange.endpt)
bartlett.test(WMChange.endpt~TreatCode,data = wmass)

## Nested ANOVA (Tank as random factor)
### check for differences in wet mass pre-treatment
lme.wm.initial<-lme(WM.initial~TreatCode,data=wmass,random=~1|TankID)
anova(lme.wm.initial)

### test for differences in wet mass growth post-treatment
lme.wmchange<-lme(WMChange.endpt~TreatCode*SpineAmp,data=wmass,random=~1|TankID)
anova(lme.wmchange)

## Generate means and standard errors for plotting
se.wmchange <- wmass %>%
  group_by(TreatCode) %>%
  summarise(n = n(),
            WMChange_mean = mean(WMChange.endpt),
            sd = sd(WMChange.endpt),
            se = sd/sqrt(n)
  ) %>%
  ungroup()

## Plot figure
gg.wmchange<-ggplot(se.wmchange,aes(x=TreatCode,y=WMChange_mean*100))+
  geom_bar(stat="identity",fill="gray25")+
  geom_errorbar(aes(ymin=(WMChange_mean*100)-(se*100),ymax=(WMChange_mean*100)+(se*100)),width=0.1)+
  scale_x_discrete(labels=wrap_10(c("Ambient","6-Hour Variable","3-Hour Variable","Constant Exposure")))+
  scale_y_continuous(expand = c(0,0), limits = c(0,25))+
  xlab("Dissolved Oxygen Treatment")+
  ylab("Wet mass growth (%)")+
  geom_segment(aes(x=.6,y=20,xend=4.4,yend=20), size=.55)+
  annotate("text", x = 0.6, y = 24.4, label = "A.", size = 6, fontface=2)+
  theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line.x = element_line(colour = "black", size = .7), axis.line.y = element_line(colour = "black", size = .7), axis.title.x = element_text(size=13), axis.title.y = element_text(size=13),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))



##########################################################
# TEST DIAMETER
diam <- read_csv("3_TestDiameter.csv") %>%
  mutate(TreatCode = as_factor(TreatCode) %>%
           fct_relevel("Ambient", "Cyc6", "Cyc3", "Constant")
  )

## Tests for ANOVA assumptions
shapiro.test(diam$Diam.initial)
bartlett.test(Diam.initial~TreatCode,data = diam)
shapiro.test(diam$DiamChange.endpt)
bartlett.test(DiamChange.endpt~TreatCode,data = diam)

## Nested ANOVA (Tank as random factor)
### check for differences in wet mass pre-treatment
lme.diam.initial<-lme(Diam.initial~TreatCode,data=diam,random=~1|TankID)
anova(lme.diam.initial)

### check for differences in diameter growth post-treatment
lme.diamchange<-lme(DiamChange.endpt~TreatCode,data=diam,random=~1|TankID)
anova(lme.diamchange)

##########################################################
# SPINE REGENERATION

spines <- read_csv("3_SpineLengths.csv") %>%
  mutate(TreatCode = as_factor(TreatCode) %>%
           fct_relevel("Ambient", "Cyc6", "Cyc3", "Constant"),
         TankID = as_factor(as.character(TankID))
  )

## Tests for ANOVA assumptions
shapiro.test(spines$MeanLength)
bartlett.test(MeanLength~TreatCode,data = spines)

## Nested ANOVA (Tank as random factor)
lme.spines<-lme(MeanLength~TreatCode,data=spines,random=~1|TankID)
anova(lme.spines)

## Generate means and standard errors for plotting
se.spines <- spines %>%
  group_by(TreatCode) %>%
  summarise(n = n(),
            SpineLength_mean = mean(MeanLength),
            sd = sd(MeanLength),
            se = sd/sqrt(n)
  ) %>%
  ungroup()

## Plot figure
gg.spines<-ggplot(se.spines,aes(x=TreatCode,y=SpineLength_mean*10))+
  geom_point(size=2.5)+
  geom_errorbar(aes(ymin=(SpineLength_mean*10)-(se*10),ymax=(SpineLength_mean*10)+(se*10)),width=.1,size=.65)+
  xlab("Dissolved Oxygen Treatment")+
  ylab("Spine length (mm)")+
  scale_x_discrete(labels=wrap_10(c("Ambient","6-Hour Variable","3-Hour Variable","Constant Exposure")))+
  scale_y_continuous(expand = c(0,0), limits = c(3,6.75))+
  geom_segment(aes(x=1.9,y=5,xend=4.1,yend=5), size=.55)+
  annotate("text", x = 1, y = 6.25, label = "A", size = 4.5)+
  annotate("text", x = 3, y = 5.25, label = "B", size = 4.5)+
  annotate("text", x = 0.6, y = 6.64, label = "B.", size = 6, fontface=2)+
  theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line.x = element_line(colour = "black", size = .7), axis.line.y = element_line(colour = "black", size = .7), axis.title.x = element_text(size=13), axis.title.y = element_text(size=13),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))

##########################################################
# BUOYANT MASS

bmass <- read_csv("3_BuoyantMass.csv") %>%
  mutate(TreatCode = as_factor(TreatCode) %>%
           fct_relevel("Ambient", "Cyc6", "Cyc3", "Constant")
  )

## Tests for ANOVA assumptions
shapiro.test(bmass$BM.initial)
bartlett.test(BM.initial~TreatCode,data = bmass)
shapiro.test(bmass$BMChange.midpt)
bartlett.test(BMChange.midpt~TreatCode,data = bmass)
shapiro.test(bmass$BMChange.endpt)
bartlett.test(BMChange.endpt~TreatCode,data = bmass)

## Nested ANOVA (Tank as random factor)
### check for differences in buoyant mass pre-treatment
lme.bm.initial<-lme(BM.initial~TreatCode,data=bmass,random=~1|TankID)
anova(lme.bm.initial)

### test for differences in buoyant mass growth mid-treatment
lme.bmchange.midpt<-lme(BMChange.midpt~TreatCode,data=bmass,random=~1|TankID)
anova(lme.bmchange.midpt)

### test for differences in buoyant mass growth post-treatment
lme.bmchange.endpt<-lme(BMChange.endpt~TreatCode*SpineAmp,data=bmass,random=~1|TankID)
anova(lme.bmchange.endpt)

## Generate means and standard errors for plotting
se.bmchange <- bmass %>%
  group_by(TreatCode) %>%
  summarise(n = n(),
            BMChange_mean = mean(BMChange.endpt),
            sd = sd(BMChange.endpt),
            se = sd/sqrt(n)
  ) %>%
  ungroup()

## Plot figure
gg.bmchange<-ggplot(se.bmchange,aes(x=TreatCode,y=100*BMChange_mean))+
  geom_bar(stat="identity",fill="gray25")+
  geom_errorbar(aes(ymin=(100*BMChange_mean)-(100*se),ymax=(100*BMChange_mean)+(100*se)),width=0.1)+
  xlab("Dissolved Oxygen Treatment")+
  ylab("Buoyant mass growth (%)")+
  scale_x_discrete(labels=wrap_10(c("Ambient","6-Hour Variable","3-Hour Variable","Constant Exposure")))+
  scale_y_continuous(expand = c(0,0), limits = c(0,17))+
  geom_segment(aes(x=1.6,y=12.75,xend=3.4,yend=12.75), size=.55)+
  annotate("text", x = 1, y = 14.25, label = "A", size = 4.5)+
  annotate("text", x = 4, y = 9.75, label = "B", size = 4.5)+
  annotate("text", x = 2.5, y = 13.5, label = "AB", size = 4.5)+
  annotate("text", x = 0.6, y = 16.5, label = "C.", size = 6, fontface=2)+
  theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line.x = element_line(colour = "black", size = .7), axis.line.y = element_line(colour = "black", size = .7), axis.title.x = element_text(size=13), axis.title.y = element_text(size=13),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))

##########################################################
# GONAD INDEX
gonads <- read_csv("3_GonadIndex.csv") %>%
  mutate(TreatCode = as_factor(TreatCode) %>%
           fct_relevel("Ambient", "Cyc6", "Cyc3", "Constant")
  )

## Tests for ANOVA assumptions
shapiro.test(gonads$GI.afdm)
bartlett.test(GI.afdm~TreatCode,data = gonads)

## Nested ANOVA (Tank as random factor)
### test for differences in gonad indices post-treatment
lme.gi.afdm<-lme(GI.afdm~TreatCode,data=gonads,random=~1|TankID)
anova(lme.gi.afdm)

### post-hoc test
summary(glht(lme.gonadindex.afdm, linfct=mcp(Treatment = "Tukey")))

## Generate means and standard errors for plotting
se.gi <- gonads %>%
  group_by(TreatCode) %>%
  summarise(n = n(),
            GI_mean = mean(GI.afdm),
            sd = sd(GI.afdm),
            se = sd/sqrt(n)
  ) %>%
  ungroup()

## Plot figure
gg.gi<-ggplot(se.gi,aes(x=TreatCode,y=GI_mean))+
  geom_point(size=2.5)+
  geom_errorbar(aes(ymin=GI_mean-se,ymax=GI_mean+se),width=0.1,size=.65)+
  xlab("Dissolved Oxygen Treatment")+
  ylab("Gonad Index")+
  scale_x_discrete(labels=wrap_10(c("Ambient","6-Hour Variable","3-Hour Variable","Constant Exposure")))+  
  scale_y_continuous(expand = c(0,0), limits = c(.38,.55))+
  annotate("text", x = 1, y = .515, label = "A", size = 4.5)+
  annotate("text", x = 4, y = .417, label = "C", size = 4.5)+
  annotate("text", x = 3, y = .45, label = "BC", size = 4.5)+
  annotate("text", x = 2, y = .485, label = "AB", size = 4.5)+
  annotate("text", x = 0.6, y = .5455, label = "D.", size = 6, fontface=2)+
  theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line.x = element_line(colour = "black", size = .7), axis.line.y = element_line(colour = "black", size = .7), axis.title.x = element_text(size=13), axis.title.y = element_text(size=13),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))


ggarrange(gg.wmchange,gg.spines,gg.bmchange,gg.gi,nrow=2,ncol=2,align="hv")

