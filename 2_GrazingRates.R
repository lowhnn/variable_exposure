library(scales)
library(nlme)
library(multcomp)
library(tidyverse)

# For label wrapping in plots
wrap_10<-wrap_format(10)


#############################################
# SHORT-TERM GRAZING EXPERIMENT

# Read dataset for short term grazing experiment
stgraz <- read_csv("2_ShortTermGrazing.csv") %>%
  mutate(TreatCode = as_factor(TreatCode) %>%
           fct_relevel("Ambient", "Cyc6", "Cyc3", "Constant")
  )

# Mass-specific grazing rates
## Tests for ANOVA assumptions
shapiro.test(stgraz$MSRate)
bartlett.test(MSRate~TreatCode,data = stgraz)

## ANOVA
lme.msgraz<-lme(MSRate~TreatCode,data=stgraz,random=~1|ExptID)
anova(lme.msgraz)

## Post-hoc tests
summary(glht(lme.msgraz, linfct=mcp(TreatCode = "Tukey")))

## Generate means and standard errors for plotting
se.stgraz <- stgraz %>%
  group_by(TreatCode) %>%
  summarise(n = n(),
            MSRate_mean = mean(MSRate),
            sd = sd(MSRate),
            se = sd/sqrt(n)
  ) %>%
  ungroup()

## Plot short-term mass-specific grazing rates
gg.stgraz<-ggplot(data=se.stgraz,aes(x=TreatCode,y=MSRate_mean))+
  geom_bar(stat="identity",fill="gray25")+
  geom_errorbar(aes(ymin=MSRate_mean-se,ymax=MSRate_mean+se),width=0.1)+
  scale_x_discrete(labels=wrap_10(c("Ambient","6-Hour Variable","3-Hour Variable","Constant Exposure")))+
  scale_y_continuous(expand = c(0,0), limits = c(0,2.25))+
  scale_fill_manual(values=c("black","gray50","gray25","gray75"))+
  xlab("Dissolved Oxygen Treatment")+
  ylab("Mass-specific grazing rate (g/g/day)")+
  geom_segment(aes(x=1.6,y=1.75,xend=3.4,yend=1.75), size=.55)+
  annotate("text", x = 1, y = 1.95, label = "A", size = 4.5)+
  annotate("text", x = 4, y = 1.225, label = "B", size = 4.5)+
  annotate("text", x = 2.5, y = 1.85, label = "AB", size = 4.5)+
  theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line.x = element_line(colour = "black", size = .7), axis.line.y = element_line(colour = "black", size = .7), axis.title.x = element_text(size=13), axis.title.y = element_text(size=13),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))         

# Per-capita grazing rates (not plotted)
## Tests for ANOVA assumptions
shapiro.test(stgraz$GrazingRate)
bartlett.test(GrazingRate~TreatCode,data = stgraz)

## ANOVA
lme.pcgraz<-lme(GrazingRate~TreatCode,data=stgraz,random=~1|ExptID)
anova(lme.pcgraz)

## Post-hoc tests
summary(glht(lme.pcgraz, linfct=mcp(TreatCode = "Tukey")))


#############################################
# LONG-TERM CUMULATIVE GRAZING 

# Read dataset for 9-week grazing
ltgraz<-read_csv("2_LongTermCumulativeGrazing.csv") %>%
  mutate(TreatCode = as_factor(TreatCode) %>%
           fct_relevel("Ambient", "Cyc6", "Cyc3", "Constant"),
  )

# Cumulative grazing at end of experiment
finalgraz<-ltgraz %>%
  filter(Date == "2017-09-05")

# Tests for ANOVA assumptions (log transform)
shapiro.test(log(finalgraz$CumAdjGraz))
bartlett.test(log(CumAdjGraz)~TreatCode,data = finalgraz)

# ANOVA
lme.finalgraz<-lme(log(CumAdjGraz)~TreatCode,data=finalgraz,random=~1|TankID)
anova(lme.finalgraz)

# Post-hoc tests
summary(glht(lme.finalgraz, linfct=mcp(TreatCode = "Tukey")))

# Generate means and standard errors for plotting
se.ltgraz <- ltgraz %>%
  group_by(Date,ExposureDays,TreatCode) %>%
  summarise(n = n(),
            CumAdjGraz_mean = mean(CumAdjGraz),
            sd = sd(CumAdjGraz),
            se = sd/sqrt(n)
  ) %>%
  ungroup()

# Plot cumulative grazing over time
gg.ltgraz<-ggplot(se.ltgraz,aes(x=ExposureDays,y=CumAdjGraz_mean,group=TreatCode,colour=TreatCode))+
  geom_point(position = position_dodge(.1))+
  geom_line(size=.8)+
  geom_errorbar(aes(ymin=CumAdjGraz_mean-se,ymax=CumAdjGraz_mean+se),width=0.5,position = position_dodge(.1))+
  scale_color_manual(values=c("black","gray50","gray25","gray75"),labels=c("Ambient", "6-Hour Variable Exposure", "3-Hour Variable Exposure","Constant Exposure"),name="Dissolved Oxygen Treatment")+  
  xlab("Experiment days")+
  ylab("Cumulative grazing (g)")+
  theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line.x = element_line(colour = "black", size = .7), axis.line.y = element_line(colour = "black", size = .7), axis.title.x = element_text(size=13), axis.title.y = element_text(size=13),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))  

