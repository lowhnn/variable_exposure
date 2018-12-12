library(tidyverse)

# Read dataset for O2 consumption
resp <- read_csv("1_OxygenConsumption.csv") %>%
  mutate(TreatCode = as_factor(TreatCode) %>%
           fct_relevel("Ambient", "Cyc6", "Cyc3", "Constant"),
         TimePoint = as_factor(TimePoint) %>%
           fct_relevel("T1","T2","T3","T4","T5","T6","T7","T8","T9")
  )

# Generate means and standard errors for plotting
se.respProp <- resp %>%
  filter(TimePoint != "T0") %>%
  group_by(TreatCode, TimePoint, ExpTime, plotGrp) %>%
  summarise(n = n(),
            MSProp_mean = mean(MSProp),
            sd = sd(MSProp),
            se = sd/sqrt(n)
  ) %>%
  ungroup()
  

# Plot relative respiration rates
gg.MSProp<-ggplot(se.respProp,aes(x=ExpTime,y=MSProp_mean,group=plotGrp,colour=TreatCode))+
  geom_point(size=2.5)+
  geom_line(size=1)+
  geom_errorbar(aes(ymin=MSProp_mean-se,ymax=MSProp_mean+se),width=0.1)+
  scale_x_continuous(expand = c(0,0), limits = c(-1.5,66))+
  scale_y_continuous(expand = c(0,0), limits = c(0.5,1.4))+
  scale_color_manual(values=c("black","gray50","gray25","gray75"))+
  facet_grid(TreatCode ~ .)+
  geom_hline(yintercept = 1,linetype="dashed")+
  xlab("Exposure time (h)")+
  ylab("Proportional respiration rate")+
  guides(colour=FALSE)+
  theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line.x = element_line(colour = "black", size = .7), axis.line.y = element_line(colour = "black", size = .7), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14),axis.text.x = element_text(size=14),axis.text.y = element_text(size=14))


# 1-sample t-tests
t.MSProp <- resp %>%
  filter(TimePoint != "T0") %>%
  group_by(TreatCode,TimePoint) %>%
  summarise(MSProp_mean = mean(MSProp),
            Tval = t.test(MSProp,mu=1)$statistic,
            Pval = t.test(MSProp,mu=1)$p.value,
            Different = Pval<0.05
  )



