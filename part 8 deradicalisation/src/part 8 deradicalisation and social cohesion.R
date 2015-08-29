#############################################################################
#This project plots Australian Social Cohesion
#Written by Sepastian Sep 
#2 August 2015
#############################################################################

######Load libraries
library('ProjectTemplate')
library(ggplot2)
library(sep)
library(scales)
library(gridExtra)
reload.project()

######

######Munge and plot osama data
temp = australiansocialcohesion[,1:2]
temp[,2] = as.numeric(gsub("%", "", temp[,2]))/100
temp = melt(temp, "Years")
temp$variable = gsub("\\.", " ", temp$variable)
p = ggplot(temp, aes(x=Years, y = value, colour = variable)) + geom_line(stat="identity", size = 3)
p = p + theme_sep()
p = p + theme(legend.position = "none", axis.text.x = element_text(size = 20),  axis.text.y = element_text(size = 15))
p = p + ylab("Experienced Racial, Ethnic or Religiou\nDiscrimination in the past year (% Yes)") + xlab("") 
p = p + ggtitle("Racial, Ethnic or Religious Discrimination Increasing in Australia")
p = p  + ylim(0, 0.25) + scale_y_continuous(labels = percent)
p
sep.png(p, "racial-disc")

######Social Cohesion Plots

cohesion = 1.5
d <- data.frame(ETIME = rnorm(sd=cohesion, n=10000))
d$N <- 1:nrow(d)
d$PROB <- dnorm(d$ETIME, sd=cohesion)
d$cohesion = "Lesser Social Cohesion"

# Bell curve

p = ggplot(d, aes(ETIME, PROB)) + geom_line() + 
  theme_few() + scale_y_continuous(limits = c(0, 0.45)) + 
  scale_x_continuous(limits = c(-6, 6)) + 
  geom_area(aes(x=ifelse(ETIME <= -2, ETIME, NA)), fill="red", alpha=0.5) +
  geom_area(aes(x=ifelse(ETIME >= 2 , ETIME, NA)), fill="red", alpha=0.5) +
  ylab("Population Density") + xlab("") + ggtitle("Figure 1: Lower Social Cohesion") + 
  geom_vline(xintercept=mean(d$ETIME), color="red", alpha=0.5, size=0.5) +
  annotate("text", -4, 0.05, label = "Active Extremists", angle = 45) + 
  annotate("text", 4, 0.05, label = "Re-active Extremists", angle = -45) + 
  annotate("text", 0, 1.05*max(d$PROB), label = "Social Acceptance") 
p = p + theme_sep()


cohesion = 1
d <- data.frame(ETIME = rnorm(sd=cohesion, n=10000))
d$N <- 1:nrow(d)
d$PROB <- dnorm(d$ETIME, sd=cohesion)
d$cohesion = "Greater Social Cohesion"

# Bell curve
p2 = ggplot(d, aes(ETIME, PROB)) + geom_line() + 
  theme_few() + scale_y_continuous(limits = c(0, 0.45)) + 
  scale_x_continuous(limits = c(-6, 6)) + 
  geom_area(aes(x=ifelse(ETIME <= -2, ETIME, NA)), fill="red", alpha=0.5) +
  geom_area(aes(x=ifelse(ETIME >= 2 , ETIME, NA)), fill="red", alpha=0.5) +
  ylab("Population Density") + xlab("") + ggtitle("Figure 2: Greater Social Cohesion") + 
  geom_vline(xintercept=mean(d$ETIME), color="red", alpha=0.5, size=0.5) +
  annotate("text", -4, 0.05, label = "Active Extremists", angle = 45) + 
  annotate("text", 4, 0.05, label = "Re-active Extremists", angle = -45) + 
  annotate("text", 0, 1.05*max(d$PROB), label = "Social Acceptance") 
p2 = p2 + theme_sep()
grid.arrange(p,p2,ncol=2)
