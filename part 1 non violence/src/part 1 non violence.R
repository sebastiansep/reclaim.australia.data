#############################################################################
#This project plots success of nonviolence
#Written by Sepastian Sep 
#2 August 2015
#############################################################################

######Load libraries
library('ProjectTemplate')
library(ggplot2)
library(ggthemes)
library(scales)
library(sep)
library(reshape2)
reload.project()

######

######Munge and plot nonviolence data
nonviolence = melt(nonviolence, "Year")
nonviolence$variable = gsub("\\.", " ", nonviolence$variable)
p = ggplot(nonviolence, aes(x = Year, y = value, group = variable, colour = variable)) + geom_line(size =3)
p = p + theme_sep()
p = p + ylab("Success Rate (%)") + xlab("Year") + ggtitle("Success Rates of Civil Resistance Campaigns\n")
p
sep.png(p, "non-violent-success-rate")



