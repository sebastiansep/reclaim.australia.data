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
library(plotly)
reload.project()

######

######Munge and plot nonviolence data
nonviolence = melt(nonviolence, "Year")
nonviolence$variable = gsub("\\.", " ", nonviolence$variable)
p = ggplot(nonviolence, aes(x = Year, y = value, group = variable, colour = variable)) + geom_line(size =2)
p = p + theme_few()
p = p + ylab("Success Rate (%)") + xlab("Year") + ggtitle("Success Rates of Civil Resistance Campaigns")
#p = p + theme(legend.position="top", legend.title = element_blank(), legend.text=element_text(size=14)) 
ggplotly(p)
