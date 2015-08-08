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
reload.project()

######

######Munge and plot osama data
temp = australiansocialcohesion[,1:2]
temp[,2] = as.numeric(gsub("%", "", temp[,2]))/100
temp = melt(temp, "Years")
temp$variable = gsub("\\.", " ", temp$variable)
p = ggplot(temp, aes(x=Years, y = value, colour = variable)) + geom_line(stat="identity", size = 3)
p = sep.theme(p)
p = p + theme(legend.position = "none")
p = p + ylab("Experienced Racial, Ethnic or Religious Discrimination in the past year (% Yes)") + xlab("") 
p = p + ggtitle("Racial, Ethnic or Religious Discrimination Increasing in Australia")
p = p  + ylim(0, 0.25) + scale_y_continuous(labels = percent)
sep.png(p, "racialdiscrim")
