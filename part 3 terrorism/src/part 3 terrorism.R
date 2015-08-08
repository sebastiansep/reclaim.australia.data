#############################################################################
#This project plots success of terrorism
#Written by Sepastian Sep 
#2 August 2015
#############################################################################

######Load libraries
library('ProjectTemplate')
library(ggplot2)
library(ggthemes)
library(grid)
library(sep)
library(treemap)
reload.project()

######

######Munge and plot osama data
osama$Prevalence = 100*osama$Prevalence/sum(osama$Prevalence)
osama$Message = factor(osama$Message, levels = osama$Message, ordered=T)
osama$end = cumsum(osama$Prevalence)
osama$start = c(0, osama$end[-length(osama$end)])
osama$id = 1:nrow(osama)
osama$Message = factor(osama$Message, levels = osama$Message, ordered=T)
p = ggplot(osama, aes(Message, fill = Message)) + geom_rect(aes(x = Message,
                                                                xmin = id - 0.45, xmax = id + 0.45, ymin = end,
                                                                ymax = start))
p = sep.theme(p)

p = p + ylab("Prevalence (%)") + xlab("") + ggtitle("Prevalence of Themes in Osama Bin Laden's Messages")
p = p + theme(axis.text.y = element_blank(), legend.position = "none")
p = p  + geom_text(data=osama, 
                   aes(Message, end-30, label=Message),
                   size=20, angle = 0)
p = p + coord_flip()
p = p + theme(axis.line.y = element_blank())
sep.png(p, "osamamessags")
######Terrorism Conflicts
terrorismwars$Conflict = factor(terrorismwars$Conflict, levels = terrorismwars$Conflict, ordered=T)
png("./graphs/warsonterror.png", width = 2560, height = 1920)
treemap(terrorismwars, index="Conflict", vSize = "Death.Toll", inflate.labels = TRUE, title = "")
dev.off()
