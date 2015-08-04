#############################################################################
#This project plots success of terrorism
#Written by Sepastian Sep 
#2 August 2015
#############################################################################

######Load libraries
library('ProjectTemplate')
library(ggplot2)
library(ggthemes)
library(plotly)
reload.project()

######

######Munge and plot osama data
osama$Prevalence = 100*osama$Prevalence/sum(osama$Prevalence)
osama$Message = factor(osama$Message, levels = osama$Message, ordered=T)
p = ggplot(osama, aes(x = Message, y = Prevalence, fill = Message)) + geom_bar(stat="identity")
p = p + theme_few()
p = p + ylab("Prevalence (%)") + xlab("") + ggtitle("Prevalence of Themes in Osama Bin Laden's Messages")
p = p + theme(axis.text.x = element_blank())
p = p + theme(legend.position="top", legend.title = element_blank(), legend.text=element_text(size=14)) 
ggplotly(p)

######Terrorism Conflicts
terrorismwars$Conflict = factor(terrorismwars$Conflict, levels = terrorismwars$Conflict, ordered=T)
p = ggplot(terrorismwars, aes(x = Conflict, y = Death.Toll, fill = Conflict)) + geom_bar(stat="identity")
p = p + theme_few()
p = p + ylab("Death Toll") + xlab("") + ggtitle("Death Toll's in Relevant Conflicts")
p = p + theme(legend.position="none") 
ggplotly(p)

#waterfall chart cant be published to plotly
# osama$end = cumsum(osama$Prevalence)
# osama$start = c(0, osama$end[-length(osama$end)])
# osama$id = 1:nrow(osama)
# osama$Message = factor(osama$Message, levels = osama$Message, ordered=T)
# p = ggplot(osama, aes(Message, fill = Message)) + geom_rect(aes(x = Message,
#                                                                 xmin = id - 0.45, xmax = id + 0.45, ymin = end,
#                                                                 ymax = start))
# p = p + theme_few()
# p = p + ylab("Prevalence (%)") + xlab("") + ggtitle("Prevalence of Themes in Osama Bin Laden's Messages")
# p = p + theme(axis.text.x = element_blank())