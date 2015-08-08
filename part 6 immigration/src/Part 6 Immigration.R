#############################################################################
#This project calculates stats on population in Australia 2014
#Written by Sepastian Sep 
#2 August 2015
#############################################################################

######Load libraries
library('ProjectTemplate')
reload.project()
library(wpp2012)
library(countrycode)
library(smss)
library(googleVis)
library(ggthemes)
library(reshape2)
library(scales)
library(dplyr)
######


######Plot Austalian Population Growth
austpopln = austpopln[1:2,]
austpopln[,1] = as.character(austpopln[,1])
austpopln[2,1] = "Residents Born in Australia"
austpopln[2,1] = "Residents Born Oversears"
austpopln[,-1] = apply(austpopln[,-1],2, function(x) 100*x/sum(x))
austpopln = melt(austpopln, "Country")
austpopln$year = as.numeric(gsub("X", "", austpopln$variable))
austpopln = austpopln[complete.cases(austpopln),]
p = ggplot(austpopln, aes(x=year, y = value, colour = Country)) +  geom_line(size = 2)
p = p  + scale_fill_manual(values = alpha(c("blue", "red"), .3))
p = p + theme_few()
p = p + theme(legend.position="top", legend.title = element_blank())
p = p + ggtitle("Composition of Australian Population since 1992")
p = p + xlab("") + ylab("Percentage of Population") + ylim(0,100)
jpeg("./graphs/austpop.jpg")
print(p)
dev.off()
######

######Plot Austalian Immigration
countryimmigration = melt(countryimmigration, "country")
countryimmigration$year = as.numeric(gsub("X", "", countryimmigration$variable))
countryimmigration = countryimmigration[complete.cases(countryimmigration),]
temp = subset(countryimmigration, year == 2014)
temp = arrange(temp, desc(value))
countryimmigration$country = factor(countryimmigration$country, levels = temp$country, ordered = T)
p = ggplot(countryimmigration, aes(x=year, y = value, fill = country)) +  geom_line(aes(colour= country), size = 2)
p = p + theme_few()
p = p + theme(legend.position="top", legend.title = element_blank()) 
p = p + ggtitle("Sources of Immigration into Australia since 1992")
p = p + xlab("") + ylab("Total Number of Immigrants")
jpeg("./graphs/austimmigrationsources.jpg")
print(p)
dev.off()
######

