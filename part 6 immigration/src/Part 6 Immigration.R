#############################################################################
#This project calculates stats on asylum seekers in Australia 2014
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
ggplotly(p)
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
ggplotly(p)
######

######Population Growth
asylumarrivals = asylumarrivals[,1:4] 
asylumarrivals = melt(asylumarrivals, "Year")
asylumarrivals$variable = gsub("\\.", " ", asylumarrivals$variable)
asylumarrivals$value = gsub("\\,", "", asylumarrivals$value)
asylumarrivals$value = as.numeric(asylumarrivals$value)
asylumarrivals$Year = as.numeric(substr(asylumarrivals$Year, 6,10))
asylumarrivals$variable = gsub("Births", "Australian Births", asylumarrivals$variable)
asylumarrivals$variable = gsub("Asylum Seeker Boat Arrivals", "Asylum Seekers", asylumarrivals$variable)
asylumarrivals$variable = factor(asylumarrivals$variable, levels = c("Australian Births", "New Migrants", "Asylum Seekers"), ordered = T)
p = ggplot(asylumarrivals, aes(x=Year, y = value, colour = variable)) +  geom_line(position = 'stack', size =2)
p = p + theme_few()
p = p + theme(legend.position="top", legend.title = element_blank()) 
p = p + ggtitle("Sources of Population Growth in Australia since 2010")
p = p + xlab("") + scale_y_continuous(name="Number of New People", labels = comma)
p = p  + scale_fill_manual(values = alpha(c("blue", "orange", "brown"), .3))
ggplotly(p)
######

###### Plot australian budget
temp = budget[1:8,3:4]
temp = arrange(temp, desc(Expenditure))
temp$Sector = factor(temp$Sector, levels = temp$Sector, ordered=T)
p = ggplot(temp, aes(x = Sector, y=Expenditure, fill = Sector)) + geom_bar(stat = "identity", position = "stack")
p = p + theme(legend.position="none", legend.title = element_blank()) 
p = p + ggtitle("Australian Budgetary Expenditure - 2015")
p = p + xlab("") + ylab("Australian Dollars (Billion)")
p= p + theme_few()
ggplotly(p)

###### 

###### Plot cost per day asylum seekers
temp = budget[11:12,1:2]
names(temp) = c("Method", "Cost")
temp$Method = factor(temp$Method, levels = temp$Method, ordered=T)
p = ggplot(temp, aes(x = Method, y=Cost, fill = Method)) + geom_bar(stat = "identity", position = "stack")
p = p + theme(legend.position="none", legend.title = element_blank()) 
p = p + ggtitle("UNHCR Estimated Costs Per Day of Different Asylum Seeker Policy")
p = p + xlab("") + ylab("US Dollars per Day")
p= p + theme_few()
ggplotly(p)

###### 

######Munge asylum data
asylum$destination = gsub("Tibetan", "China", asylum$destination)
asylum$origin = gsub("Tibetan", "China", asylum$origin)
asylum$destination = gsub("CuraÃ§ao", "Curacao", asylum$destination) 
asylum$origin = gsub("CuraÃ§ao", "Curacao", asylum$origin)
######

###### Match country codes
asylum$destiso = countrycode(asylum$destination, origin = "country.name", destination = "iso3c")
asylum$origin.iso = countrycode(asylum$origin, origin = "country.name", destination = "iso3c")
asylum$origin.iso[is.na(asylum$origin.iso)] = "UNK"
asylum$Value = as.numeric(gsub("\\*", "", asylum$Value))
asylum = asylum[complete.cases(asylum),]
###### 

###### Clean up world population data
data(popprojHigh)
pop = popprojHigh[-(1:9),c("name", "2015")]
pop$iso = countrycode(pop$name, origin = "country.name", destination = "iso3c")
###### 

###### Calculate asylum seekers and refugees per 10000 of OECD countries
asylum.dest = asylum %>% group_by(destiso, destination) %>% summarise(total = sum(Value))
asylum.dest$pop = match(asylum.dest$destiso, pop$iso)
asylum.dest$pop = 1000*pop$`2015`[asylum.dest$pop]
asylum.dest$per10000 = 10000*with(asylum.dest, total/pop) 
data("oecd.data")
oecd.data$iso = countrycode(oecd.data$nation, origin = "country.name", destination = "iso3c")
asylum.dest = subset(asylum.dest, destiso %in% oecd.data$iso)
asylum.dest = arrange(as.data.frame(asylum.dest), desc(per10000))
asylum.dest$australia = "blue"
pos = asylum.dest$destiso == "AUS"
asylum.dest$australia[pos] = "orange"
barplot(asylum.dest$per10000, names.arg = asylum.dest$destiso, las=2, main = "Number of asylum seekers and refugees in OECD per 10000 in 2014")
asylum.dest$destiso = factor(asylum.dest$destiso, levels = asylum.dest$destiso, ordered=T)
p = ggplot(asylum.dest, aes(x = destiso, y=per10000)) + geom_bar(stat = "identity", fill = asylum.dest$australia)
p = p + theme(legend.position="none", legend.title = element_blank()) 
p = p + ggtitle("Asylum seekers and refugees pre 10,000 in OECD countries - 2014")
p = p + xlab("") + ylab("Asylum seekers and refugees per 10,000 popln")
p= p + theme_few()
ggplotly(p)

###### 

# ######Calculate pc of origin destinations in Asutralia
# asylum.aust = subset(asylum, destiso == "AUS")
# asylum.aust = asylum.aust %>% group_by(destiso, origin.iso) %>% summarise(total = sum(Value))
# asylum.aust = as.data.frame(asylum.aust[,-1]) %>% group_by(origin.iso) %>% summarise(total = sum(total))
# asylum.aust = subset(asylum.aust, total > 100)
# sPDF <- joinCountryData2Map(asylum.aust,
#                             , joinCode = "ISO3"
#                             , nameJoinColumn = "origin.iso"
# )
# mapCountryData( sPDF
#                 , nameColumnToPlot="total",
#                 mapTitle= "Where do Asylum Seekers and Refugees to Australia Come from?"
# )
# ###### end


