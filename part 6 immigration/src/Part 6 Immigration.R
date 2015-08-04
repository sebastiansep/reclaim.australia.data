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
p = p + theme(legend.position="none")
p = p + ggtitle("Asylum seekers and refugees pre 10,000 in OECD countries - 2014")
p = p + xlab("") + ylab("Asylum seekers and refugees per 10,000 popln")
p= p + theme_few()
ggplotly(p)

###### 

######Calculate pc of origin destinations in Asutralia
asylum.aust = subset(asylum, destiso == "AUS")
asylum.aust = asylum.aust %>% group_by(destiso, origin.iso) %>% summarise(total = sum(Value))
asylum.aust = as.data.frame(asylum.aust[,-1]) %>% group_by(origin.iso) %>% summarise(total = sum(total))
asylum.aust = subset(asylum.aust, total > 100)
sPDF <- joinCountryData2Map(asylum.aust,
                            , joinCode = "ISO3"
                            , nameJoinColumn = "origin.iso"
)
mapCountryData( sPDF
                , nameColumnToPlot="total",
                mapTitle= "Where do Asylum Seekers and Refugees to Australia Come from?"
)
###### end


