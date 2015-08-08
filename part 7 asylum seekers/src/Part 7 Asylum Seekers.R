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
library(ggthemes)
library(reshape2)
library(scales)
library(dplyr)
library(sep)
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
asylumarrivals = subset(asylumarrivals, variable=="Asylum Seekers")
p = ggplot(asylumarrivals, aes(x=Year, y = value, colour = variable)) +  geom_bar(stat="identity", fill = "blue", colour = "blue")
p = sep.theme(p)
p = p + theme(legend.position="none", legend.title = element_blank()) 
p = p + ggtitle("Annual Number of Asylum Seekers in Australia since 2010\n")
p = p + xlab("") + scale_y_continuous(name="Number of New People", labels = comma)
p = p  + scale_fill_manual(values = alpha(c("blue", "orange", "brown"), .3))
sep.png(p, "austasylumbarchart")
######

###### Plot australian budget
temp = budget[1:8,3:4]
temp = arrange(temp, desc(Expenditure))
temp$Sector = factor(temp$Sector, levels = temp$Sector, ordered=T)
p = ggplot(temp, aes(x = Sector, y=Expenditure, fill = Sector)) + geom_bar(stat = "identity", position = "stack")
p = sep.theme(p, 45)
p = p + theme(legend.position = "none")
p = p + ggtitle("Australian Budgetary Expenditure - 2015")
p = p + xlab("") + ylab("Australian Dollars (Billions)")
p$layout$clip[p$layout$name == "panel"] <- "off"
sep.png(p, "austbudget")

temp$end = cumsum(temp$Expenditure)
temp$start = c(0, temp$end[-length(temp$end)])
temp$id = 1:nrow(temp)
temp$Sector = factor(temp$Sector, levels = temp$Sector, ordered=T)
p = ggplot(temp, aes(Sector, fill = Sector)) + geom_rect(aes(x = Sector,
                                                                 xmin = id - 0.45, xmax = id + 0.45, ymin = end,
                                                                 ymax = start))
p = sep.theme(p, 45)
p = p + theme(axis.text.x = element_blank())
p = p + theme(legend.position = "none")
p = p  + geom_text(data=temp, 
                   aes(Sector, end-80, label=Sector),
                   size=20, angle = 90)
p = p + ggtitle("Australian Budgetary Expenditure - 2015")
p = p + xlab("") + ylab("Australian Dollars (Billions)")
p$layout$clip[p$layout$name == "panel"] <- "off"
sep.png(p, "austbudgetwaterfall")

# p = p + theme_few()
# p = p + ylab("Prevalence (%)") + xlab("") + ggtitle("Prevalence of Themes in Osama Bin Laden's Messages")
# p = p + theme(axis.text.x = element_blank())

###### 

###### Plot cost per day asylum seekers
temp = budget[11:12,1:2]
names(temp) = c("Method", "Cost")
temp$Method = factor(temp$Method, levels = temp$Method, ordered=T)
p = ggplot(temp, aes(x = Method, y=Cost, fill = Method)) + geom_bar(stat = "identity", position = "stack")
p = sep.theme(p)
p = p + theme(legend.position="none") 
p = p + ggtitle("UNHCR Estimated Costs Per Day of Different Asylum Seeker Policy")
p = p + xlab("") + ylab("US Dollars per Day")

sep.png(p, "asylumcosts")

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
asylum.dest$destiso = factor(asylum.dest$destiso, levels = asylum.dest$destiso, ordered=T)
p = ggplot(asylum.dest, aes(x = destiso, y=per10000)) + geom_bar(stat = "identity", fill = asylum.dest$australia)
p = sep.theme(p)
p = p + theme(legend.position="none", legend.title = element_blank()) 
p = p + ggtitle("Asylum seekers and refugees pre 10,000 in OECD countries - 2014")
p = p + xlab("") + ylab("Asylum seekers and refugees per 10,000 popln")
sep.png(p, "oecdasylum")

###### 

# ######Calculate pc of origin destinations in Australia
asylum.aust = subset(asylum, destiso == "AUS")
asylum.aust = asylum.aust %>% group_by(destiso, origin.iso) %>% summarise(total = sum(Value))
asylum.aust = as.data.frame(asylum.aust[,-1]) %>% group_by(origin.iso) %>% summarise(total = sum(total))
names(asylum.aust) = c("iso3c", "value")
asylum.aust = arrange(asylum.aust, desc(value))
valuemapped = "Number of\nAsylum Seekers"
map = sep.map.data(asylum.aust, valuemapped)
p = ggplot(map,aes(long,lat,group=group,fill=`Number of\nAsylum Seekers`))+
  geom_polygon(color="black")+
  scale_fill_gradient(low="white",high="red")+
  theme(panel.background = element_rect(fill = "lightsteelblue2"))
p = sep.map.theme(p)
p = p + ggtitle("Sources of Australian Asylum Seekers")
p = sep.map.png(p, "asylumsources")
# ###### end


