# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

sep.theme <- function(p, x.axis.rotation=0)
{
  require(ggthemes)
  p= p + theme_few()
  p = p + theme(legend.position="top", legend.text =element_text(size=40,face="bold"), legend.title = element_blank(), axis.text.x = element_text(angle = x.axis.rotation,size=40,face="bold"),
                axis.title=element_text(size=40,face="bold"),  title=element_text(size=40,face="bold"), axis.text.y = element_text(size=40,face="bold"), panel.border = element_blank(),
                axis.line = element_line(colour = "black", size = 2), legend.title = element_blank())
  p = p +  guides(colour = guide_legend(override.aes = list(size=6)))
  return(p)
}
sep.png <- function(p, filename)
{
  filename = paste("./graphs/",filename, ".png", sep = "")
  png(filename, width = 2560, height = 1920)
  print(p)
  dev.off()
}

sep.map.theme = function(p)
{
  p = p + theme(axis.ticks = element_blank(), axis.text = element_blank(),
                legend.text =element_text(size=15,face="bold"),
                title=element_text(size=20,face="bold"),
                legend.title = element_text(size=15,face="bold"),
                legend.position="right",panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  p = p + ylab("") + xlab("")
  return(p)
}

sep.map.png <- function(p, filename)
{
  filename = paste("./graphs/",filename, ".png", sep = "")
  png("./graphs/asylumsources.png", width =1750, height = 900)
  print(p)
  dev.off()
}

sep.map.data <- function(df, valuemapped)
{
  library(maps)
  require(data.table)
  world<-map_data("world")
  w2<-data.frame(world)
  w2$iso3c = countrycode(w2$region, "country.name", "iso3c")
  w2 = subset(w2, iso3c !="ATA")
  key = match(w2$iso3c, df$iso3c)
  w2$value = 0
  w2$value = df$value[key]
  pos = is.na(w2$value)
  w2$value[pos] = 0
  setnames(w2, "value", valuemapped)
  return(w2)
}


