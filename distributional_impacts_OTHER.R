##R code for the creation of other interesting distribution/ecological graphs, besides Main Figures and SSMM
##Author: Alba Aguion
##Date: February 15th 2017

#libraries
library(rockchalk)
library(ggplot2) 
library(tibble)
library(tidyverse)
library(plyr)

#open dataset
table<-read.table("biblio_database.csv", header= T, sep= ",")

#Factors
table$researcher<-as.factor(table$researcher)
table$id_study<-as.factor(table$id_study)
table$cc<-as.factor(table$cc)
table$b_impact<-as.factor(table$b_impact)
table$b_direction<-as.factor(table$b_direction)
table$tax_group<-as.factor(table$tax_group)

#Numeric
table$b_value<-as.numeric(as.character(table$b_value), na.omit=TRUE)
table$b_years<-as.numeric(as.character(table$b_years))

#change codes to a short description; in our review center of grav separated from center of biomass
levels(table$b_impact)<-c("lat shift center of grav", #1
                          "lat shift center of bio",  #2
                          "depth shift", #3
                          "boundary lat shift center of grav", #4
                          "boundary lat shift center of bio", #5
                          "long shift center of grav", #7
                          "long shift center of bio", #8
                          "lat and long shift center of grav", #9
                          "shift in area occupied") #11

levels(table$b_direction)<-c("lat shift north center of grav", #1
                             "lat shift south center of grav", #2
                             "boundary lat shift north center of grav", #3
                             "max lat shift north center of grav", #4
                             "max lat shift south center of grav",#5
                             "min lat shift north center of grav", #6
                             "shifting deeper", #7
                             "shifting shallower", #8
                             "area expansion", #9
                             "area contraction", #10
                             "lat shift north center of bio", #11
                             "lat shift south center of bio", #12
                             "shift southweastwards center of grav", #13
                             "shift northeastwards center of grav", #14
                             "shift westwards center of grav", #19
                             "boundary lat shift east center of bio", #20
                             "boundary lat shift north center of bio") #21

levels(table$cc)<-c("AMO", #10
                    "climate velocity", #11
                    "sst", #2
                    "sst,bt,AMO", #2,3,10
                    "sst,bt", #2,3
                    "bt") #3

#new colums with center of grav combined with center of biomass
table["b_impact_combine"]<-table$b_impact
table$b_impact_combine<-combineLevels(table$b_impact_combine, levs = c("lat shift center of grav", "lat shift center of bio"), newLabel = c("lat shift"))
table$b_impact_combine<-combineLevels(table$b_impact_combine, levs = c("boundary lat shift center of grav", "boundary lat shift center of bio"), newLabel = c("boundary lat shift"))
table$b_impact_combine<-combineLevels(table$b_impact_combine, levs = c("long shift center of grav", "long shift center of bio"), newLabel = c("long shift"))
levels(table$b_impact_combine)[levels(table$b_impact_combine)=="lat and long shift center of grav"] <- "lat and long shift"

table["b_direction_combine"]<-table$b_direction
table$b_direction_combine<-combineLevels(table$b_direction_combine, levs = c("lat shift north center of grav", "lat shift north center of bio"), newLabel = c("lat shift north"))
table$b_direction_combine<-combineLevels(table$b_direction_combine, levs = c("lat shift south center of grav", "lat shift south center of bio"), newLabel = c("lat shift south"))
table$b_direction_combine<-combineLevels(table$b_direction_combine, levs = c("boundary lat shift north center of grav", "boundary lat shift north center of bio"), newLabel= c("boundary lat shift north"))
levels(table$b_direction_combine)[levels(table$b_direction_combine)=="max lat shift north center of grav"] <- "max lat shift north"
levels(table$b_direction_combine)[levels(table$b_direction_combine)=="max lat shift south center of grav"] <- "max lat shift south"
levels(table$b_direction_combine)[levels(table$b_direction_combine)=="min lat shift north center of grav"] <- "min lat shift north"
levels(table$b_direction_combine)[levels(table$b_direction_combine)=="shift southweastwards center of grav"] <- "shift southweastwards"
levels(table$b_direction_combine)[levels(table$b_direction_combine)=="shift northeastwards center of grav"] <- "shift northeastwards"
levels(table$b_direction_combine)[levels(table$b_direction_combine)=="shift westwards center of grav"] <- "shift westwards"
levels(table$b_direction_combine)[levels(table$b_direction_combine)=="boundary lat shift east center of bio"] <- "boundary lat shift east"


#SUBSETS COMBINE
lat<-subset (table, b_impact_combine=="lat shift")
lat$b_direction_combine<-factor(lat$b_direction_combine)

depth<-subset (table, b_impact_combine=="depth shift")
depth$b_direction_combine<-factor(depth$b_direction_combine)

boundary<-subset (table, b_impact_combine== "boundary lat shift")
boundary$b_direction_combine<-factor(boundary$b_direction_combine)

area<-subset (table, b_impact_combine== "shift in area occupied")
area$b_direction_combine<-factor(area$b_direction_combine)

lat_long<-subset (table, b_impact_combine== "lat and long shift")
lat_long$b_direction_combine<-factor(lat_long$b_direction_combine)

long<-subset (table, b_impact_combine== "long shift")
long$b_direction_combine<-factor(long$b_direction_combine)

#Graph 1
ggplot(lat, aes(x = site, y = b_value, fill=site)) +
  geom_boxplot() + scale_x_discrete(name = "") +
  scale_y_continuous(name = "Latitude shift (Km/decade)")+
  theme_bw()

levels(lat$b_impact)
lat$b_impactx<-factor(lat$b_impact)
lat$sitex<-factor(lat$site)
lat$b_direction_combinex<-factor(lat$b_direction_combine)

gpmeans<-tapply(lat$b_value,list(lat$sitex,lat$b_direction_combinex),mean)
n<-tapply(lat$b_value,list(lat$sitex,lat$b_direction_combinex),length)

#Graph 2
ggplot(lat, aes(x = id_study, y = b_value, fill=id_study)) +
  geom_boxplot() + scale_x_discrete(name = "") +
  scale_y_continuous(name = "Latitude shift (Km/decade)")+
  theme_bw()

#Graph 3
ggplot(depth, aes(x = site, y = b_value, fill=site)) +
  geom_boxplot() + scale_x_discrete(name = "") +
  scale_y_continuous(name = "Depth shift (m/decade)")+
  theme_bw()

#Graph4
ggplot(depth, aes(x = id_study, y = b_value, fill=id_study)) +
  geom_boxplot() + scale_x_discrete(name = "") +
  scale_y_continuous(name = "Depth shift (m/decade)")+
  theme_bw()

#Graph 5
ggplot(boundary, aes(x = id_study, y = b_value, fill=id_study)) +
  geom_boxplot() + scale_x_discrete(name = "") +
  scale_y_continuous(name = "Boundary Latitude shift (Km/decade)")+
  theme_bw()

#Graph 6
ggplot(boundary, aes(x = site, y = b_value, fill=site)) +
  geom_boxplot() + scale_x_discrete(name = "") +
  scale_y_continuous(name = "Boundary Latitude shift (Km/decade)")+
  theme_bw()

#Graph 7
ggplot(area, aes(x = id_study, y = b_value, fill=id_study)) +
  geom_boxplot() + scale_x_discrete(name = "") +
  scale_y_continuous(name = "Area shift (Km2/decade)")+
  theme_bw()

#Graph 8
ggplot(area, aes(x = site, y = b_value, fill=site)) +
  geom_boxplot() + scale_x_discrete(name = "") +
  scale_y_continuous(name = "Area shift (Km2/decade)")+
  theme_bw()

############
# ANALYSIS #
############

#Latitude observations significantly differ between north and south
hist(lat$b_value)
shapiro.test(lat$b_value)
lat$b_value_transformed<-log(lat$b_value+400)
summary(aov(lat$b_value_transformed~lat$b_direction_combine))
gpmeanslat<-tapply(lat$b_value,lat$b_direction_combine,mean);gpmeanslat
nlat<-tapply(lat$b_value,lat$b_direction_combine,length);nlat

#Boundary latitude observations significantly differ between north and south
hist(boundary$b_value)
shapiro.test(boundary$b_value)
boundary$b_value_transformed<-log(boundary$b_value+400)
summary(aov(boundary$b_value_transformed~boundary$b_direction_combine))
gpmeansboundary<-tapply(boundary$b_value,boundary$b_direction_combine,mean);gpmeansboundary
nboundary<-tapply(boundary$b_value,boundary$b_direction_combine,length);nboundary

#Depth observations significantly differ between deeper and shallower
hist(depth$b_value)
shapiro.test(depth$b_value)
depth$b_value_transformed<-log(depth$b_value+100)
summary(aov(depth$b_value_transformed~depth$b_direction_combine))
gpmeansdepth<-tapply(depth$b_value,depth$b_direction_combine,mean);gpmeansdepth
ndepth<-tapply(depth$b_value,depth$b_direction_combine,length);ndepth

##########
# GRAPHS #
##########

################################################################
#Do the different taxonomic groups have a different shift rate?#
################################################################

#1. for latitude
hist(lat$b_value)
hist(log(lat$b_value+300))
lat$b_value_transformed<-log(lat$b_value+300)
summary(aov(lat$b_value_transformed~lat$tax_group)) 
TukeyHSD(aov(lat$b_value_transformed~lat$tax_group)) #where are the dif?

gpmeans<-tapply(lat$b_value,list(lat$tax_group,lat$b_direction_combine),mean)
gpsd<-tapply(lat$b_value,list(lat$tax_group,lat$b_direction_combine),sd)
n<-tapply(lat$b_value,list(lat$tax_group,lat$b_direction_combine),length)
degf<-tapply(lat$b_value,list(lat$tax_group,lat$b_direction_combine),length)-1

#error bar function
error.bar = function(x, m, upper, lower=upper, length=0.1){
  if(length(x) != length(m) | length(m) !=length(lower) | 
     length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,m+upper, x, m-lower, angle=90, code=3, length=length)
}

#barplot colours
barx<-barplot(gpmeans,  ylim= c(-75,75),
              axis.lty=0, axisnames=FALSE, xlab="Taxonomic group", col= c("white","black","yellow", "blue", "pink"),
              ylab="Latitude shift rate (Km/decade)", beside=T)

error.bar(barx,gpmeans, gpsd/sqrt(n))

legend("topright", #change position of the legend
       legend=c("Bony fish","Non-bony fish", "Benthic crustacea", "Squid", "Benthic mollusca"),
       pch=c(22,22), pt.bg=c("white","black","yellow", "blue", "pink")) 

#barplot white
barx<-barplot(gpmeans,  ylim= c(-75,75),
              axis.lty=0, axisnames=FALSE, xlab="Taxonomic group", col= "white",
              ylab="Latitude shift rate (Km/decade)", beside=T)

error.bar(barx,gpmeans, gpsd/sqrt(n))

hist(lat$b_value)
hist(log(lat$b_value+300))
summary(aov(log(lat$b_value+300)~lat$tax_group)*lat$b_direction_combine)
TukeyHSD(aov(log(lat$b_value+300)~lat$tax_group)) #no significant differences, weird since the one before was? but we are considering north and south together?

#north
lat.north<-subset(lat, b_direction_combine=="lat shift north")
lat.north$b_direction_combine<-factor(lat.north$b_direction_combine)
hist(lat.north$b_value) #NO NORMAL DISTRIBUTION
hist(log(lat.north$b_value)) # MORE NORMAL DISTRIBUTION?
summary(aov(log(lat.north$b_value)~lat.north$tax_group)) #no sig dif
kruskal.test(lat.north$b_value~lat.north$tax_group) #no sig dif

#south
lat.south<-subset(lat, b_direction_combine=="lat shift south")
lat.south$b_direction_combine<-factor(lat.south$b_direction_combine)
hist(lat.south$b_value) #NO NORMAL DISTRIBUTION
hist(log(lat.south$b_value+120)) # MORE NORMAL DISTRIBUTION?
summary(aov(log(lat.south$b_value+120)~lat.south$tax_group)) #sig dif
TukeyHSD(aov(log(lat.south$b_value+120)~lat.south$tax_group))
kruskal.test(lat.south$b_value~lat.south$tax_group) #sig dif

#barplot species going south
gpmeans<-tapply(lat.south$b_value,lat.south$tax_group,mean)
gpsd<-tapply(lat.south$b_value,lat.south$tax_group,sd)
n<-tapply(lat.south$b_value,lat.south$tax_group,length)
degf<-tapply(lat.south$b_value,lat.south$tax_group,length)-1

#barplot 
barx<-barplot(gpmeans,  ylim= c(-50,0),
              axis.lty=0, axisnames=FALSE, xlab="Taxonomic group", col= "white",
              ylab="Latitude shift rate (Km/decade)", beside=T)

error.bar(barx,gpmeans, gpsd/sqrt(n))


#1. for depth
hist(depth$b_value)
hist(log(depth$b_value+81))
depth$b_value_transformed<-log(depth$b_value+81)
summary(aov(depth$b_value_transformed~depth$tax_group))


gpmeans<-tapply(depth$b_value,list(depth$tax_group,depth$b_direction_combine),mean)
gpsd<-tapply(depth$b_value,list(depth$tax_group,depth$b_direction_combine),sd)
n<-tapply(depth$b_value,list(depth$tax_group,depth$b_direction_combine),length)
degf<-tapply(depth$b_value,list(depth$tax_group,depth$b_direction_combine),length)-1

#barplot colours
barx<-barplot(gpmeans,  ylim= c(-75,75),
              axis.lty=0, axisnames=FALSE, xlab="Taxonomic group", col= c("white","black","yellow", "blue", "pink"),
              ylab="Depth shift rate (m/decade)", beside=T)

error.bar(barx,gpmeans, gpsd/sqrt(n))

legend("topright", #change position of the legend
       legend=c("Bony fish","Non-bony fish", "Benthic crustacea", "Squid", "Benthic mollusca"),
       pch=c(22,22), pt.bg=c("white","black","yellow", "blue", "pink")) 

#barplot white
barx<-barplot(gpmeans,  ylim= c(-75,75),
              axis.lty=0, axisnames=FALSE, xlab="Taxonomic group", col= "white",
              ylab="Depth shift rate (m/decade)", beside=T)

error.bar(barx,gpmeans, gpsd/sqrt(n))

#deeper
deeper<-subset(depth, b_direction_combine=="shifting deeper")
deeper$b_direction_combine<-factor(deeper$b_direction_combine)
hist(deeper$b_value) #NO NORMAL DISTRIBUTION
hist(log(deeper$b_value+81)) # MORE NORMAL DISTRIBUTION?
summary(aov(log(deeper$b_value+81)~deeper$tax_group)) #sig dif
TukeyHSD(aov(log(deeper$b_value+81)~deeper$tax_group))
kruskal.test(deeper$b_value~deeper$tax_group) #no sig dif

#shallower
shallower<-subset(depth, b_direction_combine=="shifting shallower")
shallower$b_direction_combine<-factor(shallower$b_direction_combine)
hist(shallower$b_value) #NO NORMAL DISTRIBUTION
hist(log(shallower$b_value)) # MORE NORMAL DISTRIBUTION?
summary(aov(log(shallower$b_value)~shallower$tax_group)) #no sig dif
TukeyHSD(aov(log(shallower$b_value)~shallower$tax_group))
kruskal.test(shallower$b_value~shallower$tax_group) #no sig dif


#shallower plot
gpmeans<-tapply(shallower$b_value,shallower$tax_group,mean)
gpsd<-tapply(shallower$b_value,shallower$tax_group,sd)
n<-tapply(shallower$b_value,shallower$tax_group,length)
degf<-tapply(shallower$b_value,shallower$tax_group,length)-1

#barplot colours
barx<-barplot(gpmeans,  ylim= c(0,25),
              axis.lty=0, axisnames=FALSE, xlab="Taxonomic group", col= "white",
              ylab="Depth shift rate (m/decade)", beside=T)

error.bar(barx,gpmeans, gpsd/sqrt(n))


############################################################
#Do stocks of different latitudes shift at different rates?#
############################################################

f.lat<-data.frame(table$tax_group, table$lat_dec, table$b_impact_combine, table$b_value)
colnames(f.lat)[2] <- "lat"
colnames(f.lat)[3] <- "impact"
colnames(f.lat)[4] <- "value"

f.lat.lat<-subset(f.lat, impact=="lat shift")
f.lat.depth<-subset(f.lat, impact=="depth shift")
f.lat.boundary<-subset(f.lat, impact=="boundary lat shift")

f.lat.lat$table.tax_group <- factor (f.lat.lat$table.tax_group, levels= c ("1", 
                                                                           "2","3", "4", "5"), labels=c("Bony fish", "Non bony fish", "Benthic Crustacea", "Squid", "Benthic mollusca"))                                       
f.lat <- ggplot(f.lat.lat, aes(x=value, y=lat, fill=table.tax_group)) + geom_point(shape=1)
f.lat + geom_point(aes(colour = table.tax_group))+
  ggtitle("") +
  labs(x = "Latitude shift rate (km/decade)", y = "Stock latitude")

plot(lat$b_value,lat$lat_dec)
cor.test(lat$b_value,lat$lat_dec, method="spearman") #no sig

f.lat.depth$table.tax_group <- factor (f.lat.depth$table.tax_group, levels= c ("1", 
                                                                               "2","3", "4", "5"), labels=c("Bony fish", "Non bony fish", "Benthic Crustacea", "Squid", "Benthic mollusca"))                     
f.depth <- ggplot(f.lat.depth, aes(x=value, y=lat, fill=table.tax_group)) + geom_point(shape=1)
f.depth + geom_point(aes(colour = table.tax_group))+
  ggtitle("") +
  labs(x = "Depth shift rate (m/decade)", y = "Stock latitude")

f.lat.boundary$table.tax_group <- factor (f.lat.boundary$table.tax_group, levels= c ("1" 
), labels=c("Bony fish"))                     
f.boundary <- ggplot(f.lat.boundary, aes(x=value, y=lat, fill=table.tax_group)) + geom_point(shape=1)
f.boundary + geom_point(aes(colour = table.tax_group))+
  ggtitle("") +
  labs(x = "Boundary Latitude shift rate (km/decade)", y = "Stock latitude")

plot(depth$b_value,depth$lat_dec)
cor.test(depth$b_value,depth$lat_dec, method="pearson") #no sig


###########################################################################################
#Using fishbase environments, Do the species have different shifting rates? (STOCK LEVEL) #
###########################################################################################

#open dataset
table<-read.table("ReviewDatst.csv", header= T, sep= ",")
str(table)


#Factors
table$researcher<-as.factor(table$researcher)
table$id_study<-as.factor(table$id_study)
table$cc<-as.factor(table$cc)
table$b_impact<-as.factor(table$b_impact)
table$b_direction<-as.factor(table$b_direction)
table$tax_group<-as.factor(table$tax_group)

#Numeric
table$b_value<-as.numeric(as.character(table$b_value), na.omit=TRUE)
table$b_years<-as.numeric(as.character(table$b_years))

#change codes to a short description; in our review center of grav separated from center of biomass
levels(table$b_impact)<-c("lat shift center of grav", #1
                          "lat shift center of bio",  #2
                          "depth shift", #3
                          "boundary lat shift center of grav", #4
                          "boundary lat shift center of bio", #5
                          "long shift center of grav", #7
                          "long shift center of bio", #8
                          "lat and long shift center of grav", #9
                          "shift in area occupied") #11

levels(table$b_direction)<-c("lat shift north center of grav", #1
                             "lat shift south center of grav", #2
                             "boundary lat shift north center of grav", #3
                             "max lat shift north center of grav", #4
                             "max lat shift south center of grav",#5
                             "min lat shift north center of grav", #6
                             "shifting deeper", #7
                             "shifting shallower", #8
                             "area expansion", #9
                             "area contraction", #10
                             "lat shift north center of bio", #11
                             "lat shift south center of bio", #12
                             "shift southweastwards center of grav", #13
                             "shift northeastwards center of grav", #14
                             "shift westwards center of grav", #19
                             "boundary lat shift east center of bio", #20
                             "boundary lat shift north center of bio") #21

levels(table$cc)<-c("AMO", #10
                    "climate velocity", #11
                    "sst", #2
                    "sst,bt,AMO", #2,3,10
                    "sst,bt", #2,3
                    "bt") #3

#new colums with center of grav combined with center of biomass
table["b_impact_combine"]<-table$b_impact
table$b_impact_combine<-combineLevels(table$b_impact_combine, levs = c("lat shift center of grav", "lat shift center of bio"), newLabel = c("lat shift"))
table$b_impact_combine<-combineLevels(table$b_impact_combine, levs = c("boundary lat shift center of grav", "boundary lat shift center of bio"), newLabel = c("boundary lat shift"))
table$b_impact_combine<-combineLevels(table$b_impact_combine, levs = c("long shift center of grav", "long shift center of bio"), newLabel = c("long shift"))
levels(table$b_impact_combine)[levels(table$b_impact_combine)=="lat and long shift center of grav"] <- "lat and long shift"

table["b_direction_combine"]<-table$b_direction
table$b_direction_combine<-combineLevels(table$b_direction_combine, levs = c("lat shift north center of grav", "lat shift north center of bio"), newLabel = c("lat shift north"))
table$b_direction_combine<-combineLevels(table$b_direction_combine, levs = c("lat shift south center of grav", "lat shift south center of bio"), newLabel = c("lat shift south"))
table$b_direction_combine<-combineLevels(table$b_direction_combine, levs = c("boundary lat shift north center of grav", "boundary lat shift north center of bio"), newLabel= c("boundary lat shift north"))
levels(table$b_direction_combine)[levels(table$b_direction_combine)=="max lat shift north center of grav"] <- "max lat shift north"
levels(table$b_direction_combine)[levels(table$b_direction_combine)=="max lat shift south center of grav"] <- "max lat shift south"
levels(table$b_direction_combine)[levels(table$b_direction_combine)=="min lat shift north center of grav"] <- "min lat shift north"
levels(table$b_direction_combine)[levels(table$b_direction_combine)=="shift southweastwards center of grav"] <- "shift southweastwards"
levels(table$b_direction_combine)[levels(table$b_direction_combine)=="shift northeastwards center of grav"] <- "shift northeastwards"
levels(table$b_direction_combine)[levels(table$b_direction_combine)=="shift westwards center of grav"] <- "shift westwards"
levels(table$b_direction_combine)[levels(table$b_direction_combine)=="boundary lat shift east center of bio"] <- "boundary lat shift east"

#Counts
tapply(table$b_impact_combine, table$tax_group, length)
tapply(table$b_impact_combine, table$EnvTemp, length)
tapply(table$b_impact_combine, table$Resilience, length)

#SUBSETS COMBINE
lat<-subset (table, b_impact_combine=="lat shift")
lat$b_direction_combine<-factor(lat$b_direction_combine)

depth<-subset (table, b_impact_combine=="depth shift")
depth$b_direction_combine<-factor(depth$b_direction_combine)

boundary<-subset (table, b_impact_combine== "boundary lat shift")
boundary$b_direction_combine<-factor(boundary$b_direction_combine)

area<-subset (table, b_impact_combine== "shift in area occupied")
area$b_direction_combine<-factor(area$b_direction_combine)

lat_long<-subset (table, b_impact_combine== "lat and long shift")
lat_long$b_direction_combine<-factor(lat_long$b_direction_combine)

long<-subset (table, b_impact_combine== "long shift")
long$b_direction_combine<-factor(long$b_direction_combine)


#error bar function
error.bar = function(x, m, upper, lower=upper, length=0.1){
  if(length(x) != length(m) | length(m) !=length(lower) | 
     length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,m+upper, x, m-lower, angle=90, code=3, length=length)
}


gpmeans<-tapply(lat$b_value,lat$EnvTemp,mean)
gpsd<-tapply(lat$b_value,lat$EnvTemp,sd)
n<-tapply(lat$b_value,lat$EnvTemp,length)
degf<-tapply(lat$b_value,lat$EnvTemp,length)-1

barx<-barplot(gpmeans,ylim=c(-30,70),
              axis.lty=0, xlab="Environment",
              ylab="Latitude shift rate (Km/decade)", beside=T)

error.bar(barx,gpmeans, gpsd/sqrt(n))

hist(lat$b_value) #NO NORMAL DISTRIBUTION
shapiro.test(lat$b_value)
hist(log(lat$b_value+400)) # MORE NORMAL DISTRIBUTION?
summary(aov(log(lat$b_value+400)~lat$EnvTemp)) #no sig dif
kruskal.test(lat$b_value~lat$EnvTemp) #no sig dif


gpmeans<-tapply(depth$b_value,depth$EnvTemp,mean)
gpsd<-tapply(depth$b_value,depth$EnvTemp,sd)
n<-tapply(depth$b_value,depth$EnvTemp,length)
degf<-tapply(depth$b_value,depth$EnvTemp,length)-1

barx<-barplot(gpmeans, ylim=c(-20,20),
              axis.lty=0, xlab="Environment",
              ylab="Depth shift rate (m/decade)", beside=T)

error.bar(barx,gpmeans, gpsd/sqrt(n))

hist(depth$b_value) #NO NORMAL DISTRIBUTION
shapiro.test(depth$b_value)
hist(log(depth$b_value+100)) # MORE NORMAL DISTRIBUTION?
summary(aov(log(depth$b_value+100)~depth$EnvTemp)) #sig dif
TukeyHSD(aov(log(depth$b_value+100)~depth$EnvTemp))
kruskal.test(lat$b_value~lat$EnvTemp) #no sig dif