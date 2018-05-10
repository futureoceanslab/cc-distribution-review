##R code for the analysis of our review integrated with Fishbase information
##Author: Alba Aguion
##Date: February 15th 2017

#libraries
library(rockchalk)
library(ggplot2) 
library(tidyverse)
library(plyr)

# STOCK LEVEL####

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

#Counts
tapply(table$b_impact_combine, table$tax_group, length)
tapply(table$b_impact_combine, table$EnvTemp, length)
tapply(table$b_impact_combine, table$Resilience, length)



#EnvTemp
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




# SPECIES LEVEL####

#open dataset
table<-read.table("ReviewDatsp.csv", header= T, sep= ",")
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

#Counts
tapply(table$b_impact_combine, table$DemersPelag, length)
