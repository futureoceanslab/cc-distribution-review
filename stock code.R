##R code for the analysis of Resilience and Environment at STOCK level after the integration of Database Review and FISH base
##Author: Alba
##Date: April 19th 2017

#set working directory
setwd("C:/Users/alba.aguion/OneDrive/CLOCK_TEAM/03_FUTURE OCEANS/FO_BIBLIO/FO_DATA_ANALYSIS/R for fishbase review")
ReviewSt <- read.csv("ReviewDatst.csv", header=T, sep = ",")

#libraries
library(rockchalk)
library(ggplot2)
library(Rmisc)

str(ReviewSt)

#Factors
ReviewSt$researcher<-as.factor(ReviewSt$researcher)
ReviewSt$id_study<-as.factor(ReviewSt$id_study)
ReviewSt$cc<-as.factor(ReviewSt$cc)
ReviewSt$b_impact<-as.factor(ReviewSt$b_impact)
ReviewSt$b_direction<-as.factor(ReviewSt$b_direction)
ReviewSt$cc<-as.factor(ReviewSt$cc)

#Numeric
ReviewSt$b_value<-as.numeric(as.character(ReviewSt$b_value), na.omit=TRUE)
ReviewSt$b_years<-as.numeric(as.character(ReviewSt$b_years))

#change codes to a short description; in our review center of grav separated from center of biomass
levels(ReviewSt$b_impact)<-c("lat shift center of grav", #1
                             "lat shift center of bio",  #2
                             "depth shift", #3
                             "boundary lat shift center of grav", #4
                             "boundary lat shift center of bio", #5
                             "long shift center of grav", #7
                             "long shift center of bio", #8
                             "lat and long shift center of grav", #9
                             "shift in area occupied") #11

levels(ReviewSt$b_direction)<-c("lat shift north center of grav", #1
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

levels(ReviewSt$cc)<-c("AMO", #10
                       "climate velocity", #11
                       "sst", #2
                       "sst,bt,AMO", #2,3,10
                       "sst,bt", #2,3
                       "bt") #3


##FOR THIS ANALYSIS WE ARE GOING TO COMBINE CENTER OF GRAVITY AND CENTER OF BIOMASS
#new colums with center of grav combined with center of biomass
ReviewSt["b_impact_combine"]<-ReviewSt$b_impact
ReviewSt$b_impact_combine<-combineLevels(ReviewSt$b_impact_combine, levs = c("lat shift center of grav", "lat shift center of bio"), newLabel = c("lat shift"))
ReviewSt$b_impact_combine<-combineLevels(ReviewSt$b_impact_combine, levs = c("boundary lat shift center of grav", "boundary lat shift center of bio"), newLabel = c("boundary lat shift"))
ReviewSt$b_impact_combine<-combineLevels(ReviewSt$b_impact_combine, levs = c("long shift center of grav", "long shift center of bio"), newLabel = c("long shift"))
levels(ReviewSt$b_impact_combine)[levels(ReviewSt$b_impact_combine)=="lat and long shift center of grav"] <- "lat and long shift"

ReviewSt["b_direction_combine"]<-ReviewSt$b_direction
ReviewSt$b_direction_combine<-combineLevels(ReviewSt$b_direction_combine, levs = c("lat shift north center of grav", "lat shift north center of bio"), newLabel = c("lat shift north"))
ReviewSt$b_direction_combine<-combineLevels(ReviewSt$b_direction_combine, levs = c("lat shift south center of grav", "lat shift south center of bio"), newLabel = c("lat shift south"))
ReviewSt$b_direction_combine<-combineLevels(ReviewSt$b_direction_combine, levs = c("boundary lat shift north center of grav", "boundary lat shift north center of bio"), newLabel= c("boundary lat shift north"))
levels(ReviewSt$b_direction_combine)[levels(ReviewSt$b_direction_combine)=="max lat shift north center of grav"] <- "max lat shift north"
levels(ReviewSt$b_direction_combine)[levels(ReviewSt$b_direction_combine)=="max lat shift south center of grav"] <- "max lat shift south"
levels(ReviewSt$b_direction_combine)[levels(ReviewSt$b_direction_combine)=="min lat shift north center of grav"] <- "min lat shift north"
levels(ReviewSt$b_direction_combine)[levels(ReviewSt$b_direction_combine)=="shift southweastwards center of grav"] <- "shift southweastwards"
levels(ReviewSt$b_direction_combine)[levels(ReviewSt$b_direction_combine)=="shift northeastwards center of grav"] <- "shift northeastwards"
levels(ReviewSt$b_direction_combine)[levels(ReviewSt$b_direction_combine)=="shift westwards center of grav"] <- "shift westwards"
levels(ReviewSt$b_direction_combine)[levels(ReviewSt$b_direction_combine)=="boundary lat shift east center of bio"] <- "boundary lat shift east"

#Subsets for b_impact_combine
lat<-subset (ReviewSt, b_impact_combine=="lat shift")
depth<-subset (ReviewSt, b_impact_combine=="depth shift")
boundary<-subset (ReviewSt, b_impact_combine== "boundary lat shift")
area<-subset (ReviewSt, b_impact_combine== "shift in area occupied")
lat_long<-subset (ReviewSt, b_impact_combine== "lat and long shift")
long<-subset (ReviewSt, b_impact_combine== "long shift")

#Subset for b_direction_combine
lat_north<-subset (ReviewSt, b_direction_combine =="lat shift north")
lat_south<-subset (ReviewSt, b_direction_combine =="lat shift south")
deeping<-subset (ReviewSt, b_direction_combine =="shifting deeper")
shallowing<-subset (ReviewSt, b_direction_combine =="shifting shallower")
area_contraction<-subset (ReviewSt, b_direction_combine=="area contraction")
area_expansion<-subset (ReviewSt, b_direction_combine=="area expansion")


############
#Resilience#  #no sig results
############

#Latitudinal shifts and Resilience
stripchart(lat$b_value~lat$Resilience, vertical=T,pch=20,
           col=c("blue","green", "red", "black"),method="jitter",jitter=0.2,
           ylab="Latitude shift (km/dec)")


mod1<-aov(b_value~Resilience, data=lat)
summary(mod1)
tapply(lat$b_value, lat$Resilience, mean, na.omit=TRUE)
#no sig difference 

#Depth shifts and Resilience
stripchart(depth$b_value~depth$Resilience, vertical=T,pch=20,
           col=c("blue","green", "red", "black"),method="jitter",jitter=0.2,
           ylab="Depth shift (m/dec)")


mod2<-aov(b_value~Resilience, data=depth)
summary(mod2)
tapply(depth$b_value, depth$Resilience, mean)
#no sig dif

#Boundary shifts and Resilience
stripchart(boundary$b_value~boundary$Resilience, vertical=T,pch=20,
           col=c("blue","green", "red", "black"),method="jitter",jitter=0.2,
           ylab="Range shift (km/dec)")


mod3<-aov(b_value~Resilience, data=boundary)
summary(mod3)
tapply(boundary$b_value, boundary$Resilience, mean)
#No sig dif

#Area occupied and Resilience
stripchart(area$b_value~area$Resilience, vertical=T,pch=20,
           col=c("blue","green", "red", "black"),method="jitter",jitter=0.2,
           ylab="Area occupied shift (km2/dec)")


mod4<-aov(b_value~Resilience, data=area)
summary(mod4)
tapply(area$b_value, area$Resilience, mean)
#no sig



###Resilience graph

data1<-read.table("resilience.csv", header= T, sep= "," )

data1$resilience <- factor(data1$resilience, levels = c("high","medium", "low", "very_low"),
                      labels=c("high", "medium", "low", "very low"))

data1$impact <- factor (data1$impact, levels= c ("area", 
                                                 "boundary","depth","latitude"), labels=c("Area","Boundary Latitude", "Depth", "Mean Latitude"))                                       

ggplot(data = data1, aes(x = impact, y = Sample.Size, fill = factor(resilience))) + 
  geom_bar(stat="identity", alpha=0.8) + scale_fill_manual(values=c("black", "grey", "yellow", "blue")) +
  labs( y = "Number of Observations", x = "Impact", fill = "Resilience")+ theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'))





#########
#EnvTemp#
#########

levels(ReviewSt$EnvTemp)

#Latitudinal shifts and Environment
stripchart(lat$b_value~lat$EnvTemp, vertical=T,pch=20,
           col=c("blue","green", "red", "black"),method="jitter",jitter=0.2,
           ylab="Latitude shift (km/dec)")


mod5<-aov(b_value~EnvTemp, data=lat)
summary(mod5)
#no sig
tapply(area$b_value, area$EnvTemp, mean)


##############################
#Depth shifts and Environment# #sig
##############################

stripchart(depth$b_value~depth$EnvTemp, vertical=T,pch=20,
           col=c("blue","green", "red", "black"),method="jitter",jitter=0.2,
           ylab="Depth shift (m/dec)")

shapiro.test(depth$b_value)

log_b_value<-log(depth$b_value+81)

mod6<-aov(log_b_value~EnvTemp, data=depth)
summary(mod6)
tapply(depth$b_value, depth$EnvTemp, mean)

#SIGNIFICANT DIFFERENCES

TukeyHSD(aov(log_b_value~EnvTemp, data=depth))

#Difference between subtropical and deep water temperate deep-water

gpmeans=tapply(depth$b_value,depth$EnvTemp,mean)
gpsd=tapply(depth$b_value,depth$EnvTemp,sd)
n=tapply(depth$b_value,depth$EnvTemp,length)
degf=tapply(depth$b_value,depth$EnvTemp,length)-1

barx = barplot(gpmeans,
              names.arg=levels(depth$EnvTemp),
              ylim=c(-20,5), axis.lty=1,
              xlab="Environment", ylab="Depth Shift Rate (m/decade)")
#our own function for putting on error bars
error.bar = function(x, m, upper, lower=upper, length=0.1){
  if(length(x) != length(m) | length(m) !=length(lower) | 
             length(lower) != length(upper))
     stop("vectors must be same length")
  arrows(x,m+upper, x, m-lower, angle=90, code=3, length=length)
}

error.bar(barx, gpmeans,gpsd/sqrt(n))



#Boundary and Environment
stripchart(boundary$b_value~boundary$EnvTemp, vertical=T,pch=20,
           col=c("blue","green", "red", "black"),method="jitter",jitter=0.2,
           ylab="Range shift (km/dec)")


mod7<-aov(b_value~EnvTemp, data=boundary)
summary(mod7)
#No sig dif

#Area occupied and Environment
stripchart(area$b_value~area$EnvTemp, vertical=T,pch=20,
           col=c("blue","green", "red", "black"),method="jitter",jitter=0.2,
           ylab="Area occupied (km2/dec)")


mod8<-aov(b_value~EnvTemp, data=area)
summary(mod8)
#no sig dif
