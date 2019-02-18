##R code for the ecological analysis of our review:TO MERGE OR NOT CENTER OF BIOMASS AND CENTER OF GRAVITY (=DISTRIBUTION)
##no sig differences
##Author: Alba and Iratxe
##Date: June 8th 2017

#libraries
library(rockchalk)
library(ggplot2)

#open dataset
table<-read.table("data/biblio_database.csv", header= T, sep= ",")
##Subset of biblio_database without duplicated data
##table<- table%>%
##  filter(table$duplicate=="1")

#Factors
table$researcher<-as.factor(table$researcher)
table$id_study<-as.factor(table$id_study)
table$cc<-as.factor(table$cc)
table$b_impact<-as.factor(table$b_impact)
table$b_direction<-as.factor(table$b_direction)

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


##########################
#  Graphical Exploration #
##########################

# RUN UNTIL "MULTIPLOT"

table$b_impact <- factor(table$b_impact,labels = c("lat shift center of grav", 
                                                   "lat shift center of bio", 
                                                   "depth shift", 
                                                   "boundary lat shift center of grav", 
                                                   "boundary lat shift center of bio", 
                                                   "long shift center of grav", 
                                                   "long shift center of bio", 
                                                   "lat and long shift center of grav", 
                                                   "shift in area occupied"))

###b_impact for latitudes and longitudes
lat_lon<-table[table$b_impact %in% c("lat shift center of grav",
                                     "lat shift center of bio",
                                     "boundary lat shift center of grav",
                                     "boundary lat shift center of bio",
                                     "long shift center of grav", 
                                     "long shift center of bio", 
                                     "lat and long shift center of grav"),]

l<-as.data.frame(table(lat_lon$b_impact))
l<-rbind(list(l[1:2,],l[4:8,]))

plot_latlon<-ggplot(lat_lon, aes(x= lat_lon$b_impact , y = lat_lon$b_value)) +
  scale_x_discrete(name = NULL, 
                   labels=paste(c("lat shift center \n of grav",   
                                  "lat shift center \n of bio",
                                  "boundary lat \n shift center \n of grav",
                                  "boundary lat \n shift center \n of bio",
                                  "long shift \n center of grav", 
                                  "long shift \n center of bio", 
                                  "lat and long shift \n center of grav"),"\n(n=",l$Freq,")",sep="")) +
  scale_y_continuous(name = "Latitude/longitude shift (Km/decade)")  +
  theme(axis.text.x=element_text(colour="black", size = 8)) +
  geom_boxplot()

###b_impact for depths
depth<-table[table$b_impact %in% c("depth shift"),]

d<-as.data.frame(table(depth$b_impact))
d<-d[3,]

plot_depth<-ggplot(depth, aes(x= depth$b_impact , y = depth$b_value)) +
  scale_x_discrete(name = NULL, 
                   labels=paste(c("depth shift"),"\n(n=",d$Freq,")",sep="")) +
  scale_y_continuous(name = "Depth shift (m/decade)")  +
  theme(axis.text.x=element_text(colour="black", size = 8)) +
  geom_boxplot() 

###b_impact for areas
area<-table[table$b_impact %in% c("shift in area occupied"),]

a<-as.data.frame(table(area$b_impact))
a<-a[9,]

plot_area<-ggplot(area, aes(x= area$b_impact , y = area$b_value)) +
  scale_x_discrete(name = NULL, 
                   labels=paste(c("Area shift"),"\n(n=",a$Freq,")",sep="")) +
  scale_y_continuous(name = "Area shift (km2/decade)")  +
  theme(axis.text.x=element_text(colour="black", size = 8)) +
  geom_boxplot() 

source("http://peterhaschke.com/Code/multiplot.R")
multiplot(plot_latlon, plot_depth, plot_area, cols=3)


##############
# Statistics #
##############
#depth and area are all observations from center of biomass

#to date boundary latitude only 1 obs for biomass so no suitable for a statistical test

#lat+long, long not done

table<-read.table("data/biblio_database.csv", header= T, sep= ",")
table<-table[,1:69] #get rid of empty columns

#FOR LATITUDE: 
table$b_impact<-as.factor(table$b_impact)
table$b_years<-as.numeric(as.character(table$b_years))

latitude1<-subset(table, b_impact=="1", select=c(b_value,b_impact)) #center of grav
latitude2<-subset(table, b_impact=="2", select=c(b_value,b_impact)) #center of bio
latitude<-rbind(latitude1,latitude2)
latitude$b_impact<-factor(latitude$b_impact)

tapply(latitude$b_value,latitude$b_impact,mean)
tapply(latitude$b_value,latitude$b_impact,var)

boxplot(latitude$b_value~latitude$b_impact)

#Normality and Homocedasticity
hist(latitude$b_value) #Kinda of N distribution
hist(log(latitude$b_value)) #More N distribution

tapply(latitude$b_value,latitude$b_impact, shapiro.test) # NO NORMAL DISTRI
bartlett.test(latitude$b_value,latitude$b_impact) #HOMOCEDASTICITY 

boxplot(latitude$b_value~latitude$b_impact)
boxplot(log(latitude$b_value+1)~latitude$b_impact)

#NO PARAMETRIC TEST - not very strong test
kruskal.test(b_value~b_impact, data=latitude) # < 0.05

#RUNNING ANOVAS NO TRANS AND LOG TRANS
m1<-aov(latitude$b_value~latitude$b_impact)
b_value_transformed <- log(102 + latitude$b_value) #get rid of negative values we add a constant

m2<-aov(b_value_transformed~latitude$b_impact)

summary(m1)
summary(m2) #No significant differences. OK to combine them
