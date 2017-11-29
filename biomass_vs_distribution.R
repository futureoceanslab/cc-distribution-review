##R code for the ecological analysis of our review
## TO MERGE OR NOT TO MERGE CENTER OF BIOMASS AND CENTER OF DISTRIBUTION?
##Author: Alba and Iratxe
##Date: June 8th 2017

#libraries
library(rockchalk)
library(ggplot2)
library(dplyr)
library(Hmisc)

#setwd("~/OneDrive/CLOCK_TEAM/03_FUTURE OCEANS/FO_BIBLIO") 
setwd("C:/Users/alba.aguion/OneDrive/CLOCK_TEAM/03_FUTURE OCEANS/FO_BIBLIO")

#open dataset
table1<-read.table("biblio_database.csv", header= T, sep= ",")
table<-table1[,1:68]    #delete columns 
str(table)

#TO REMOVE NICOLAS ET AL. - values too different from the rest of the observations
table<-subset(table, ! id_study=="6") 

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

#Subsets for later exploration
lat_cg<-subset (table, b_impact=="lat shift center of grav")
boundary_cg<-subset (table, b_impact== "boundary lat shift center of grav")
long_cg<-subset (table, b_impact== "long shift center of grav")
#depth_cg = depth (all observations for depth are center of gravity, none is center of biomass)
#area_cg = area (no differentiation between center of grav and center of bio)
#lat_long_cg = lat_long (all observations for lat and long are center of gravity, none is center of biomass)
lat_bio<-subset (table, b_impact=="lat shift center of bio")
boundary_bio<-subset (table, b_impact== "boundary lat shift center of bio")
long_bio<-subset (table, b_impact== "long shift center of bio")




####################################################
#  Graphical and Statistical Exploration           #
#  Merging centre of gravity and centre of biomass #
####################################################

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
l<-bind_rows(list(l[1:2,],l[4:8,]))

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



#Statistical Analysis
#depth and area are all observations from center of biomass
#to date boundary latitude and longitude only 1 obs so no suitable for a statistical test

#FOR LATITUDE: 
table1<-read.table("biblio_database.csv", header= T, sep= ",")
table<-table1[,1:68]    #delete empty columns 
#TO REMOVE NICOLAS ET AL. - values too different from the rest of the observations
table<-subset(table, ! id_study=="6") 


table$b_impact<-as.factor(table$b_impact)
table$b_years<-as.numeric(as.character(table$b_years))

latitude1<-subset(table, b_impact=="1", select=c(b_value,b_impact))
latitude2<-subset(table, b_impact=="2", select=c(b_value,b_impact))
latitude<-rbind(latitude1,latitude2)

#write.csv(latitude, file="latitude.csv")

tapply(latitude$b_value,latitude$b_impact,mean)
tapply(latitude$b_value,latitude$b_impact,var)

#Normality and Homocedasticity
hist(latitude$b_value) #NO NORMAL DISTRIBUTION
hist(log(latitude$b_value)) # MORE NORMAL DISTRIBUTION?

tapply(latitude$b_value,latitude$b_impact, shapiro.test) # NO NORMAL DISTRI
bartlett.test(latitude$b_value,latitude$b_impact) #HOMOCEDASTICITY WHEN NICOLAS REMOVED

boxplot(latitude$b_value~latitude$b_impact)
boxplot(log(latitude$b_value+1)~latitude$b_impact)

#NO PARAMETRIC TEST - not very strong test
kruskal.test(b_value~b_impact, data=latitude) 

#RUNNING ANOVAS NO TRANS AND LOG TRANS
m1<-aov(latitude$b_value~latitude$b_impact)
#get rid of negative values we add a constant
b_value_transformed <- log(102 + latitude$b_value)
m2<-aov(b_value_transformed~latitude$b_impact)
        
summary(m1)
summary(m2)



#ALL TESTS INDICATE SIG DIFFERENCES CENTER OF GRAV vs. CENTER OF BIOMASS
#NO SIG WHEN REMOVING NICOLAS ET AL (study 6)



##############
# Analysis 1 #
##############


################################################
#center of grav combined with center of biomass#
################################################


#Subsets for b_impact_combined
lat<-subset (table, b_impact_combine=="lat shift")
depth<-subset (table, b_impact_combine=="depth shift")
boundary<-subset (table, b_impact_combine== "boundary lat shift")
area<-subset (table, b_impact_combine== "shift in area occupied")
lat_long<-subset (table, b_impact_combine== "lat and long shift")
long<-subset (table, b_impact_combine== "long shift")

#Latitude 
#Latitude depending on cc factor

as.data.frame(table(lat$cc))

ggplot(data=lat,aes(x=cc,y=b_value))+
  geom_boxplot(aes(fill=factor(lat$cc)))+  
  scale_y_continuous("Latitude shift (Km/decade)")+
  scale_x_discrete()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(),legend.title=element_blank())+
  scale_fill_manual(values=c("gray83", "cornsilk3", "skyblue4", "seagreen4","red4", "yellow3"))


#Latitude depending on site

as.data.frame(table(lat$site))

ggplot(data=lat,aes(x=site,y=b_value))+
  geom_boxplot(aes(fill=factor(lat$site)))+
  scale_y_continuous("Latitude shift (Km/decade)")+
  scale_x_discrete()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(),legend.title=element_blank())+
  scale_fill_manual(values=c("gray83", "cornsilk3", "skyblue4", "seagreen4","red4", "yellow3", "maroon", "deepskyblue", "tomato1", "pink2", "cyan", "gold", "plum", "snow3", "tan"))


#Depth 
#Depth depending on cc factor

as.data.frame(table(depth$cc))

ggplot(data=depth,aes(x=cc,y=b_value))+
  geom_boxplot(aes(fill=factor(depth$cc)))+
  scale_y_continuous("Depth (m/decade)")+
  scale_x_discrete(NULL)+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(),legend.title=element_blank())+
  scale_fill_manual(values=c("gray83", "cornsilk3", "skyblue4", "seagreen4","red4", "yellow3"))
                

#Depth depending on site

as.data.frame(table(depth$site))

ggplot(data=depth,aes(x=site,y=b_value))+
  geom_boxplot(aes(fill=factor(depth$site)))+
  scale_y_continuous("Depth shift (m/decade)")+
  scale_x_discrete()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())+ 
  scale_fill_manual(values=c("gray83", "cornsilk3", "skyblue4", "seagreen4","red4", "yellow3", "maroon", "deepskyblue", "tomato1", "pink2", "cyan", "gold", "plum", "snow3", "tan"))



#Boundary latitude 
#Boundary latitude depending on cc factor

as.data.frame(table(boundary$cc))

ggplot(data=boundary,aes(x=cc,y=b_value))+
  geom_boxplot(aes(fill=factor(boundary$cc)))+
  scale_y_continuous("Boundary latitude shift (Km/decade)")+
  scale_x_discrete(NULL)+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(),legend.title=element_blank())+
  scale_fill_manual(values=c("gray83", "cornsilk3", "skyblue4", "seagreen4","red4", "yellow3"))
                    


#Boundary latitude depending on site

ggplot(data=boundary,aes(x=site,y=b_value))+
  geom_boxplot(aes(fill=factor(boundary$site)))+
  scale_y_continuous("Boundary Latitude shift (Km/decade)")+
  scale_x_discrete()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), legend.title=element_blank())+ 
  scale_fill_manual(values=c("gray83", "cornsilk3", "skyblue4", "seagreen4","red4"))


#Area 
#Area depending on cc factor

as.data.frame(table(area$cc))

ggplot(data=area,aes(x=cc,y=b_value))+
  geom_boxplot(aes(fill=factor(area$cc)))+  
  scale_y_continuous("Shift in area occupied (Km2/decade)")+
  scale_x_discrete()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(),legend.title=element_blank())+
  scale_fill_manual(values=c("gray83", "cornsilk3", "skyblue4"))


#Area depending on site

ggplot(data=area,aes(x=site,y=b_value))+
  geom_boxplot(aes(fill=factor(area$site)))+
  scale_y_continuous("Shift in area occupied (Km2/decade)")+
  scale_x_discrete()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(),legend.title=element_blank())+
  scale_fill_manual(values=c("gray83"))

#Lat and long 
#Latitude and Longitude depending on cc factor

as.data.frame(table(lat_long$cc))

ggplot(data=lat_long,aes(x=cc,y=b_value))+
  geom_boxplot(aes(fill=factor(lat_long$cc)))+  
  scale_y_continuous("Latitude and Longitude shift (Km/decade)")+
  scale_x_discrete()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(),legend.title=element_blank())+
  scale_fill_manual(values=c("gray83"))


#Latitude and Longitude depending on site
ggplot(data=lat_long,aes(x=site,y=b_value))+
  geom_boxplot(aes(fill=factor(lat_long$site)))+
  scale_y_continuous("Latitude and Longitude shift (Km/decade)")+
  scale_x_discrete()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(),legend.title=element_blank())+
  scale_fill_manual(values=c("gray83", "cornsilk3"))

#Longitude
#Long depending on cc factor

as.data.frame(table(long$cc))

ggplot(data=long,aes(x=cc,y=b_value))+
  geom_boxplot(aes(fill=factor(long$cc)))+  
  scale_y_continuous("Longitude shift (Km/decade)")+
  scale_x_discrete()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(),legend.title=element_blank())+
  scale_fill_manual(values=c("gray83", "cornsilk3", "skyblue4", "seagreen4","red4"))


#Long depending on site

ggplot(data=long,aes(x=site,y=b_value))+
  geom_boxplot(aes(fill=factor(long$site)))+
  scale_y_continuous("Longitude shift (Km/decade)")+
  scale_x_discrete()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(),legend.title=element_blank())+
  scale_fill_manual(values=c("gray83", "cornsilk3", "skyblue4"))







##############
# Analysis 2 #
##############


#################
#center of grav #
#################

#Latitude table - center of grav
#Latitude -center of grav- depending on cc factor
ggplot(data=lat_cg,aes(x=cc,y=b_value))+
  geom_boxplot(aes(fill=factor(lat_cg$cc)))+  
  scale_y_continuous("Latitude shift of the center of gravity (Km/decade)")+
  scale_x_discrete()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(),legend.title=element_blank())+
  scale_fill_manual(values=c("gray83", "cornsilk3", "skyblue4", "seagreen4","red4", "yellow3", "maroon", "deepskyblue", "tomato1", "pink2", "cyan", "gold", "plum", "snow3", "tan"))


#Latitude -center of grav- depending on site
ggplot(data=lat_cg,aes(x=site,y=b_value))+
  geom_boxplot(aes(fill=factor(lat_cg$site)))+
  scale_y_continuous("Latitude shift of the center of gravity(Km/decade)")+
  scale_x_discrete()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(),legend.title=element_blank())+
  scale_fill_manual(values=c("gray83", "cornsilk3", "skyblue4", "seagreen4","red4", "yellow3", "maroon", "deepskyblue", "tomato1", "pink2", "cyan", "gold", "plum", "snow3", "tan"))

#Boundary latitude center of gravity table
#Boundary latitude -center of grav- depending on cc factor
ggplot(data=boundary_cg,aes(x=cc,y=b_value))+
  geom_boxplot(aes(fill=factor(boundary_cg$cc)))+
  scale_y_continuous("Boundary latitude shift of the center of gravity (Km/decade)")+
  scale_x_discrete(NULL)+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(),legend.title=element_blank())+
  scale_fill_manual(values=c("gray83", "cornsilk3", "skyblue4", "seagreen4","red4", "yellow3", "maroon", "deepskyblue", "tomato1", "pink2", "cyan", "gold", "plum", "snow3", "tan"))


#Boundary latitude -center of grav- depending on site
ggplot(data=boundary_cg,aes(x=site,y=b_value))+
  geom_boxplot(aes(fill=factor(boundary_cg$site)))+
  scale_y_continuous("Boundary Latitude shift of the center of gravity (Km/decade)")+
  scale_x_discrete()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), legend.title=element_blank())+ 
  scale_fill_manual(values=c("gray83", "cornsilk3", "skyblue4", "seagreen4","red4", "yellow3", "maroon", "deepskyblue", "tomato1", "pink2", "cyan", "gold", "plum", "snow3", "tan"))

#Longitude table - center of grav
#Long -center of grav- depending on cc factor
ggplot(data=long_cg,aes(x=cc,y=b_value))+
  geom_boxplot(aes(fill=factor(long_cg$cc)))+  
  scale_y_continuous("Longitude shift center of gravity (Km/decade)")+
  scale_x_discrete()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(),legend.title=element_blank())+
  scale_fill_manual(values=c("gray83", "cornsilk3", "skyblue4", "seagreen4","red4", "yellow3", "maroon", "deepskyblue", "tomato1", "pink2", "cyan", "gold", "plum", "snow3", "tan"))


#Long -center of grav- depending on site
ggplot(data=long_cg,aes(x=site,y=b_value))+
  geom_boxplot(aes(fill=factor(long_cg$site)))+
  scale_y_continuous("Longitude shift center of gravity (Km/decade)")+
  scale_x_discrete()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(),legend.title=element_blank())+
  scale_fill_manual(values=c("gray83", "cornsilk3", "skyblue4", "seagreen4","red4", "yellow3", "maroon", "deepskyblue", "tomato1", "pink2", "cyan", "gold", "plum", "snow3", "tan"))





##############
# Analysis 3 #
##############


####################
#center of biomass #
####################


#Latitude table - center of bio
#Latitude -center of bio- depending on cc factor
ggplot(data=lat_bio,aes(x=cc,y=b_value))+
  geom_boxplot(aes(fill=factor(lat_bio$cc)))+  
  scale_y_continuous("Latitude shift of the center of biomass (Km/decade)")+
  scale_x_discrete()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(),legend.title=element_blank())+
  scale_fill_manual(values=c("gray83", "cornsilk3", "skyblue4", "seagreen4","red4", "yellow3", "maroon", "deepskyblue", "tomato1", "pink2", "cyan", "gold", "plum", "snow3", "tan"))


#Latitude -center of bio- depending on site
ggplot(data=lat_bio,aes(x=site,y=b_value))+
  geom_boxplot(aes(fill=factor(lat_bio$site)))+
  scale_y_continuous("Latitude shift of the center of biomass(Km/decade)")+
  scale_x_discrete()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(),legend.title=element_blank())+
  scale_fill_manual(values=c("gray83", "cornsilk3", "skyblue4", "seagreen4","red4", "yellow3", "maroon", "deepskyblue", "tomato1", "pink2", "cyan", "gold", "plum", "snow3", "tan"))

#Boundary latitude center of biomass table
#Boundary latitude -center of bio- depending on cc factor
ggplot(data=boundary_bio,aes(x=cc,y=b_value))+
  geom_boxplot(aes(fill=factor(boundary_bio$cc)))+
  scale_y_continuous("Boundary latitude shift of the center of biomass (Km/decade)")+
  scale_x_discrete(NULL)+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(),legend.title=element_blank())+
  scale_fill_manual(values=c("gray83", "cornsilk3", "skyblue4", "seagreen4","red4", "yellow3", "maroon", "deepskyblue", "tomato1", "pink2", "cyan", "gold", "plum", "snow3", "tan"))


#Boundary latitude -center of bio- depending on site
ggplot(data=boundary_bio,aes(x=site,y=b_value))+
  geom_boxplot(aes(fill=factor(boundary_bio$site)))+
  scale_y_continuous("Boundary Latitude shift of the center of biomass (Km/decade)")+
  scale_x_discrete()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), legend.title=element_blank())+ 
  scale_fill_manual(values=c("gray83", "cornsilk3", "skyblue4", "seagreen4","red4", "yellow3", "maroon", "deepskyblue", "tomato1", "pink2", "cyan", "gold", "plum", "snow3", "tan"))

#Longitude table - center of bio
#Long -center of bio- depending on cc factor
ggplot(data=long_bio,aes(x=cc,y=b_value))+
  geom_boxplot(aes(fill=factor(long_bio$cc)))+  
  scale_y_continuous("Longitude shift center of biomass (Km/decade)")+
  scale_x_discrete()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(),legend.title=element_blank())+
  scale_fill_manual(values=c("gray83", "cornsilk3", "skyblue4", "seagreen4","red4", "yellow3", "maroon", "deepskyblue", "tomato1", "pink2", "cyan", "gold", "plum", "snow3", "tan"))


#Long -center of bio- depending on site
ggplot(data=long_bio,aes(x=site,y=b_value))+
  geom_boxplot(aes(fill=factor(long_bio$site)))+
  scale_y_continuous("Longitude shift center of biomass (Km/decade)")+
  scale_x_discrete()+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(),legend.title=element_blank())+
  scale_fill_manual(values=c("gray83", "cornsilk3", "skyblue4", "seagreen4","red4", "yellow3", "maroon", "deepskyblue", "tomato1", "pink2", "cyan", "gold", "plum", "snow3", "tan"))

