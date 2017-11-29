##R code for the ecological analysis of our review
## Analysis of climate variables related to the distributional impacts and impacts´magnitudes
##Author: Alba 
##Date: August 20th 2017

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


#######################################
# CLIMATE CHANGE VARIABLES PER IMPACT #
#######################################

#Nicolas is not consider in the analysis

#SUBSETS COMBINE
lat<-subset (table, b_impact_combine=="lat shift")
depth<-subset (table, b_impact_combine=="depth shift")
boundary<-subset (table, b_impact_combine== "boundary lat shift")
area<-subset (table, b_impact_combine== "shift in area occupied")
lat_long<-subset (table, b_impact_combine== "lat and long shift")
long<-subset (table, b_impact_combine== "long shift")


as.data.frame(table(lat$cc))
as.data.frame(table(depth$cc))
as.data.frame(table(boundary$cc))
as.data.frame(table(area$cc))
as.data.frame(table(lat_long$cc))
as.data.frame(table(long$cc))

impacts <- c ("Latitude", "Depth", "Area", "Boundary")
AMO <- c ("7", "4", "7", "5")
Climate_Velocity <- c ("158", "158", "0", "0")
sst <- c ("21", "1", "0", "1" )
sst_bt_AMO <- c ("12", "13","12", "16")
sst_bt <- c ("5", "3","0","1") #For latitude 1+4 (4 from lat_long)
bt <- c ("25", "30", "0", "9")

data<-data.frame(impacts, AMO, Climate_Velocity, sst, sst_bt_AMO, sst_bt, bt)

#Stacked Bar Chart
#http://rstudio-pubs-static.s3.amazonaws.com/3256_bb10db1440724dac8fa40da5e658ada5.html
#Colors: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
#Colors2: https://greggilbertlab.sites.ucsc.edu/wp-content/uploads/sites/276/2015/10/colorbynames.png

setwd("C:/Users/alba.aguion/OneDrive/CLOCK_TEAM/03_FUTURE OCEANS/FO_BIBLIO/FO_DATA_ANALYSIS/Ecological Analysis")
data1<-read.table("Stalked_Bar_Chart_cc_variables.csv", header= T, sep= "," )

data1$cc <- factor(data1$cc, levels = c("sst_bt","AMO","sst", "sst_bt_AMO",
                                        "bt","Climate_Velocity"), labels=c("SST and BT", "AMO", "SST", "SST, BT and AMO", "BT", "Climate Velocity"))

data1$Impact <- factor (data1$Impact, levels= c ("Latitude", 
                                        "Depth","Boundary", "Area"), labels=c("Mean Latitude", "Depth", "Boundary Latitude", "Area"))                                       

ggplot(data = data1, aes(x = Impact, y = Sample.Size, fill = factor(cc))) + 
  geom_bar(stat="identity", alpha=0.8) + scale_fill_manual(values=c("darkgreen", "chartreuse3", "yellow", "orange", "orangered", "red3")) +
  labs( y = "Number of Observations", x = "Distributional Impacts", fill = "Climate Change variables")+ theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'), legend.position= "left")








#####################
# IMPACTS MAGNITUDE #
#####################

#Nicolas et al will not be considered for this analysis

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


#Subsets for b_impact_combined
lat<-subset (table, b_impact_combine=="lat shift")
depth<-subset (table, b_impact_combine=="depth shift")
boundary<-subset (table, b_impact_combine== "boundary lat shift")
area<-subset (table, b_impact_combine== "shift in area occupied")
lat_long<-subset (table, b_impact_combine== "lat and long shift")
long<-subset (table, b_impact_combine== "long shift")


############
# LATITUDE #
############

#BOXPLOT
boxplot.lat<- ggplot(data=lat,aes(x=b_impact_combine,y=b_value))+
  geom_boxplot(fill="grey", color="black")+  
  scale_y_continuous("Shift Rate (Km/decade)")+
  scale_x_discrete()+
  labs(x="Mean Latitude")+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(),legend.title=element_blank())+
  theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black')); boxplot.lat


#GEOM_BAR
#lat$b_value<-round(lat$b_value, digits =0) #needed for geom_bar and type 1 histogram, but not for the type 2 histogram
lat.counts<-subset(lat, select=c(b_value))
lat.counts<-as.data.frame(table(lat.counts$b_value))
str(lat.counts)
lat.counts$Var1<-as.numeric(as.character(lat.counts$Var1))

ggplot(data=lat.counts, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", position=position_dodge())+ theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'))


#HISTOGRAMS
#Type 1
qplot(lat.counts$Var1, geom="histogram",
      main = "", 
      xlab = "Mean Latitude Shift Rate (km/dec)",  
      ylab = "",
      fill=I("black"), 
      col=I("black"), 
      alpha=I(0.75),
      xlim=c(-150,300),
      ylim=c(0,17),breaks=c(-175,-160,-145,-130,-115,-100,-85,-70,-55,-40,-25,-10,5,20,35,50,65,80,95,110,125,140,155,170,185,200,215,230,245,260,275,290))+ theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'))

qplot(lat.counts$Var1, geom="histogram",
      main = "", 
      xlab = "Mean Latitude Shift Rate(km/dec)",  
      ylab = "",
      fill=I("black"), 
      col=I("black"), 
      alpha=I(0.75),
      xlim=c(-150,300),
      ylim=c(0,11),breaks=c(-180,-170,-160,-150,-140,-130,-120,-110,-100,-90,-80,-70,-60,-50,-40,-30,-20,-10,0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280,290,300))+ theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'))

#missing values? maybe breaks that match with b_values - seems better the histogram type 2

#Type 2 - WAY BETTER TYPE 3
lat.counts$group <- ifelse(lat.counts$Var1<=-1, 1, ifelse(lat.counts$Var1<=0,2,3))
ggplot(lat.counts, aes(x=Var1))  +
  geom_histogram(binwidth=9, data=subset(lat.counts,group==1), fill="blue") +
  geom_histogram(binwidth=9, data=subset(lat.counts,group==2), fill="white") +
  geom_histogram(binwidth=9, data=subset(lat.counts,group==3), fill="red") +
  scale_x_continuous(name="Mean Latitude Shift Rate (Km/dec)") +
  scale_y_continuous(name="Count")

#Type 3
lat.counts.pos<-subset(lat.counts, group==3)
lat.counts.neg<-subset(lat.counts, group==1)

#Type 3.1
ggplot()+
geom_histogram(aes(x=Var1, fill="g"), alpha=0.5, data=lat.counts.neg, binwidth = 9) +
geom_histogram(aes(x=Var1, fill="b"), alpha=0.5, data=lat.counts.pos, binwidth = 9) +
scale_x_continuous(name="Mean Latitude Shift Rate (Km/dec)") + scale_y_continuous(name="")+
scale_colour_manual(name="Shift direction", values=c("b" = "deepskyblue", "g"="chartreuse2"), labels=c("b"="North", "g"="South")) +
scale_fill_manual(name="Shift direction", values=c("b" = "deepskyblue", "g"="chartreuse2"), labels=c("b"="North", "g"="South"))+ theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'))

#Type 3.2
a<-ggplot()+
  geom_histogram(aes(x=Var1, fill="g", colour="g"), alpha=0.7, data=lat.counts.neg, binwidth = 9) +
  geom_histogram(aes(x=Var1, fill="b", colour="b"), alpha=0.7, data=lat.counts.pos, binwidth = 9) +
  scale_x_continuous(name="Mean Latitude Shift Rate (Km/decade)") + scale_y_continuous(name="")+
  scale_colour_manual(name="Shift direction", values=c("b" = "red3", "g"="darkgreen"), labels=c("b"="North", "g"="South")) +
  scale_fill_manual(name="Shift direction", values=c("b" = "red3", "g"="darkgreen"), labels=c("b"="North", "g"="South")) + theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'),legend.position=c(.85, .75))
a

# Type 4 : as a percentage 

ggplot()+
  geom_histogram(aes(x=Var1, y=(..count..)/sum(..count..),fill="g", colour="g"), alpha=0.7, data=lat.counts.neg, binwidth = 10) +
  geom_histogram(aes(x=Var1, y=(..count..)/sum(..count..),fill="b", colour="b"), alpha=0.7, data=lat.counts.pos, binwidth = 10) +
  scale_x_continuous(name="Mean Latitude Shift Rate (Km/dec)") + scale_y_continuous(name="Percentage")+
  scale_colour_manual(name="Shift direction", values=c("b" = "skyblue", "g"="royalblue4"), labels=c("b"="North", "g"="South")) +
  scale_fill_manual(name="Shift direction", values=c("b" = "skyblue", "g"="royalblue4"), labels=c("b"="North", "g"="South")) + theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'),legend.position=c(.85, .75))


#########
# DEPTH #
#########

#BOXPLOT
ggplot(data=depth,aes(x=b_impact_combine,y=b_value))+
  geom_boxplot(fill="grey", color="black")+  
  scale_y_continuous("Shift Rate (m/decade)")+
  scale_x_discrete()+
  labs(x="Depth")+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(),legend.title=element_blank())+
  theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'))


#GEOM_BAR
#depth$b_value<-round(depth$b_value, digits = 0)
depth.counts<-subset(depth, select = c(b_value))
depth.counts<-as.data.frame(table(depth.counts$b_value))
str(depth.counts)
depth.counts$Var1<-as.numeric(as.character(depth.counts$Var1))

ggplot(data=depth.counts, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", position=position_dodge())+ theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'))


#HISTOGRAMS
#Type 1
range(depth.counts$Var1)
qplot(depth.counts$Var1, geom="histogram",
                  main = "", 
                  xlab = "Depth Shift Rate (m/dec)",  
                  ylab = "",
                  fill=I("black"), 
                  col=I("black"), 
                  alpha=I(0.75),
                  xlim=c(-100,50),
                  ylim=c(0,6),breaks=c(-110,-105,-100,-95,-90,-85,-80,-75,-70,-65,-60,-55, -50,-45,-40,-35,-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40,45,50,55,60))+ theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'))

#Type 2
depth.counts$group <- ifelse(depth.counts$Var1<=-0, 1, ifelse(depth.counts$Var1<=0,2,3))

depth.counts.pos<-subset(depth.counts, group==3)
depth.counts.neg<-subset(depth.counts, group==1)

ggplot()+
  geom_histogram(aes(x=Var1, fill="r", colour="r"), alpha=.4, data=depth.counts.pos, binwidth = 4) +
  geom_histogram(aes(x=Var1, fill="b",colour="b"), alpha=.4, data=depth.counts.neg, binwidth = 4) +
  scale_x_continuous(name="Depth Shift Rate (m/dec)") + scale_y_continuous(name="")+
  scale_colour_manual(name="Shift direction", values=c("b" = "blue", "r"="red"), labels=c("b"="Deeper", "r"="Shallower")) +
  scale_fill_manual(name="Shift direction", values=c("b" = "blue", "r"="red"), labels=c("b"="Deeper", "r"="Shallower"))

#Type 3

b<-ggplot()+
  geom_histogram(aes(x=Var1, fill="g", colour="g"), alpha=0.7, data=depth.counts.neg, binwidth = 4) +
  geom_histogram(aes(x=Var1, fill="b", colour="b"), alpha=0.7, data=depth.counts.pos, binwidth = 4) +
  scale_x_continuous(name="Depth Shift Rate (m/decade)") + scale_y_continuous(name="")+
  scale_colour_manual(name="Shift direction", values=c("b" = "red3", "g"="darkgreen"), labels=c("b"="Shallower", "g"="Deeper")) +
  scale_fill_manual(name="Shift direction", values=c("b" = "red3", "g"="darkgreen"), labels=c("b"="Shallower", "g"="Deeper")) + theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'),legend.position=c(.85, .75))
b

############
# BOUNDARY #
############

#BOXPLOT
box.plot.boundary<-ggplot(data=boundary,aes(x=b_impact_combine,y=b_value))+
  geom_boxplot(fill="grey", color="black")+  
  scale_y_continuous("Shift Rate (km/decade)")+
  scale_x_discrete()+
  labs(x="Boundary Latitude")+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(),legend.title=element_blank())+
  theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black')); box.plot.boundary


#GEOM_BAR
#boundary$b_value<-round(boundary$b_value, digits = 0)
boundary.counts<-subset(boundary, select=c(b_value))
boundary.counts<-as.data.frame(table(boundary.counts$b_value))
boundary.counts$Var1<-as.numeric(as.character(boundary.counts$Var1))

ggplot(data=boundary.counts, aes(x=Var1, y=Freq)) + scale_y_continuous(limits = c (0,3)) +
  geom_bar(stat="identity", position=position_dodge())+ theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'))


#HISTOGRAMS
#Type 1
range(boundary.counts$Var1)

qplot(boundary.counts$Var1, geom="histogram",
                    main = "", 
                    xlab = "Boundary Latitude Shift Rate (km/dec)",  
                    ylab = "",
                    fill=I("black"), 
                    col=I("black"), 
                    alpha=I(0.75),
                    xlim=c(-100,100),
                    ylim=c(0,6),breaks=c(-110,-100,-90,-80,-70,-60,-50,-40,-30,-20,-10,0,10,20,30,40,50,60,70,80,90,100))+ theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'))
#Type 2
boundary.counts$group <- ifelse(boundary.counts$Var1<=-0, 1, ifelse(boundary.counts$Var1<=0,2,3))

boundary.counts.pos<-subset(boundary.counts, group==3)
boundary.counts.neg<-subset(boundary.counts, group==1)

ggplot()+
  geom_histogram(aes(x=Var1, fill="r",colour="r"), alpha=.4, data=boundary.counts.neg, binwidth = 9) +
  geom_histogram(aes(x=Var1, fill="b", colour="b"), alpha=.4, data=boundary.counts.pos, binwidth = 9) +
  scale_x_continuous(name="Boundary Latitude Shift Rate (Km/dec)") + scale_y_continuous(name="")+
  scale_colour_manual(name="Shift direction", values=c("b" = "blue", "r"="red"), labels=c("b"="North", "r"="South")) +
  scale_fill_manual(name="Shift direction", values=c("b" = "blue", "r"="red"), labels=c("b"="North", "r"="South"))

#Type 3
c<-ggplot()+
  geom_histogram(aes(x=Var1, fill="g", colour="g"), alpha=0.7, data=boundary.counts.neg, binwidth = 9) +
  geom_histogram(aes(x=Var1, fill="b", colour="b"), alpha=0.7, data=boundary.counts.pos, binwidth = 9) +
  scale_x_continuous(name="Boundary Latitude Shift Rate (Km/decade)") + scale_y_continuous(name="")+
  scale_colour_manual(name="Shift direction", values=c("b" = "red3", "g"="darkgreen"), labels=c("b"="North", "g"="South")) +
  scale_fill_manual(name="Shift direction", values=c("b" = "red3", "g"="darkgreen"), labels=c("b"="North", "g"="South")) + theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'),legend.position=c(.85, .75))



########
# AREA #
########

#BOXPLOT
ggplot(data=area,aes(x=b_impact_combine,y=b_value))+
  geom_boxplot(fill="grey", color="black")+  
  scale_y_continuous("Shift Rate (km2/decade)")+
  scale_x_discrete()+
  labs(x="Area")+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(),legend.title=element_blank())+
  theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'))

#VALUES SEEM TO HIGH - all Nye et al.

#GEOM_BAR
#area$b_value<-round(area$b_value, digits = 0)
area.counts<-subset(area, select=c(b_value))
area.counts<-as.data.frame(table(area.counts$b_value))
area.counts$Var1<-as.numeric(as.character(area.counts$Var1))

ggplot(data=area.counts, aes(x=Var1, y=Freq)) + scale_y_continuous(limits = c (0,3)) +
  geom_bar(stat="identity", position=position_dodge())+ theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'))


#HISTOGRAMS
#Type 1
range(area.counts$Var1)
area.hist<- qplot(area.counts$Var1, geom="histogram",
                      main = "", 
                      xlab = "Area Shift Rate (km2/dec)",  
                      ylab = "",
                      fill=I("black"), 
                      col=I("black"), 
                      alpha=I(0.75)) + theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'))
area.hist

#Type 2
area.counts$group <- ifelse(area.counts$Var1<=-0, 1, ifelse(area.counts$Var1<=0,2,3))

area.counts.pos<-subset(area.counts, group==3)
area.counts.neg<-subset(area.counts, group==1)

ggplot()+
  geom_histogram(aes(x=Var1, fill="r", colour="r"), alpha=.4, data=area.counts.pos, binwidth = 19) +
  geom_histogram(aes(x=Var1, fill="b",colour="b"), alpha=.4, data=area.counts.neg, binwidth = 19) +
  scale_x_continuous(name="Area Shift Rate (Km2/dec)") + scale_y_continuous(name="")+
  scale_colour_manual(name="Shift direction", values=c("b" = "blue", "r"="red"), labels=c("b"="Contraction", "r"="Expansion")) +
  scale_fill_manual(name="Shift direction", values=c("b" = "blue", "r"="red"), labels=c("b"="Contraction", "r"="Expansion"))

#Type 3

d<-ggplot()+
  geom_histogram(aes(x=Var1, fill="g", colour="g"), alpha=0.7, data=area.counts.neg, binwidth = 80) +
  geom_histogram(aes(x=Var1, fill="b", colour="b"), alpha=0.7, data=area.counts.pos, binwidth = 80) +
  scale_x_continuous(name="Area Shift Rate (Km2/decade)") + scale_y_continuous(name="",breaks = seq(0, 3, 1))+
  scale_colour_manual(name="Shift direction", values=c("b" = "red3", "g"="darkgreen"), labels=c("b"="Expansion", "g"="Contraction")) +
  scale_fill_manual(name="Shift direction", values=c("b" = "red3", "g"="darkgreen"), labels=c("b"="Expansion", "g"="Contraction")) + theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'),legend.position=c(.85, .75))






#########################
# Multiple plot function#
#########################



# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


multiplot(a,b,c,d, cols=2)

