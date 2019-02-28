##R code for the ecological analysis of our review PAPER FIGURES
##Author: Alba Aguión, Iratxe Rubio
##Date: August 20th 2017


#libraries
library(rockchalk)
library(ggplot2) 
library(tidyverse)
library(tidyr)
library(reshape2) #melt function

#open dataset
table<-read.table("data/biblio_database.csv", header= T, sep= ",")
##Subset of biblio_database without duplicated data
##table1<- table1%>%
##  filter(table1$duplicate=="1")
str(table)

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

#SUBSETS COMBINE
lat<-subset (table, b_impact_combine=="lat shift")
depth<-subset (table, b_impact_combine=="depth shift")
boundary<-subset (table, b_impact_combine== "boundary lat shift")
area<-subset (table, b_impact_combine== "shift in area occupied")
lat_long<-subset (table, b_impact_combine== "lat and long shift")
long<-subset (table, b_impact_combine== "long shift")

#################################################
# Figure 3. CLIMATE CHANGE VARIABLES PER IMPACT #
#################################################
clim_lat <- as.data.frame(table(lat$cc))
clim_depth <- as.data.frame(table(depth$cc))
clim_bound <- as.data.frame(table(boundary$cc))
clim_ar <- as.data.frame(table(area$cc))
#clim_latlon <- as.data.frame(table(lat_long$cc))
#clim_lon <- as.data.frame(table(long$cc))

dat <- cbind(clim_lat, clim_depth[,2], clim_bound[,2], clim_ar[,2])
colnames(dat) <- c("cc","Mean Latitude","Depth","Area","Boundary Latitude")

data1 <- melt(dat)
colnames(data1)[2] <- "impacts"

ggplot(data = data1, aes(x = impacts, y = value, fill = factor(cc))) + 
  geom_bar(stat="identity", alpha=0.8) + scale_fill_manual(values=c("darkgreen", "chartreuse3", "yellow", "orange", "orangered", "red3")) +
  labs( y = "Number of Observations", x = "", fill = "Climate Change variables")+ theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'), legend.position= "left")


#same figure without climate velocity
data2 <- subset (data1,  ! cc == "Climate_Velocity") # same dataset without climate velocity

data2$cc <- factor(data2$cc, levels = c("sst_bt","AMO","sst", "sst_bt_AMO",
                                        "bt"), labels=c("SST and BT", "AMO", "SST", "SST, BT and AMO", "BT"))
data2$impacts <- factor (data2$impacts, levels= c ("Latitude", 
                                                 "Depth","Boundary", "Area"), labels=c("Mean Latitude", "Depth", "Boundary Latitude", "Area"))                                       

ggplot(data = data2, aes(x = impacts, y = value, fill = factor(cc))) + 
  geom_bar(stat="identity", alpha=0.8) + scale_fill_manual(values=c("darkgreen", "chartreuse3", "yellow", "orange", "orangered", "red3")) +
  labs( y = "Number of Observations", x = "", fill = "Climate Change variables")+ theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'), legend.position= "left",axis.text=element_text(size=12),axis.title=element_text(size=12),legend.text=element_text(size=12))



###############################
# Figure 2. IMPACTS MAGNITUDE #
###############################

############
# LATITUDE #
############

lat$sign<-ifelse(lat$b_value>0,"North", "South")# there are no zeros sum(lat$b_value==0)

Fig2.lat<-lat %>% 
  ggplot()+
  geom_histogram(aes(x=b_value, fill=sign, colour=sign), alpha=0.7, binwidth = 9) +
  scale_x_continuous(name="Mean Latitude Shift Rate (Km/decade)") + scale_y_continuous(name="Number of Observations")+
  scale_colour_manual(name="Shift direction", values=c("North" = "skyblue", "South"="royalblue4"), labels=c("North"="North", "South"="South")) +
  scale_fill_manual(name="Shift direction", values=c("North" = "skyblue", "South"="royalblue4"), labels=c("North"="North", "South"="South")) + theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'),legend.position=c(.85, .75))

Fig2.lat


#########
# DEPTH #
#########

depth$sign<-ifelse(depth$b_value>0,"Deeper", "Shallower")# there are no zeros sum(depth$b_value==0)

Fig2.depth<-depth %>%
  ggplot()+
  geom_histogram(aes(x=b_value, fill=sign, colour=sign), alpha=0.7, binwidth = 4) +
  scale_x_continuous(name="Depth Shift Rate (m/decade)") + scale_y_continuous(name="Number of Observations")+
  scale_colour_manual(name="Shift direction", values=c("Deeper" = "skyblue", "Shallower"="royalblue4"), labels=c("Deeper"="Deeper", "Shallower"="Shallower")) +
  scale_fill_manual(name="Shift direction", values=c("Deeper" = "skyblue", "Shallower"="royalblue4"), labels=c("Deeper"="Deeper", "Shallower"="Shallower")) + theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'),legend.position=c(.85, .75))

Fig2.depth

############
# BOUNDARY #
############

boundary$sign<-ifelse(boundary$b_value>0,"North", "South")# there are no zeros sum(boundary.lat$b_value==0)


Fig2.boundary<-boundary %>%
  ggplot()+
  geom_histogram(aes(x=b_value, fill=sign, colour=sign), alpha=0.7, binwidth = 9) +
  scale_x_continuous(name="Boundary Latitude Shift Rate (Km/decade)") + scale_y_continuous(name="Number of Observations")+
  scale_colour_manual(name="Shift direction", values=c("North" = "skyblue", "South"="royalblue4"), labels=c("North"="North", "South"="South")) +
  scale_fill_manual(name="Shift direction", values=c("North" = "skyblue", "South"="royalblue4"), labels=c("North"="North", "South"="South")) + theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'),legend.position=c(.85, .75))

Fig2.boundary


#########################
# Multiple plot function#
#########################

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


multiplot(Fig2.lat, Fig2.depth, Fig2.boundary, cols=3)


#SSMM BOXPLOTS
#LATITUDE
ggplot(lat, aes(x =cc, y=b_value)) +
  geom_boxplot()+
  scale_y_continuous(name = "Mean Latitude Shift Rate (Km/decade)")+
  scale_x_discrete(name = "Climate Change Variables")

#DEPTH
ggplot(depth, aes(x =cc, y=b_value)) +
  geom_boxplot()+
  scale_y_continuous(name = "Depth Shift Rate (m/decade)")+
  scale_x_discrete(name = "Climate Change Variables")

#BOUNDARY LATITUDE
ggplot(boundary, aes(x =cc, y=b_value)) +
  geom_boxplot()+
  scale_y_continuous(name = "Boundary Latitude Shift Rate (Km/decade)")+
  scale_x_discrete(name = "Climate Change Variables")

#AREA
ggplot(area, aes(x =cc, y=b_value)) +
  geom_boxplot()+
  scale_y_continuous(name = "Area Shift Rate (Km2/decade)")+
  scale_x_discrete(name = "Climate Change Variables")
