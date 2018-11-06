##R code for MAIN figures new for first paper draft
##Author: ELena Ojea
##Date: November 6th 2018


library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(grid)
library(cowplot)

##1## Figure ecological###########################################################################
#Open Biblio_database with SAU data on EEZ and FE:

Sys.setlocale("LC_ALL", "C") #if not opening the database on mac
#open dataset
table1<-read.table("data/biblio_database.csv", header= T, sep= ",")
table<-table1[,1:69]    #delete columns 
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

###############################
# Figure 2. IMPACTS MAGNITUDE #
###############################

############
# LATITUDE #
############

#histogram
lat$sign<-ifelse(lat$b_value>0,"North", "South")# there are no zeros sum(lat$b_value==0)

Fig2.lat<-lat %>% 
  ggplot()+
  geom_histogram(aes(x=b_value, fill=sign, colour=sign), alpha=0.7, binwidth = 9) +
  scale_x_continuous(name="Mean Latitude Shift Rate (Km/decade)") + scale_y_continuous(name="Number of Observations")+
  scale_colour_manual(name="Shift direction", values=c("North" = "skyblue", "South"="royalblue4"), labels=c("North"="North", "South"="South")) +
  scale_fill_manual(name="Shift direction", values=c("North" = "skyblue", "South"="royalblue4"), labels=c("North"="North", "South"="South")) + theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'),legend.position=c(.85, .75))+
  coord_flip()+
  theme_bw()
Fig2.lat

#taxa barplot

lat$tax_group <- as.factor(lat$tax_group)

lat.barplot <- ggplot(lat, aes(tax_group, b_value, fill = tax_group, label=NULL)) +
              geom_boxplot(data=subset(lat, b_value>=0), aes(tax_group, b_value),  na.rm=TRUE, outlier.shape = 1, outlier.size = 0.1)+
              geom_boxplot(data=subset(lat, b_value<0), aes(tax_group, b_value),  na.rm=TRUE, outlier.shape = 1, outlier.size = 0.1)+
              geom_hline(yintercept=c(0), linetype="dotted")+
              scale_y_continuous(name ="km/decade", breaks = seq(-100, 200, by = 50)) +
              xlab(NULL)+
              theme(axis.text.x =element_blank())+
              scale_fill_brewer(palette="Blues")+
              theme_bw()
  

#join_lat.jpeg
 join.lat <- plot_grid(lat.barplot, Fig2.lat, labels = c("A", "B"), align="h")
#ggsave("join_lat.jpeg")

#########
# DEPTH #
#########

depth$sign<-ifelse(depth$b_value>0,"Deeper", "Shallower")# there are no zeros sum(depth$b_value==0)

Fig2.depth<-depth %>%
  ggplot()+
  geom_histogram(aes(x=b_value, fill=sign, colour=sign), alpha=0.7, binwidth = 4) +
  scale_x_continuous(name="Depth Shift Rate (m/decade)") + scale_y_continuous(name="Number of Observations")+
  scale_colour_manual(name="Shift direction", values=c("Deeper" = "skyblue", "Shallower"="royalblue4"), labels=c("Deeper"="Deeper", "Shallower"="Shallower")) +
  scale_fill_manual(name="Shift direction", values=c("Deeper" = "skyblue", "Shallower"="royalblue4"), labels=c("Deeper"="Deeper", "Shallower"="Shallower")) + theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'),legend.position=c(.85, .75))+
  coord_flip()+
  theme_bw()
Fig2.depth

#taxa barplot depth

depth$tax_group <- as.factor(depth$tax_group)

depth.barplot <- ggplot(depth, aes(tax_group, b_value, fill = tax_group, label=NULL)) +
  geom_boxplot(data=subset(depth, b_value>=0), aes(tax_group, b_value),  na.rm=TRUE, outlier.shape = 1, outlier.size = 0.1)+
  geom_boxplot(data=subset(depth, b_value<0), aes(tax_group, b_value),  na.rm=TRUE, outlier.shape = 1, outlier.size = 0.1)+
  geom_hline(yintercept=c(0), linetype="dotted")+
  scale_y_continuous(name ="m/decade", breaks = seq(-80, 60, by = 20)) +
  xlab(NULL)+
  theme(axis.text.x =element_blank())+
  scale_fill_brewer(palette="Blues")+
  theme_bw()


#join_depth.jpeg
join.depth <- plot_grid(depth.barplot, Fig2.depth, labels = c("A", "B"), align="h")
#ggsave("join_depth.jpeg")

#join_lat_depth
join.lat.depth <- plot_grid(lat.barplot, Fig2.lat, depth.barplot, Fig2.depth, labels = c("A", "B", "C", "D"), align="hv")
#ggsave("join_lat_depth.jpeg")



############
# BOUNDARY #
############

boundary$sign<-ifelse(boundary$b_value>0,"North", "South")# there are no zeros sum(boundary.lat$b_value==0)

Fig2.boundary<-boundary %>%
  ggplot()+
  geom_histogram(aes(x=b_value, fill=sign, colour=sign), alpha=0.7, binwidth = 9) +
  scale_x_continuous(name="Boundary Latitude Shift Rate (Km/decade)") + scale_y_continuous(name="Number of Observations")+
  scale_colour_manual(name="Shift direction", values=c("North" = "skyblue", "South"="royalblue4"), labels=c("North"="North", "South"="South")) +
  scale_fill_manual(name="Shift direction", values=c("North" = "skyblue", "South"="royalblue4"), labels=c("North"="North", "South"="South")) + theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'),legend.position=c(.85, .75))+
  coord_flip()+
  theme_bw()
Fig2.boundary


#taxa barplot depth

boundary$tax_group <- as.factor(boundary$tax_group)

boundary.barplot <- ggplot(boundary, aes(tax_group, b_value, fill = tax_group, label=NULL)) +
  geom_boxplot(data=subset(boundary, b_value>=0), aes(tax_group, b_value),  na.rm=TRUE, outlier.shape = 1, outlier.size = 0.1)+
  geom_boxplot(data=subset(boundary, b_value<0), aes(tax_group, b_value),  na.rm=TRUE, outlier.shape = 1, outlier.size = 0.1)+
  geom_hline(yintercept=c(0), linetype="dotted")+
  scale_y_continuous(name ="km/decade", breaks = seq(-100, 125, by = 25)) +
  xlab(NULL)+
  theme(axis.text.x =element_blank())+
  scale_fill_brewer(palette="Blues")+
  theme_bw()
