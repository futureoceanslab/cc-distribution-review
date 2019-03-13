##R code for the ecological analysis of our review PAPER FIGURES
##Author: Alba Aguión, Iratxe Rubio
##Date: August 20th 2017


#libraries
library(rockchalk)
library(ggplot2) 
library(tidyverse)
library(tidyr)
library(reshape2) #melt function
library(cowplot)

#open dataset
table <- read.csv("data/biblio_database2.csv")

#Factors
table$researcher <- as.factor(table$researcher)
table$id_study <- as.factor(table$id_study)
table$cc <- as.factor(table$cc)
table$b_impact <- as.factor(table$b_impact)
table$b_direction <- as.factor(table$b_direction)

#Numeric
table$b_value <- as.numeric(as.character(table$b_value), na.omit=TRUE)
table$b_years <- as.numeric(as.character(table$b_years))

#SUBSETS COMBINE
lat <- subset (table, b_impact_combine=="lat shift")
depth <- subset (table, b_impact_combine=="depth shift")
boundary <- subset (table, b_impact_combine== "boundary lat shift")
area <- subset (table, b_impact_combine== "shift in area occupied")
lat_long <- subset (table, b_impact_combine== "lat and long shift")
long <- subset (table, b_impact_combine== "long shift")

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
  geom_bar(stat="identity", alpha=0.8) + scale_fill_manual(values=c("darkgreen", "chartreuse3", "yellow", "orange", "orangered", "red3", "blue", "pink")) +
  labs( y = "Number of Observations", x = "", fill = "Climate Change variables")+ theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'), legend.position= "left")


#same figure without climate velocity
data2 <- subset (data1,  ! cc == "climate velocity") # same dataset without climate velocity

ggplot(data = data2, aes(x = impacts, y = value, fill = factor(cc))) + 
  geom_bar(stat="identity", alpha=0.8) + scale_fill_manual(values=c("darkgreen", "chartreuse3", "yellow", "orange", "orangered", "red3", "blue")) +
  labs( y = "Number of Observations", x = "", fill = "Climate Change variables")+ theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'), legend.position= "left",axis.text=element_text(size=12),axis.title=element_text(size=12),legend.text=element_text(size=12))



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
