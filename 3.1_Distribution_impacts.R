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
data <- read.csv("data/biblio_database2.csv")

#Factors
data$researcher <- as.factor(data$researcher)
data$id_study <- as.factor(data$id_study)
data$cc <- as.factor(data$cc)
data$b_impact_combine <- as.factor(data$b_impact_combine)
data$b_direction_combine <- as.factor(data$b_direction_combine)

#Numeric
data$b_value <- as.numeric(as.character(data$b_value), na.omit=TRUE)
data$b_years <- as.numeric(as.character(data$b_years))

#SUBSETS COMBINE
lat <- subset (data, b_impact_combine =="lat shift")
depth <- subset (data, b_impact_combine =="depth shift")
boundary <- subset (data, b_impact_combine == "boundary lat shift")
area <- subset (data, b_impact_combine == "shift in area occupied")
lat_lon <- subset (data, b_impact_combine == "lat and long shift")
lon <- subset (data, b_impact_combine == "long shift")
lat_range <- subset (data, b_impact_combine == "latitude range shift")
depth_range <- subset (data, b_impact_combine == "depth range shift")

#################################################
# Figure SSMM. CLIMATE CHANGE VARIABLES PER IMPACT #
#################################################
clim_lat <- as.data.frame(table(lat$cc))
clim_depth <- as.data.frame(table(depth$cc))
clim_bound <- as.data.frame(table(boundary$cc))
clim_ar <- as.data.frame(table(area$cc))
clim_latlon <- as.data.frame(table(lat_lon$cc))
clim_lon <- as.data.frame(table(lon$cc))
clim_depran <- as.data.frame(table(depth_range$cc))
clim_latran <- as.data.frame(table(lat_range$cc))

dat <- cbind(clim_lat, clim_depth[,2], clim_bound[,2], clim_ar[,2],
             clim_latlon[,2], clim_lon[,2], clim_depran[,2], clim_latran[,2])
colnames(dat) <- c("cc","Mean Latitude","Depth","Boundary Latitude","Area",
                   "Latitude and Longitude", "Mean Longitude", "Depth Range",
                   "Latitude Range")

data1 <- melt(dat)
colnames(data1)[2] <- "impacts"

my.labels <- c("Mean \n Latitude",
               "Depth",
               "Boundary \n Latitude",
               "Area",
               "Latitude and \n Longitude", 
               "Mean \n Longitude", 
               "Depth \n Range",
               "Latitude \n Range")

ggplot(data = data1, aes(x = impacts, y = value, fill = factor(cc))) + 
  geom_bar(stat="identity", alpha=0.8) + 
  scale_fill_manual(values=c("darkgreen", "chartreuse3", "yellow", 
                             "orange", "orangered", "red3", "blue", 
                             "pink")) +
  labs(y = "Number of Observations", 
      x = "", 
      fill = "Climate Change variables") + 
  theme(panel.background = element_rect(fill = 'whitesmoke', 
                                        colour = 'black'), 
        legend.position= "left") +
  scale_x_discrete(labels = my.labels)


###############################
# Figure 2. IMPACTS MAGNITUDE #
###############################

############
# LATITUDE #
############

#histogram
lat$sign <- ifelse(lat$b_value > 0,"North", "South") #there are no zeros sum(lat$b_value == 0)

Fig2.lat <- lat %>% 
              ggplot() +
                geom_histogram(aes(b_value, fill = sign, colour = sign),
                               alpha = 0.7, binwidth = 9) +
                scale_x_continuous(name = "Mean Latitude Shift Rate (km/decade)") + 
                scale_y_continuous(name = "Number of Observations") +
                scale_colour_manual(name = "Shift direction", 
                                    values = c("North" = "skyblue", 
                                               "South" = "royalblue4"), 
                                    labels = c("North" = "North", 
                                               "South" = "South")) +
                scale_fill_manual(name = "Shift direction", 
                                  values = c("North" = "skyblue", 
                                             "South"="royalblue4"), 
                                  labels = c("North"="North", 
                                             "South"="South")) +
                theme(panel.background = element_rect(fill = 'whitesmoke',
                                                      colour = 'black'),
                      legend.position = c(.85, .75)) +
                coord_flip() +
                theme_bw() 

#taxa barplot
lat$tax_group <- as.factor(lat$tax_group)

my.labels2 <- c("Bony-fish", 
                "Non-bony \n fish",
                "Benthic \n crustacea",
                "Cephalopode",
                "Benthic \n mollusca")

lat.barplot <- ggplot(lat, aes(tax_group, b_value, fill = tax_group)) + 
                geom_boxplot(data = subset(lat, b_value >= 0), 
                             aes(tax_group, b_value),  
                             na.rm = T, outlier.shape = 1, outlier.size = 0.1) +
                geom_boxplot(data = subset(lat, b_value < 0), 
                             aes(tax_group, b_value),  
                             na.rm = T, outlier.shape = 1, outlier.size = 0.1) +
                geom_hline(yintercept = c(0), linetype = "dotted") +
                scale_y_continuous(name = "km/decade", breaks = seq(-100, 200, by = 50)) +
                xlab(NULL)+
                theme(axis.text.x =element_blank()) +
                scale_fill_brewer(palette = "Blues") +
                theme_bw() +
                scale_x_discrete(labels = my.labels2) +
                theme(legend.position="none")

#########
# DEPTH #
#########
depth$sign <- ifelse(depth$b_value > 0, "Shallower", "Deeper")# there are no zeros sum(depth$b_value == 0)

Fig2.depth <- depth %>%
                ggplot()+
                  geom_histogram(aes(b_value, fill = sign, colour = sign), 
                                 alpha = 0.7, binwidth = 4) +
                  scale_x_continuous(name = "Depth Shift Rate (m/decade)") + 
                  scale_y_continuous(name = "Number of Observations")+
                  scale_colour_manual(name = "Shift direction", 
                                      values = c("Deeper" = "royalblue4", 
                                                 "Shallower"="skyblue"), 
                                      labels = c("Deeper" = "Deeper", 
                                                 "Shallower" = "Shallower"),
                                      guide = guide_legend(reverse = T)) +
                  scale_fill_manual(name = "Shift direction", 
                                    values = c("Deeper" = "royalblue4", 
                                               "Shallower" = "skyblue"), 
                                    labels = c("Deeper" = "Deeper", 
                                               "Shallower" = "Shallower"),
                                    guide = guide_legend(reverse = T)) +
                  theme(panel.background = element_rect(fill = 'whitesmoke', 
                                                        colour = 'black'),
                        legend.position = c(.85, .75)) +
                  coord_flip() +
                  theme_bw()

#taxa barplot depth
depth$tax_group <- as.factor(depth$tax_group)

depth.barplot <- ggplot(depth, aes(tax_group, b_value, fill = tax_group)) +
                    geom_boxplot(data = subset(depth, b_value >= 0), 
                                 aes(tax_group, b_value),  
                                 na.rm = T, outlier.shape = 1, outlier.size = 0.1) +
                    geom_boxplot(data = subset(depth, b_value < 0), 
                                 aes(tax_group, b_value),  
                                 na.rm = T, outlier.shape = 1, outlier.size = 0.1) +
                    geom_hline(yintercept=c(0), linetype = "dotted")+
                    scale_y_continuous(name ="m/decade", breaks = seq(-80, 60, by = 20)) +
                    xlab(NULL) +
                    theme(axis.text.x =element_blank()) +
                    scale_fill_brewer(palette="Blues") +
                    theme_bw() +
                    scale_x_discrete(labels = my.labels2) +
                    theme(legend.position="none")

join.lat.depth <- plot_grid(lat.barplot, Fig2.lat, depth.barplot, Fig2.depth, labels = c("A", "B", "C", "D"), align="hv")
#ggsave("join_lat_depth.jpeg")