##################################################################
##### This script includes the code for the ecological analysis 
##### of our review and produces manuscript figures and data.
##### 20/08/2017
##### INPUT FILES: biblio_database1.csv
##### OUTPUT FILES: manuscript Figures XXXXXXXX
##################################################################

library(tidyverse)
library(reshape2) #dcast function
source("function_multiplot.R")

#open dataset
data <- read.csv("data/biblio_database1.csv")

#Factors
data$id_study <- as.factor(data$id_study)
data$id_obs <- as.factor(data$id_obs)
data$year_publication <- as.factor(data$year_publication)

#Numeric
data$years_data <- as.numeric(as.character(data$years_data), na.omit = T)

##############################################
#VALUES ECOLOGICAL PART MANUSCRIPT
##############################################
v <- data %>% group_by(FAO_area) %>% summarise(n = n())
#Proportion of observation in the North ATL
(196*100)/sum(v$n)

v2 <- data %>% group_by(site) %>% summarise(n = n())
#Proportion of observation in the Eastern Bearing Sea 74
(74*100)/sum(v2$n)

#Number of species by location
v3 <- data %>% group_by(site, scientific_name) %>% summarise(n = n()) %>%
        group_by(site) %>% summarise(sp_num = sum(n))

#number of observations by article
v4 <- data %>% group_by(reference) %>% summarise(n = n()) 
(318*100)/sum(v4$n)

#Pinsky
v5 <- filter(data, reference == "Pinsky, M.L., Worm, B., Fogarty, M.J., Sarmiento, J.L. and Levin, S.A.")
unique(v5$site)

##############################################
# Figure CLIMATE CHANGE VARIABLES PER IMPACT #
##############################################

counts <- dcast(data, response ~ cc_driver_detail, fun.aggregate = length)

cc_driver.counts <- gather(counts,"cc_driver", "counts", 2:8)

ssmm <- ggplot(cc_driver.counts, aes(x = response, y= counts , fill = cc_driver)) + 
          geom_bar(stat="identity", alpha=0.8) + 
          scale_fill_manual(values = c("darkgreen", "grey", "lightblue", 
                                       "yellow", "deeppink4", "black", "red")) +
          labs(y = "Number of Observations", 
               x = "", 
               fill = "Climate Change variables") + 
          theme(panel.background = element_rect(fill = 'whitesmoke', 
                                                colour = 'black'), 
                legend.position= "left") 

############################
# Figure IMPACTS MAGNITUDE #
############################

############
# LATITUDE #
############

#histogram
lat <- subset (data, response == "latitude")
lat$sign <- ifelse(lat$decadal_change > 0,"Polewards (+)", "Towards equator (-)") #there are no zeros sum(lat$decadal_change == 0)

Fig2.lat <- lat %>% 
              ggplot() +
              geom_histogram(aes(decadal_change, fill = sign),
                             alpha = 0.7, binwidth = 9) +
              scale_x_continuous(name = "Mean Latitude Shift Rate (km/decade)") + 
              scale_y_continuous(name = "Number of Observations") +
              scale_colour_manual(name = "Shift direction", 
                                  values = c("Polewards (+)" = "skyblue", 
                                             "Towards equator (-)" = "royalblue4"), 
                                  labels = c("Polewards (+)" = "Polewards", 
                                             "Towards equator (-)" = "Towards equator")) +
              scale_fill_manual(name = "Shift direction", 
                                values = c("Polewards (+)" = "skyblue", 
                                           "Towards equator (-)"="royalblue4"), 
                                labels = c("Polewards (+)" = "Polewards", 
                                           "Towards equator (-)" = "Towards equator")) +
              coord_flip() +
              theme_bw() +
              theme(legend.position = c(.85, .9),
                    axis.title.x = element_text(size = 16),
                    axis.title.y = element_text(size = 16),
                    axis.text.x = element_text(size = 14),
                    axis.text.y = element_text(size = 14),
                    title = element_text(size = 16),
                    legend.title = element_text(size = 16),
                    legend.text = element_text(size = 14)) +
              ggtitle("b)")

#taxa barplot
my.labels2 <- c("Benthic \n crustacea",
                "Benthic \n mollusca",
                "Bony-fish", 
                "Cephalopode",
                "Non-bony \n fish")

lat.barplot <- ggplot(lat, aes(taxa, decadal_change)) + 
                geom_boxplot(data = subset(lat, decadal_change >= 0), 
                             aes(taxa, decadal_change),  
                             na.rm = T, outlier.shape = 1, outlier.size = 0.1) +
                geom_boxplot(data = subset(lat, decadal_change < 0), 
                             aes(taxa, decadal_change),  
                             na.rm = T, outlier.shape = 1, outlier.size = 0.1) +
                geom_hline(yintercept = c(0), linetype = "dotted") +
                scale_y_continuous(name = "km/decade", breaks = seq(-100, 200, by = 50)) +
                xlab(NULL)+
                theme(axis.text.x = element_blank()) +
                theme_bw() +
                scale_x_discrete(labels = my.labels2) +
                theme(legend.position="none",
                      axis.title.y = element_text(size = 16),
                      axis.text.x = element_text(size = 14),
                      axis.text.y = element_text(size = 14),
                      title = element_text(size = 16)) +
                ggtitle("a)")

#########
# DEPTH #
#########
depth <- subset (data, response == "depth")
depth$sign <- ifelse(depth$decadal_change < 0, "Shallower (-)", "Deeper (+)")# there are no zeros sum(depth$decadal_change == 0)

depth$decadal_change_reversed <- (-1)*depth$decadal_change

Fig2.depth <- depth %>%
                ggplot()+
                geom_histogram(aes(decadal_change_reversed, fill = sign), 
                               alpha = 0.7, binwidth = 4) +
                scale_x_continuous(name = "Depth Shift Rate (m/decade)") + 
                scale_y_continuous(name = "Number of Observations")+
                scale_colour_manual(name = "Shift direction", 
                                    values = c("Deeper (+)" = "royalblue4", 
                                               "Shallower (-)"="skyblue"), 
                                    labels = c("Deeper (+)" = "Deeper", 
                                               "Shallower (-)" = "Shallower"),
                                    guide = guide_legend(reverse = T)) +
                scale_fill_manual(name = "Shift direction", 
                                  values = c("Deeper (+)" = "royalblue4", 
                                             "Shallower (-)" = "skyblue"), 
                                  labels = c("Deeper (+)" = "Deeper", 
                                             "Shallower (-)" = "Shallower"),
                                  guide = guide_legend(reverse = T)) +
                coord_flip() +
                theme_bw() +
                theme(legend.position = c(.85, .9),
                      axis.title.x = element_text(size = 16),
                      axis.title.y = element_text(size = 16),
                      axis.text.x = element_text(size = 14),
                      axis.text.y = element_text(size = 14),
                      title = element_text(size = 16),
                      legend.title = element_text(size = 16),
                      legend.text = element_text(size = 14)) +
                ggtitle("d)")

#taxa barplot depth
depth.barplot <- ggplot(depth, aes(taxa, decadal_change)) +
                  geom_boxplot(data = subset(depth, decadal_change >= 0), 
                               aes(taxa, decadal_change),  
                               na.rm = T, outlier.shape = 1, outlier.size = 0.1) +
                  geom_boxplot(data = subset(depth, decadal_change < 0), 
                               aes(taxa, decadal_change),  
                               na.rm = T, outlier.shape = 1, outlier.size = 0.1) +
                  geom_hline(yintercept = c(0), linetype = "dotted")+
                  scale_y_continuous(name ="m/decade", breaks = seq(-80, 60, by = 20)) +
                  xlab(NULL) +
                  theme(axis.text.x = element_blank()) +
                  theme_bw() +
                  scale_x_discrete(labels = my.labels2) +
                  theme(legend.position="none",
                        axis.title.y = element_text(size = 16),
                        axis.text.x = element_text(size = 14),
                        axis.text.y = element_text(size = 14),
                        title = element_text(size = 16)) +
                  ggtitle("c)")

#saving figures
#join.lat.depth <- plot_grid(lat.barplot, Fig2.lat, depth.barplot, Fig2.depth, labels = c("A", "B", "C", "D"), align="hv")
#plot(join.lat.depth)
#ggsave("paper_figures/join_lat_depth.jpeg")

png(file = "paper_figures/figure_2ab.png", 
    width = 16, height = 7, units = 'in', res = 600)
multiplot(lat.barplot, Fig2.lat, cols = 2)
dev.off()

png(file = "paper_figures/figure_2cd.png", 
    width = 16, height = 7, units = 'in', res = 600)
multiplot(depth.barplot, Fig2.depth, cols = 2)
dev.off()

png(file = "paper_figures/ssmm_1.png", 
    width = 10, height = 7, units = 'in', res = 600)
multiplot(ssmm)
dev.off()