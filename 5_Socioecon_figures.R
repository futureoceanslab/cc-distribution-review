##################################################################
##### This script produces socioeconomic fugures for the manuscript.
##### 01/03/2019
##### INPUT FILES: biblio_database_full.csv
##### OUTPUT FILES: manuscipt figures XXXXXXXXXXXX
##################################################################

library(RColorBrewer)#brewer.pal
library(tidyverse)
library(ggrepel)#geom_text_repel
source("function_multiplot.R")

#Open Biblio_data with SAU data on FE
data_original <- read.csv("data/biblio_database_full.csv")
length(unique(data_original$scientific_name)) #193, ok!
data_original$ID <- 1:dim(data_original)[1]


#####1. DATA TYDING ####
# REMOVE NAS IN FISHING ENTITY
remove <- filter(data_original, fishing_entity  == "Unknown Fishing Country")
rm_id <- remove$ID
data <- filter(data_original, !ID %in% rm_id)
length(unique(data$scientific_name)) #193, ok!

remove2 <- filter(data_original, is.na(fishing_entity) == T)
rm_id <- remove2$ID
data <- filter(data, !ID %in% rm_id)
length(unique(data$scientific_name)) #
#we loose a lot of species again!!

# Subsets of the impacts depending on the response type lat or depth
data$response <- as.factor(data$response)
levels(data$response)
latitude <- subset (data, response == "latitude")
depth <- subset (data, response == "depth")


#####2. FIGURE 3: RELATIONAL DEPENDENCY between FISHING ENTITIES AND EEZ (T AND $) ####
##plot catch dependency
P1<- ggplot(data, aes(x = area_name, y = fishing_entity, fill = catchdepFEEZ)) +
  geom_tile(data = subset(data, !is.na(fishing_entity))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_gradientn("Dependency (t)", colours = rev(brewer.pal(9, "Spectral"))) +
  xlab("Economic Exclusive Zones") + ylab("Fishing entities") +
  ggtitle("a)")
P1

##plot $ dependency
P2<- ggplot(data, aes(x = area_name, y = fishing_entity, fill = landedvalueFEEZ/landedvalueFE)) +
  geom_tile(data = subset(data, !is.na(fishing_entity))) +
  theme_bw() +
  theme( axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_gradientn("Dependency ($)", colours = rev(brewer.pal(9, "Spectral"))) +
  xlab("Economic Exclusive Zones") + ylab("Fishing entities") +
  ggtitle("b)")
P2
## If we want to modify scale, code here: ", limits=c(0,1250000), breaks=seq(0,1250000, by=1000000)"

##PAPER Fig3 
png(file = "paper_figures/figure_3ab.png", 
    width = 16, height = 7, units = 'in', res = 600)
F3 <- multiplot(P1, P2, cols = 2)
dev.off()


# REMOVE OUTLIERS in impact. remove decadal_change>300, one observation
# range(data$decadal_change)
# plot(data$decadal_change, data$id_obs) #to check for outliers
# data <- subset(data, data$decadal_change < 300)

#####3. FIGURE 4:

##prueba grafico percapita catch (relative catches)
ggplot(latitude, aes(catchpercapita, decadal_change, label = scientific_name)) +
  geom_point(aes(colour = tonnesFEspT/1000000, size = 1.3, alpha = 0.6)) +
  scale_colour_gradient(low = "blue", high = "red", name = "Catch\n(million t/year)") +
  theme(axis.text.x = element_text(angle = -45, hjust = 0.06, size = 10),
        axis.text.y = element_text(size = 10),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_text_repel(data = subset(latitude, latitude$catchpercapita > 0.005), 
                  aes(color = tonnesFEspT/1000000), size = 3, vjust = 1) +
  guides(size = F,
         alpha = F) +
  labs(x = "fishing countries percapita catch",
       y = "latitude shift (km)") + 
  facet_wrap(~ fishing_entity)

library(ggforce)
##prueba grafico landed/gdp (catch)relative landed value)
ggplot(latitude, aes(landedperGDP, decadal_change, label = scientific_name)) +
  geom_point(aes(color = landedvalueFEspT/1000000, size = 1.3, alpha = 0.6)) +
  scale_colour_gradient(low = "blue", high = "red", name = "Value ($)") +
  theme(axis.text.x = element_text(angle = -45, hjust = 0.06, size = 10),
        axis.text.y = element_text(size = 10),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        legend.key=element_blank(),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_text_repel(data=subset(latitude, latitude$landedperGDP > 0.0002), 
                  aes(color = landedvalueFEspT/1000000), size = 3, vjust = 1) +
  guides(size = F,
         alpha = F) +
  labs(x = "relative landed value",
       y = "latitude shift (km)") +
  facet_wrap(~ fishing_entity) 


##per capita impacts are higher for nordic countries, landed value importance at the country level more important for northern and african countries


#dependency
ggplot(latitude, aes(catchdepFE, decadal_change, label = scientific_name)) +
  geom_point(aes(color = landedvalueFEsp/landedvalueFE, size = 1.3, alpha = 0.6)) +
  scale_colour_gradient(low = "blue", high = "red", name = "Value dependency") +
  theme(axis.text.x = element_text(angle = -45, hjust = 0.06, size = 10),
        axis.text.y = element_text(size = 10),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        legend.key=element_blank(),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_text_repel(data=subset(latitude, catchdepFE > 0.1), 
                  aes(color = landedvalueFEsp/landedvalueFE), size = 3, vjust = 1) +
  guides(size = F,
         alpha = F) +
  labs(x = "catch dependency",
       y = "latitude shift (km)") +
  facet_wrap(~ fishing_entity)


#total (catch)
latitude$catchcapita <- latitude$tonnesFEspT / latitude$pop
latitude$valgdp <- latitude$landedvalueFEspT / latitude$gdp

# a<-latitude %>%
#   group_by(fishing_entity, scientific_name) %>%
#   summarise(catchcapita = unique(catchcapita),
#             decadal_change = mean(decadal_change, na.rm = T),
#             n = n()) 
# 
# b <- filter(latitude, fishing_entity == "Russian Federation", 
#             scientific_name == "Gadus morhua")

ggplot(latitude, aes(catchcapita, decadal_change, label = scientific_name)) +#tonnesFEspT/pop
  geom_point(aes(color = tonnesFEspT, size = 1.3, alpha = 0.4)) +
  scale_colour_gradient(low = "blue", high = "red", name = "Total catch (t)") +
  theme(axis.text.x = element_text(angle = -45, hjust = 0.06, size = 10),
        axis.text.y = element_text(size = 10),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        legend.key=element_blank(),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_text_repel(data=subset(latitude, catchcapita > 0.4),
                  aes(color = tonnesFEspT), size = 3, vjust = 1) +
  guides(size = F,
         alpha = F) +
  labs(x = "catch per capita",
       y = "latitude shift (km)") +
  facet_wrap(~ fishing_entity) 


#total (value)
ggplot(latitude, aes(valgdp, decadal_change, label = scientific_name)) +#tonnesFEspT/pop
  geom_point(aes(color = landedvalueFEspT, size = 1.3, alpha = 0.4)) +
  scale_colour_gradient(low = "blue", high = "red", name = "Total value ($)") +
  theme(axis.text.x = element_text(angle = -45, hjust = 0.06, size = 10),
        axis.text.y = element_text(size = 10),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        legend.key=element_blank(),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_text_repel(data = subset(latitude, valgdp > 0.01),
                  aes(color = landedvalueFEspT), size = 3, vjust = 1) +
  guides(size = F,
         alpha = F) +
  labs(x = "value per gdp",
       y = "latitude shift (km)") +
  facet_wrap(~ fishing_entity) 


#ALL!!!!!!!!!!!!!!!
latitude2 <- latitude
latitude2$fishing_entity <- as.character(latitude2$fishing_entity)
unique(latitude2$fishing_entity)
latitude2$fishing_entity[latitude2$fishing_entity == "Russian Federation"] <- "Russia"
latitude2$fishing_entity[latitude2$fishing_entity == "Korea (South)"] <- "Korea"
latitude2$fishing_entity[latitude2$fishing_entity == "Saint Pierre & Miquelon (France)"] <- "St P&M (Fr)"

latitude2$area_name <- as.character(latitude2$area_name)
unique(latitude2$area_name)
latitude2$area_name[latitude2$area_name == "USA (East Coast)"] <- "USA (East)"
latitude2$area_name[latitude2$area_name == "USA (West Coast)"] <- "USA (West)"
latitude2$area_name[latitude2$area_name == "USA (Alaska, Subarctic)"] <- "USA (Alaska)"
latitude2$area_name[latitude2$area_name == "Azores Isl. (Portugal)"] <- "Azores Isl"
latitude2$area_name[latitude2$area_name == "Spain (mainland, Med and Gulf of Cadiz)"] <- "Spain (Med)"
latitude2$area_name[latitude2$area_name == "South Africa (Atlantic and Cape)"] <- "South Africa (ATL,Cape)"
latitude2$area_name[latitude2$area_name == "France (Atlantic Coast)"] <- "France (ATL Coast)"



ggplot(latitude2, aes(area_name, decadal_change, label = scientific_name)) +#tonnesFEspT/pop
  geom_point(aes(color = catchpercapita, size = 2.5, alpha = 0.4)) +
  scale_colour_gradient(low = "blue", high = "red", name = "Catchcapita") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.35, size = 8),
        axis.text.y = element_text(size = 10),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        legend.key=element_blank(),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_text_repel(data = subset(latitude2, decadal_change > 350),
                   aes(color = catchpercapita), size = 3, vjust = 1) +
  guides(size = F,
         alpha = F) +
  labs(x = "EEZ",
       y = "latitude shift (km)",
       title = "Fishing countries") +
  facet_wrap(~ fishing_entity) 


###FIGURE FISHING ENTITIES CATCH DEPENDENCY
##LATITUDE
P3 <- ggplot(latitude, aes(catchdepFE, decadal_change, colour=decadal_change, label = scientific_name)) +
  geom_point(aes(colour = decadal_change, size = tonnesEEZsp), alpha = 0.4) +
  geom_text_repel(data=subset(latitude, latitude$catchdepFE>0.07), aes(color=decadal_change), size=3, 
                  vjust=1) +
  scale_color_gradient("km/decade", low = "blue", high = "red")+
  ylim(-100, 450)+
  scale_size_continuous("Species landings in EEZ (t/y)", range=c(3,20),breaks=c(250000, 500000, 750000, 1000000))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        #legend.key=element_blank(),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  labs(x = "FE species catch dependency on EEZ",
       y = "latitude shift")
P3

##DEPTH
P4 <- ggplot(depth, aes(catchdepFE, decadal_change, colour=decadal_change, label = scientific_name)) +
  geom_point(aes(colour = decadal_change, size = tonnesEEZsp), alpha = 0.4) +
  geom_text_repel(data=subset(depth, depth$catchdepFE>0.10), aes(color=decadal_change), size=3, 
                  vjust=1) +
  scale_color_gradient("km/decade", low = "blue", high = "red")+
  ylim(-50, 100)+
  scale_size_continuous("Species landings in EEZ (t/y)", range=c(3,20),breaks=c(250000, 500000, 750000, 1000000))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        #legend.key=element_blank(),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  labs(x = "FE species catch dependency on EEZ",
       y = "depth shift")
P4

##Paper Fig 4. IMpact and Catch dependency
png(file = "paper_figures/figure_4ab.png", 
 width = 16, height = 7, units = 'in', res = 600)
F4 <- multiplot(P3, P4, cols = 2)
dev.off()


##Fishing entities impact catch dependency in value
##LATITUDE
latitude$landdepFE <- latitude$landedvalueFEsp/latitude$landedvalueFE   #fishing entity dependency on EEZ for economic landings of that species
latitude$landedvalueEEZsp <- latitude$landedvalueEEZsp/1000000   #convert landedvalues to million $

P5 <- ggplot(latitude, aes(landdepFE, decadal_change, colour=decadal_change, label = scientific_name)) +
  geom_point(aes(colour = decadal_change, size = landedvalueEEZsp), alpha = 0.4) +
  geom_text_repel(data=subset(latitude, latitude$landdepFE>0.1), aes(color=decadal_change), size=3, 
  vjust=1) +
  scale_color_gradient("km/decade", low = "blue", high = "red")+
  ylim(-100, 450)+
  scale_size_continuous("Species landed value in EEZ (M$)", range=c(3,20),breaks=c(50, 100, 500, 1000))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        #legend.key=element_blank(),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  labs(x = "FE landed value dependency on EEZ",
       y = "latitude shift")
P5

##DEPTH
depth$landdepFE <- depth$landedvalueFEsp/depth$landedvalueFE   #fishing entity dependency on EEZ for economic landings of that species
depth$landedvalueEEZsp <- depth$landedvalueEEZsp/1000000   #convert landedvalues to million $

P6 <- ggplot(depth, aes(landdepFE, decadal_change, colour=decadal_change, label = scientific_name)) +
  geom_point(aes(colour = decadal_change, size = landedvalueEEZsp), alpha = 0.4) +
  geom_text_repel(data=subset(depth, depth$landdepFE>0.10), aes(color=decadal_change), size=3, 
                  vjust=1) +
  scale_color_gradient("km/decade", low = "blue", high = "red")+
  ylim(-50, 100)+
  scale_size_continuous("Species landed value in EEZ (M$)", range=c(3,20),breaks=c(50, 100, 500, 1000))+
  scale_size_continuous("Species landed value in EEZ (M$)", range=c(3,20),breaks=c(50, 100, 500, 1000))+
  
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        #legend.key=element_blank(),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  labs(x = "FE landed value dependency on EEZ",
       y = "depth shift")
P6

##Paper Fig 5. IMpact and Catch dependency - landed value
png(file = "paper_figures/figure_5ab.png", 
    width = 16, height = 7, units = 'in', res = 600)
F5 <- multiplot(P5, P6, cols = 2)
dev.off()


####FINAL TABLE of most vulnerable countries

##Latitude
LatitudeTable <- subset(latitude, latitude$catchdepFE>0.05)
colnames(LatitudeTable)
LatitudeTable <- LatitudeTable[ , c(3, 2, 8, 1, 31)]

write.csv(LatitudeTable, "paper_tables/latitudetable.csv")

##Depth
DepthTable <- subset(depth, depth$catchdepFE>0.04)
colnames(DepthTable)
DepthTable <- DepthTable[ , c(3, 2, 8, 1, 31)]

write.csv(DepthTable, "paper_tables/depthtable.csv")


##################################### SUPPLEMENTARY MATERIALS #############################################



#Final figures socioeconomic - SSMM
##Latitude with catches and labels on fishing entities

P3s1 <- ggplot(latitude, aes(catchdepFE, decadal_change, colour=decadal_change, label = fishing_entity)) +
  geom_point(aes(colour = decadal_change, size = tonnesEEZsp), alpha = 0.4) +
  geom_text_repel(data=subset(latitude, latitude$catchdepFE>0.07), aes(color=decadal_change), size=3, 
                  vjust=1) +
  scale_color_gradient("km/decade", low = "blue", high = "red")+
  ylim(-100, 450)+
  scale_size_continuous("Species landings in EEZ (t/y)", range=c(3,20),breaks=c(250000, 500000, 750000, 1000000))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        #legend.key=element_blank(),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  labs(x = "FE species catch dependency on EEZ",
       y = "latitude shift")
P3s1


##Latitude with catches and EEZ labels 
P3s2 <- ggplot(latitude, aes(catchdepFE, decadal_change, colour=decadal_change, label = area_name)) +
  geom_point(aes(colour = decadal_change, size = tonnesEEZsp), alpha = 0.4) +
  geom_text_repel(data=subset(latitude, latitude$catchdepFE>0.07), aes(color=decadal_change), size=3, 
                  vjust=1) +
  scale_color_gradient("km/decade", low = "blue", high = "red")+
  ylim(-100, 450)+
  scale_size_continuous("Species landings in EEZ (t/y)", range=c(3,20),breaks=c(250000, 500000, 750000, 1000000))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        #legend.key=element_blank(),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  labs(x = "FE species catch dependency on EEZ",
       y = "latitude shift")
P3s2


png(file = "paper_figures/figure_3s.png", 
    width = 16, height = 7, units = 'in', res = 600)
F3s <- multiplot(P3s1, P3s2, cols = 2)
dev.off()

###DEPTH with catched and fishing entity labels
P4s1 <- ggplot(depth, aes(catchdepFE, decadal_change, colour=decadal_change, label = fishing_entity)) +
  geom_point(aes(colour = decadal_change, size = tonnesEEZsp), alpha = 0.4) +
  geom_text_repel(data=subset(depth, depth$catchdepFE>0.10), aes(color=decadal_change), size=3, 
                  vjust=1) +
  scale_color_gradient("km/decade", low = "blue", high = "red")+
  ylim(-50, 100)+
  scale_size_continuous("Species landings in EEZ (t/y)", range=c(3,20),breaks=c(250000, 500000, 750000, 1000000))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        #legend.key=element_blank(),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  labs(x = "FE species catch dependency on EEZ",
       y = "depth shift")
P4s1


P4s2 <- ggplot(depth, aes(catchdepFE, decadal_change, colour=decadal_change, label = area_name)) +
  geom_point(aes(colour = decadal_change, size = tonnesEEZsp), alpha = 0.4) +
  geom_text_repel(data=subset(depth, depth$catchdepFE>0.10), aes(color=decadal_change), size=3, 
                  vjust=1) +
  scale_color_gradient("km/decade", low = "blue", high = "red")+
  ylim(-50, 100)+
  scale_size_continuous("Species landings in EEZ (t/y)", range=c(3,20),breaks=c(250000, 500000, 750000, 1000000))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        #legend.key=element_blank(),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  labs(x = "FE species catch dependency on EEZ",
       y = "depth shift")
P4s2

png(file = "paper_figures/figure_4s.png", 
    width = 16, height = 7, units = 'in', res = 600)
F4s <- multiplot(P4s1, P4s2, cols = 2)
dev.off()


###for fishing landed values

##LATITUDE and landings and fishing entities as labels
latitude$landdepFE <- latitude$landedvalueFEsp/latitude$landedvalueFE   #fishing entity dependency on EEZ for economic landings of that species
latitude$landedvalueEEZsp <- latitude$landedvalueEEZsp/1000000   #convert landedvalues to million $

P5s1 <- ggplot(latitude, aes(landdepFE, decadal_change, colour=decadal_change, label = fishing_entity)) +
  geom_point(aes(colour = decadal_change, size = landedvalueEEZsp), alpha = 0.4) +
  geom_text_repel(data=subset(latitude, latitude$landdepFE>0.1), aes(color=decadal_change), size=3, 
                  vjust=1) +
  scale_color_gradient("km/decade", low = "blue", high = "red")+
  ylim(-100, 450)+
  scale_size_continuous("Species landed value in EEZ (M$)", range=c(3,20),breaks=c(50, 100, 500, 1000))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        #legend.key=element_blank(),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  labs(x = "FE landed value dependency on EEZ",
       y = "latitude shift")
P5s1


##LATITUDE and landings and EEZ as labels
latitude$landdepFE <- latitude$landedvalueFEsp/latitude$landedvalueFE   #fishing entity dependency on EEZ for economic landings of that species
latitude$landedvalueEEZsp <- latitude$landedvalueEEZsp/1000000   #convert landedvalues to million $

P5s2 <- ggplot(latitude, aes(landdepFE, decadal_change, colour=decadal_change, label = area_name)) +
  geom_point(aes(colour = decadal_change, size = landedvalueEEZsp), alpha = 0.4) +
  geom_text_repel(data=subset(latitude, latitude$landdepFE>0.1), aes(color=decadal_change), size=3, 
                  vjust=1) +
  scale_color_gradient("km/decade", low = "blue", high = "red")+
  ylim(-100, 450)+
  scale_size_continuous("Species landed value in EEZ (M$)", range=c(3,20),breaks=c(50, 100, 500, 1000))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        #legend.key=element_blank(),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  labs(x = "FE landed value dependency on EEZ",
       y = "latitude shift")
P5s2

png(file = "paper_figures/figure_5s.png", 
    width = 16, height = 7, units = 'in', res = 600)
F5s <- multiplot(P5s1, P5s2, cols = 2)
dev.off()


##DEPTH with fishing entity labels
depth$landdepFE <- depth$landedvalueFEsp/depth$landedvalueFE   #fishing entity dependency on EEZ for economic landings of that species
depth$landedvalueEEZsp <- depth$landedvalueEEZsp/1000000   #convert landedvalues to million $

P6s1 <- ggplot(depth, aes(landdepFE, decadal_change, colour=decadal_change, label = fishing_entity)) +
  geom_point(aes(colour = decadal_change, size = landedvalueEEZsp), alpha = 0.4) +
  geom_text_repel(data=subset(depth, depth$landdepFE>0.10), aes(color=decadal_change), size=3, 
                  vjust=1) +
  scale_color_gradient("km/decade", low = "blue", high = "red")+
  ylim(-50, 100)+
  scale_size_continuous("Species landed value in EEZ (M$)", range=c(3,20),breaks=c(50, 100, 500, 1000))+
  scale_size_continuous("Species landed value in EEZ (M$)", range=c(3,20),breaks=c(50, 100, 500, 1000))+
  
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        #legend.key=element_blank(),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  labs(x = "FE landed value dependency on EEZ",
       y = "depth shift")
P6s1

##DEPTH with EEZ labels
P6s2 <- ggplot(depth, aes(landdepFE, decadal_change, colour=decadal_change, label = area_name)) +
  geom_point(aes(colour = decadal_change, size = landedvalueEEZsp), alpha = 0.4) +
  geom_text_repel(data=subset(depth, depth$landdepFE>0.10), aes(color=decadal_change), size=3, 
                  vjust=1) +
  scale_color_gradient("km/decade", low = "blue", high = "red")+
  ylim(-50, 100)+
  scale_size_continuous("Species landed value in EEZ (M$)", range=c(3,20),breaks=c(50, 100, 500, 1000))+
  scale_size_continuous("Species landed value in EEZ (M$)", range=c(3,20),breaks=c(50, 100, 500, 1000))+
  
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        #legend.key=element_blank(),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  labs(x = "FE landed value dependency on EEZ",
       y = "depth shift")
P6s2

png(file = "paper_figures/figure_6s.png", 
    width = 16, height = 7, units = 'in', res = 600)
F6s <- multiplot(P6s1, P6s2, cols = 2)
dev.off()



##Making a specific subset to plot the specific varibales
mybiblio_database <- read.csv("data/biblio_database.csv", stringsAsFactors=FALSE, header=T, sep = ",")

##Subset of biblio_database without duplicated data
##mybiblio_database<- mybiblio_database%>%
##  filter(mybiblio_database$duplicate=="1")
mybiblio_database<-mybiblio_database[,c(2, 3,4, 6, 8,32,33,59, 63)]
str(mybiblio_database)
mybiblio_database<- as.data.frame(mybiblio_database)

##order in time 
mybiblio_database<-mybiblio_database[order(mybiblio_database$article_year),]

##creating new column with the number of observations
mybiblio_database$observations<-1 

###### characteristics
names(mybiblio_database)
glimpse( mybiblio_database)

##1st graph : obersvations-papers-duration of studies
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==10]<-"0-25"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==15]<-"0-25"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==17]<-"0-25"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==23]<-"0-25"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==24]<-"0-25"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==25]<-"0-25"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==27]<-"26-35"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==28]<-"26-35"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==30]<-"26-35"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==31]<-"26-35"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==35]<-"26-35"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==38]<-"35-45"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==39]<-"35-45"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==40]<-"35-45"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==41]<-"35-45"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==42]<-"35-45"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==75]<-"75-80"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==77]<-"75-80"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==78]<-"75-80"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==80]<-"75-80"

##Re-order the axis from alphabetical to dataframe's order
mybiblio_database$reference <- reorder( mybiblio_database$reference, mybiblio_database$article_year)

##Tailoring data for next plotting
mybiblio_database$years_data_pattern<- factor( mybiblio_database$years_data_pattern, levels=c("1","2"),
                                            labels=c("Continuous","Discontinuous")) 

mybiblio_database$data_type<- factor( mybiblio_database$data_type, levels=c("1","4"),
                                      labels=c("Data collected","Biological surveys"))

mybiblio_database$statistical_methodology<- factor( mybiblio_database$statistical_methodology, levels=c("1","2","5","2,3"),
                                                    labels=c("Correlation","Regression","Indirect relation", "Regression and Spatial analysis"))

##FIGURES
##1st Figure: Stalked-Bar-Chart that shows "nº observations-duration of studies-statistical methodology"
ggplot(data = mybiblio_database, aes(x = categoriesbyyears, y = observations, fill = statistical_methodology)) + 
  geom_bar(stat="identity", alpha=0.8) + scale_fill_manual(values=c("darkgreen", "yellow", "orange", "orangered")) +
  labs(title="Quality of data-source", y = "Number of observations", x = "Duration of the studies (Years)", fill = "Statistical methodology")+ 
  theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'), legend.position= "left")

##2d figure: Table with general information.
## Plotting a table including following variables: reference, article_year, years_data, years_data_patern, data_type and statistical_methodology 
#creating a new subset
mytable<-mybiblio_database[,c(3,4,6,7,8,9,10)]
mytable
##Re-order the columns in the dataframe
mytable<-mytable[, c(1,2,7,3,4,5,6)]
##Save the table in csv to further manipulation
write.csv(mytable, file = "data/Methodsdata.csv")

## Creation of a database in excell (called "Methods_table.csv") based on the information of mytable dataframe, but manually simplified.## Need to be codified.
##Open "Methods_table.csv"
Mytable<- read.csv("data/Methods_table.csv", stringsAsFactors=FALSE, header=T, sep = ";")
##Transforming the database in a dataframe
Mytable<- as.data.frame(Mytable)

##Exporting and save the second figure (table) as a pdf
pdf(file = "data/Description of database.pdf", height = 11, width = 13)
grid.table(Mytable, rows=NULL)
dev.off() 
