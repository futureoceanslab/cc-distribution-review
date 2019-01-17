##Graphs for Catch dependency in SAU and Review
#Elena Ojea
#December 2017
#source: Biblio_database_full.csv
#output: figures impacts catch dependency, catch pressure

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(grid)
library(gridExtra)
library(plotly)


#Open Biblio_data with SAU data on EEZ and FE:
data <- read.csv("data/Biblio_database_full.csv", stringsAsFactors = FALSE)

##PREPARE VARIABLES FOR PLOTS

#Subsets of the impacts
data$b_impact <- as.factor(data$b_impact)

levels(data$b_impact)
levels(data$b_impact) <- c("mean lat shift", "mean lat shift", 
                            "depth shift", "boundary lat shift", 
                            "boundary lat shift", "mean lon shift", 
                            "mean lon shift", "mean lat and lon shift", 
                            "area occupied")

latitude <- subset (data, b_impact =="mean lat shift")
depth    <- subset (data, b_impact=="depth shift")
range    <- subset (data, b_impact== "boundary lat shift")
area     <- subset (data, b_impact == "area occupied")


####LATITUDE

ggplot(latitude, aes(area_name, b_value)) +
  geom_point(aes(fill = DemersPelag)) +
  scale_colour_manual(values = c("blue","pink","grey","green","black","yellow")) +
  #geom_text_repel(data=subset(latitude, latitude$catchdepFE>0.05), size=2, vjust=1)+
  theme(axis.text.x = element_text(angle=-45, hjust= 0.06)) +
  scale_color_gradient(low = "blue", high = "red") +
  ggtitle("Countries species dependence")




###PLOTS CATCH DEPENDENCY PER SP and FE total SP, per SP and FE total catch

#FE total catch dependency on species in EEZ
#FIGIRE 1
l1 <- ggplot(latitude, aes(catchdepFE, b_value, label = scientific_name)) +
  geom_jitter(alpha=0.8, aes(color=fishing_entity, size=tonnesFEsp),
              position = position_jitter(width = .001))+
  #geom_text_repel(data=subset(latitude, latitude$catchdepFE>0.05), size=3, vjust=1)+
  #geom_text_repel(data=subset(latitude, latitude$b_value>150), size=3, vjust=1)+
  ggtitle("Latitude Shift")+
  xlab("FE total catch dependency on species in EEZ")+
  ylab("km/decade")+
  theme(legend.position="bottom")
l1


l1 + facet_wrap( ~ latitude$fishing_entity)

#FE species catch dependency in EEZ
#FIGIRE 2
l2 <- ggplot(latitude, aes(catchdepFEsp, b_value, label = scientific_name)) +
  #geom_point()
  geom_jitter(alpha=0.8, aes(color=fishing_entity, size=tonnesFEsp),
              position = position_jitter(width = .001))+
  geom_text_repel(data=subset(latitude, latitude$tonnesFEsp>100000), size=3, vjust=1)+
  geom_text_repel(data=subset(latitude, latitude$b_value>75), size=3, vjust=1)+
  ggtitle("FE species catch dependency in EEZ")+
  xlab("FE-SP catch dep")+
  ylab("km/decade")+
  ylim(-50, 100)+
  theme(legend.position="bottom")
  l2 
  
  
##problema con los NAs?
  #FIGIRE 3
  
  latitude <- subset(latitude, !is.na(latitude$fishing_entity))
  l3 <- ggplot(latitude, aes(fishing_entity, b_value, label = scientific_name, na.rm=TRUE)) +
    #geom_point()
    geom_jitter(alpha=0.8, aes(color=catchdepFEsp, size=tonnesFEsp),
                position = position_jitter(width = .001))+
    theme(axis.text.x = element_text(angle=-45, hjust= 0.06))+
    #geom_text_repel(data=subset(latitude, latitude$tonnesFEsp>20000), size=3, vjust=1)+
    ggtitle("latitude impacts in fishing entities")+
    xlab("Fishing Entities")+
    ylab("km/decade")+
    ylim(-100, 200)
    l3



##using fishing entities in the X Axis
  #FIGURE 4
  latitude <- subset(latitude, !is.na(latitude$fishing_entity))
l4 <- ggplot(latitude, aes(fishing_entity,catchdepFE, label=scientific_name))+
  geom_point(aes(color=b_value, size=tonnesFEsp))+
  geom_text_repel(data=subset(latitude, latitude$catchdepFE>0.05), size=2, vjust=1)+
  theme(axis.text.x = element_text(angle=-45, hjust= 0.06))+
  scale_color_gradient(low = "blue", high = "red")+
  ggtitle("Countries species dependence")
l4

#FIGURE 5
l5 <- ggplot(latitude, aes(fishing_entity,landedvalueFEsp, label=scientific_name))+
  geom_point(aes(color=b_value, size=tonnesFEsp))+
  geom_text_repel(data=subset(latitude, latitude$landedvalueFEsp>1e+08), size=2, vjust=1)+
  theme(axis.text.x = element_text(angle=-45, hjust= 0.06))+
  scale_color_gradient(low = "blue", high = "red")+
  ggtitle("Countries species dependence - value")
l5

###PLOTS CATCH PRESSURE PER EEZ and SP
#FIGURE 6
l6 <- ggplot(latitude, aes(catchpresEEZsp, b_value, label = scientific_name)) +
  #geom_point()
  geom_jitter(alpha=0.8, aes(color=area_name, size=tonnesEEZsp),
              position = position_jitter(width = .001))+
  geom_text_repel(data=subset(latitude, latitude$catchpresEEZsp>0.02), size=2, vjust=1)+
  geom_text_repel(data=subset(latitude, latitude$b_value>150), size=2, vjust=1)+  ggtitle("Latitude Shift")+
  xlab("EEZ-SP catch pressure")+
  ylab("km/decade")
l6 

#FIGURE 7

l7 <- ggplot(latitude, aes(catchpresEEZsp, b_value, label = scientific_name)) +
  geom_jitter(alpha=0.8, aes(color=area_name, size=tonnesEEZ),
              position = position_jitter(width = .001))+
  geom_text_repel(data=subset(latitude, latitude$catchpresEEZsp>0.01), size=3, vjust=1)+
  #geom_text_repel(data=subset(latitude, latitude$b_value>150), size=3, vjust=1)+
  ggtitle("Latitude Shift")+
  xlab("EEZ catch pressure on species")+
  ylab("km/decade")+
  theme(legend.position="bottom")
l7


#FIGURE 8
l8 <- ggplot(latitude, aes(tonnesEEZsp, b_value, label = scientific_name)) +
  geom_jitter(alpha=0.8, aes(color=area_name, size=landedvalueEEZsp),
              position = position_jitter(width = .001))+
  geom_text_repel(data=subset(latitude, latitude$catchpresEEZsp>0.01), size=3, vjust=1)+
  #geom_text_repel(data=subset(latitude, latitude$b_value>150), size=3, vjust=1)+
  ggtitle("Latitude Shift")+
  xlab("tonnesEEZsp")+
  ylab("km/decade")+
  theme(legend.position="bottom")
l8

##using scientific names in the X Axis
#FIGURE 9
l9 <- ggplot(latitude, aes(scientific_name,b_value, label=fishing_entity))+
  geom_point(aes(color=catchpresEEZsp, size=tonnesEEZ))+
  #geom_text_repel(data=subset(latitude, latitude$catchdepFE>0.05), size=3, vjust=1)+
  theme(axis.text.x = element_text(size=8, angle=-45, hjust= 0.06))+
  scale_color_gradient(low = "blue", high = "red")+
  ylim(-50, 200)
l9

#FIGURE 10
latitude_graph1<-latitude %>%
  group_by(fishing_entity,area_name,catchdepFEEZ) %>%
  #filter(fishing_entity=="Belgium")%>%
  summarise(b_mean=mean(b_value,na.rm = T),
            b_min=min(b_value,na.rm = T),
            b_max=max(b_value,na.rm = T),
            #b_sd=sd(b_value,na.rm = T),
            b_n=n(),
            graph="dep")
latitude_graph<-latitude_graph1[complete.cases(latitude_graph1), ]

graph1<-filter(latitude_graph, area_name %in% c("Belgium","Denmark (North Sea)",
                                                "Germany (North Sea)","Netherlands",
                                                "Norway","Spain (Northwest)","United Kingdom (UK)")) 
graph2<-filter(latitude_graph, area_name %in% c("Canada (East Coast)","Japan (main islands)",
                                                "Korea (South)","USA (Alaska, Subarctic)","USA (East Coast)",
                                                "USA (Gulf of Mexico)","USA (West Coast)")) 

ggplot(graph1, aes(x = fishing_entity, y = catchdepFEEZ))+#, group = b_mean, fill = b_mean
  geom_bar(stat = "identity")+#, width = 0.5, position = "dodge"
  facet_grid(. ~ area_name)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, size = 8,vjust=0.2,hjust=1),
        strip.text = element_text(size=12))+
  ggtitle("FE catch dependency on EEZ")

ggplot(graph2, aes(x = fishing_entity, y = catchdepFEEZ))+#, group = b_mean, fill = b_mean
  geom_bar(stat = "identity")+#, width = 0.5, position = "dodge"
  facet_grid(. ~ area_name)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, size = 8,vjust=0.2,hjust=1),
        strip.text = element_text(size=12))+
  ggtitle("FE catch dependency on EEZ")

#?NO PUEDE HABER MISSING VALUES!!!!!!!!!!!! ERROR AL HACER MERGE EN DATA_MERGE_scripts

##MODEL example - needs data from fishbase too
m <-glm(b_value  ~ study_year+lat_dec+b_years+tonnesEEZ+landedvalueEEZ+tonnesEEZsp+landedvalueEEZsp+tonnesFEsp+tonnesFE+landedvalueFE+catchdepFEsp+landdepFEsp+spvalueFE, data=latitude)
summary(m)
