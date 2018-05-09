##Graphs for Catch dependency in SAU and Review
#Elena Ojea
#December 2017
#source: Biblio_database_full.csv
#output: figures impacts catch dependency, catch pressure


library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)


#Open Biblio_data with SAU data on EEZ and FE:

Biblio_data <- read.csv("data/Biblio_database_full.csv", stringsAsFactors=FALSE)


#CATCH DEPENDENCY OF FISHING ENTITIES

#1. Species catch dependency on the area
Biblio_data$catchdepFEsp <- Biblio_data$tonnesFEsp/Biblio_data$tonnesFEspT #the dependence of the country species catches on the EEZ species catches
Biblio_data$landdepFEsp <-  Biblio_data$landedvalueFEsp/Biblio_data$landedvalueFEspT #the dependence of the country total SP Value on the EEZ SP catch value 

range(Biblio_data$catchdepFEsp, na.rm=TRUE)
range(Biblio_data$landdepFEsp, na.rm=TRUE) #is the same relation

#2. Country dependency on the species in the area

Biblio_data$catchdepFE <- Biblio_data$tonnesFEsp/Biblio_data$tonnesFE #the dependence of the country species catches on the EEZ species catches
range(Biblio_data$catchdepFE, na.rm=TRUE)

#3. Country dependency on the area

Biblio_data$catchdepFEEZ <- Biblio_data$tonnesFEEZ/Biblio_data$tonnesEEZ 
range(Biblio_data$catchdepFEEZ, na.rm=TRUE)

##VALUE OF SPECIES FOR FISHING ENTITIES

###Species VAlue in FE: landed value/tonnes

Biblio_data$spvalueFE  <- Biblio_data$landedvalueFEsp/Biblio_data$tonnesFEsp
range(Biblio_data$spvalueFE, na.rm=TRUE)
quantile(Biblio_data$spvalueFE, na.rm=TRUE)


##CATCH PRESSURE IN EEZ

Biblio_data$catchpresEEZsp <- Biblio_data$tonnesEEZsp/Biblio_data$tonnesEEZ
Biblio_data$landpresEEZsp <- Biblio_data$landedvalueEEZsp/Biblio_data$landedvalueEEZ

range(Biblio_data$catchpresEEZsp, na.rm=TRUE)

#prices in EEZs (something is wrong here, strange numbers!)
Biblio_data$spvalueEEZ <- Biblio_data$tonnesEEZsp/Biblio_data$landedvalueEEZsp 
range(Biblio_data$spvalueEEZ, na.rm=TRUE)
quantile(Biblio_data$spvalueEEZ, na.rm=TRUE)



##PREPARE VARIABLES FOR PLOTS

#Subsets of the impacts
Biblio_data$b_impact<-as.factor(Biblio_data$b_impact)

#levels(Biblio_dataSAU$b_impact)
levels(Biblio_data$b_impact)<-c("mean lat shift", "mean lat shift", "depth shift", "boundary lat shift", "boundary lat shift", "mean long shift", "mean long shift", "mean lat and long shift", "area occupied")
#remove Nicolas (uds=6)
Biblio_data <- subset(Biblio_data, Biblio_data$id_study!=6)

latitude <- subset (Biblio_data, b_impact =="mean lat shift")
depth    <- subset (Biblio_data, b_impact=="depth shift")
range    <- subset (Biblio_data, b_impact== "boundary lat shift")
area     <- subset (Biblio_data, b_impact == "area occupied")


####LATITUDE
###PLOTS CATCH DEPENDENCY PER SP and FE total SP, per SP and FE total catch


#FE total catch dependency on species in EEZ
#FIGIRE 1
l1 <- ggplot(latitude, aes(catchdepFE, b_value, label = scientific_name)) +
  geom_jitter(alpha=0.8, aes(color=fishing_entity, size=tonnesFEsp),
              position = position_jitter(width = .001))+
  geom_text_repel(data=subset(latitude, latitude$catchdepFE>0.05), size=3, vjust=1)+
  geom_text_repel(data=subset(latitude, latitude$b_value>150), size=3, vjust=1)+
  ggtitle("Latitude Shift")+
  xlab("FE total catch dependency on species in EEZ")+
  ylab("km/decade")+
  theme(legend.position="bottom")
l1

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
latitude_b_min<-latitude %>%
  group_by(fishing_entity) %>%
  summarise(b_mean=mean(b_value,na.rm = T),
            b=min(b_value,na.rm = T),
            b_sd=sd(b_value,na.rm = T),
            b_n=n(),
            graph="b_min")
#latitude_b_min$graph<-"b_min"
latitude_b_max<-latitude %>%
  group_by(fishing_entity) %>%
  summarise(b_mean=mean(b_value,na.rm = T),
            b=max(b_value,na.rm = T),
            b_sd=sd(b_value,na.rm = T),
            b_n=n(),
            graph="b_max")
#latitude_b_max$graph<-"b_max"

latitude_b<-bind_rows(latitude_b_min,latitude_b_max)

ggplot(latitude_b)+
  geom_point(aes(x=fishing_entity,y=graph,size=b))+ #color=b, 
  #scale_color_gradient(low = "blue", high = "red")+
  scale_color_gradient(guide = F)+
  ylim(min(latitude_b$graph), max(latitude_b$graph))+ 
  coord_flip()
#NO PUEDE HABER MISSING VALUES!!!!!!!!!!!! ERROR AL HACER MERGE EN DATA_MERGE_scripts

##MODEL example - needs data from fishbase too
m <-glm(b_value  ~ study_year+lat_dec+b_years+tonnesEEZ+landedvalueEEZ+tonnesEEZsp+landedvalueEEZsp+tonnesFEsp+tonnesFE+landedvalueFE+catchdepFEsp+landdepFEsp+spvalueFE, data=latitude)
summary(m)

