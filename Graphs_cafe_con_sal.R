#Graphs for café con sal
#Alba Aguión
#17 Mayo 2018
#source: Biblio_database_full.csv

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

options(scipen=999) #remove scientific notation

#Figure 1. Impact, location and mean catch

####LATITUDE

Fig1LAT <- ggplot(latitude, aes(area_name,b_value))+
  geom_point(aes(size=tonnesEEZ), color="royalblue")+
  theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'), axis.text.x = element_text(size=8, angle=-45, hjust= 0.06))+
  scale_y_continuous(name="Mean Latitude Shift Rate (Km/decade)")+ scale_x_discrete(name ="EEZ")+
  labs(size="Mean catch (tonnes/year)\n2010-2014\n");Fig1LAT

Fig1LAT + geom_text_repel(data=subset(latitude, b_value > 100),aes(area_name,b_value,label=scientific_name),vjust="inward",hjust="inward")
Fig1LAT + geom_text_repel(data=subset(latitude, b_value < -50),aes(area_name,b_value,label=scientific_name),vjust="inward",hjust="inward")

####DEPTH

Fig1DEPTH <- ggplot(depth, aes(area_name,b_value))+
  geom_point(aes(size=tonnesEEZ), color="royalblue")+
  theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'), axis.text.x = element_text(size=8, angle=-45, hjust= 0.06))+
  scale_y_continuous(name="Depth Shift Rate (m/decade)")+ scale_x_discrete(name ="EEZ")+
  labs(size="Mean catch (tonnes/year)\n2010-2014\n");Fig1DEPTH

Fig1DEPTH + geom_text_repel(data=subset(depth, b_value > 35),aes(area_name,b_value,label=scientific_name),vjust="inward",hjust="inward")
Fig1DEPTH + geom_text_repel(data=subset(depth, b_value < -35),aes(area_name,b_value,label=scientific_name),vjust="inward",hjust="inward")


#Figure 2. Impact and FE catch dependency

####LATITUDE

Fig2LAT <- ggplot(latitude, aes(catchdepFE, b_value, label = scientific_name)) +
  geom_jitter(alpha=0.8, aes(color=fishing_entity, size=tonnesFEsp),
              position = position_jitter(width = .001))+
  geom_text_repel(data=subset(latitude, latitude$catchdepFE>0.15), size=3, vjust=1)+
  geom_text_repel(data=subset(latitude, latitude$b_value>150), size=3, vjust=1)+
  xlab("FE total catch dependency on species in EEZ")+
  ylab("Mean Latitude Shift Rate (km/decade)")+
  theme(legend.position="bottom", panel.background = element_rect(fill = 'whitesmoke', colour = 'black'))+
  labs(size="Mean catch (tonnes/year)\n2010-2014\n", color="Fishing Entity")

Fig2LAT

####DEPTH

Fig2DEPTH <- ggplot(depth, aes(catchdepFE, b_value, label = scientific_name)) +
  geom_jitter(alpha=0.8, aes(color=fishing_entity, size=tonnesFEsp),
              position = position_jitter(width = .001))+
  geom_text_repel(data=subset(depth, depth$catchdepFE>0.10), size=3, vjust=1)+
  geom_text_repel(data=subset(depth, depth$b_value>40), size=3, vjust=1)+
  geom_text_repel(data=subset(depth, depth$b_value< -35), size=3, vjust=1)+
  xlab("FE total catch dependency on species in EEZ")+
  ylab("Depth Shift Rate (m/decade)")+
  theme(legend.position="bottom", panel.background = element_rect(fill = 'whitesmoke', colour = 'black'))+
  labs(size="Mean catch (tonnes/year)\n2010-2014\n", color="Fishing Entity")

Fig2DEPTH

