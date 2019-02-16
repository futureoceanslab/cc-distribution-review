##Graphs for Catch dependency in SAU and Review
#DEPTH
#Elena Ojea
#December 2017


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

a<-filter(Biblio_data,catchdepFEEZ>1)

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

Biblio_data$b_value <- as.numeric(as.character(Biblio_data$b_value))
#Subsets of the impacts
Biblio_data$b_impact<-as.factor(Biblio_data$b_impact)
Biblio_data$b_value<-as.numeric(as.character(Biblio_data$b_value))

#levels(Biblio_dataSAU$b_impact)
levels(Biblio_data$b_impact)<-c("mean lat shift", "mean lat shift", "depth shift", "boundary lat shift", "boundary lat shift", "mean long shift", "mean long shift", "mean lat and long shift", "area occupied")
#remove Nicolas (uds=6)
Biblio_data <- subset(Biblio_data, Biblio_data$id_study!=6)

latitude <- subset (Biblio_data, b_impact =="mean lat shift")
depth    <- subset (Biblio_data, b_impact=="depth shift")
range    <- subset (Biblio_data, b_impact== "boundary lat shift")
area     <- subset (Biblio_data, b_impact == "area occupied")

####RANGE###PLOTS CATCH DEPENDENCY PER SP and FE total SP, per SP and FE total catch
#FE total catch dependency on species in EEZ

#FIGURE 3

range    <- subset (Biblio_data, b_impact== "boundary lat shift")
l3 <- ggplot(range, aes(fishing_entity, b_value, label = scientific_name, na.rm=TRUE)) +
  #geom_point()
  geom_jitter(alpha=0.8, aes(color=catchdepFEsp, size=tonnesFEsp),
              position = position_jitter(width = .001))+
  theme(axis.text.x = element_text(angle=-45, hjust= 0.06))+
  #geom_text_repel(data=subset(range, range$tonnesFEsp>20000), size=3, vjust=1)+
  ggtitle("range impacts in fishing entities")+
  xlab("Fishing Entities")+
  ylab("km/decade")
l3



##using fishing entities in the X Axis
#FIGURE 4

l4 <- ggplot(range, aes(fishing_entity,catchdepFE, label=scientific_name))+
  geom_point(aes(color=b_value, size=tonnesFEsp))+
  geom_text_repel(data=subset(range, depth$catchdepFE>0.05), size=2, vjust=1)+
  theme(axis.text.x = element_text(angle=-45, hjust= 0.06))+
  scale_color_gradient(low = "blue", high = "red")+
  ggtitle("Countries species dependence")
l4


##using scientific names in the X Axis
#FIGURE 9
l9 <- ggplot(range, aes(scientific_name,b_value, label=fishing_entity))+
  geom_point(aes(color=catchpresEEZsp, size=tonnesEEZ))+
  #geom_text_repel(data=subset(range, range$catchdepFE>0.05), size=3, vjust=1)+
  theme(axis.text.x = element_text(size=6, angle=-45, hjust= 0.06))+
  scale_color_gradient(low = "blue", high = "red")
l9


####AREA###PLOTS CATCH DEPENDENCY PER SP and FE total SP, per SP and FE total catch
#FE total catch dependency on species in EEZ

#FIGURE 3


l3 <- ggplot(area, aes(fishing_entity, b_value, label = scientific_name, na.rm=TRUE)) +
  #geom_point()
  geom_jitter(alpha=0.8, aes(color=catchdepFEsp, size=tonnesFEsp),
              position = position_jitter(width = .001))+
  theme(axis.text.x = element_text(angle=-45, hjust= 0.06))+
  #geom_text_repel(data=subset(area, range$tonnesFEsp>20000), size=3, vjust=1)+
  ggtitle("area impacts in fishing entities")+
  xlab("Fishing Entities")+
  ylab("km/decade")
l3



##using fishing entities in the X Axis
#FIGURE 4

l4 <- ggplot(area, aes(fishing_entity,catchdepFE, label=scientific_name))+
  geom_point(aes(color=b_value, size=tonnesFEsp))+
  geom_text_repel(data=subset(area, depth$catchdepFE>0.05), size=2, vjust=1)+
  theme(axis.text.x = element_text(angle=-45, hjust= 0.06))+
  scale_color_gradient(low = "blue", high = "red")+
  ggtitle("Countries species dependence")
l4


##using scientific names in the X Axis
#FIGURE 9
l9 <- ggplot(area, aes(scientific_name,b_value, label=fishing_entity))+
  geom_point(aes(color=catchpresEEZsp, size=tonnesEEZ))+
  #geom_text_repel(data=subset(area, range$catchdepFE>0.05), size=3, vjust=1)+
  theme(axis.text.x = element_text(size=6, angle=-45, hjust= 0.06))+
  scale_color_gradient(low = "blue", high = "red")
l9




