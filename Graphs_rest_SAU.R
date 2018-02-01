##Graphs for Catch dependency in SAU and Review
#DEPTH
#Elena Ojea
#December 2017


library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)

#Open ReviewDat with SAU data on EEZ and FE:

ReviewDat <- read.csv("data/ReviewDat.csv", stringsAsFactors=FALSE)


#CATCH DEPENDENCY OF FISHING ENTITIES

#1. Species catch dependency on the area
ReviewDat$catchdepFEsp <- ReviewDat$tonnesFEsp/ReviewDat$tonnesFEspT #the dependence of the country species catches on the EEZ species catches
ReviewDat$landdepFEsp <-  ReviewDat$landedvalueFEsp/ReviewDat$landedvalueFEspT #the dependence of the country total SP Value on the EEZ SP catch value 

range(ReviewDat$catchdepFEsp, na.rm=TRUE)
range(ReviewDat$landdepFEsp, na.rm=TRUE) #is the same relation

#2. Country dependency on the species in the area

ReviewDat$catchdepFE <- ReviewDat$tonnesFEsp/ReviewDat$tonnesFE #the dependence of the country species catches on the EEZ species catches
range(ReviewDat$catchdepFE, na.rm=TRUE)

##VALUE OF SPECIES FOR FISHING ENTITIES

###Species VAlue in FE: landed value/tonnes

ReviewDat$spvalueFE  <- ReviewDat$landedvalueFEsp/ReviewDat$tonnesFEsp
range(ReviewDat$spvalueFE, na.rm=TRUE)
quantile(ReviewDat$spvalueFE, na.rm=TRUE)


##CATCH PRESSURE IN EEZ

ReviewDat$catchpresEEZsp <- ReviewDat$tonnesEEZsp/ReviewDat$tonnesEEZ
ReviewDat$landpresEEZsp <- ReviewDat$landedvalueEEZsp/ReviewDat$landedvalueEEZ

range(ReviewDat$catchpresEEZsp, na.rm=TRUE)

#prices in EEZs (something is wrong here, strange numbers!)
ReviewDat$spvalueEEZ <- ReviewDat$tonnesEEZsp/ReviewDat$landedvalueEEZsp 
range(ReviewDat$spvalueEEZ, na.rm=TRUE)
quantile(ReviewDat$spvalueEEZ, na.rm=TRUE)



##PREPARE VARIABLES FOR PLOTS

ReviewDat$b_value <- as.numeric(as.character(ReviewDat$b_value))
#Subsets of the impacts
ReviewDat$b_impact<-as.factor(ReviewDat$b_impact)
ReviewDat$b_value<-as.numeric(as.character(ReviewDat$b_value))

#levels(ReviewDatSAU$b_impact)
levels(ReviewDat$b_impact)<-c("mean lat shift", "mean lat shift", "depth shift", "boundary lat shift", "boundary lat shift", "mean long shift", "mean long shift", "mean lat and long shift", "area occupied")
#remove Nicolas (uds=6)
ReviewDat <- subset(ReviewDat, ReviewDat$id_study!=6)

latitude <- subset (ReviewDat, b_impact =="mean lat shift")
depth    <- subset (ReviewDat, b_impact=="depth shift")
range    <- subset (ReviewDat, b_impact== "boundary lat shift")
area     <- subset (ReviewDat, b_impact == "area occupied")

####RANGE###PLOTS CATCH DEPENDENCY PER SP and FE total SP, per SP and FE total catch
#FE total catch dependency on species in EEZ

#FIGURE 3

range    <- subset (ReviewDat, b_impact== "boundary lat shift")
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






##MODEL example - needs data from fishbase too
m <-glm(b_value  ~ study_year+lat_dec+b_years+tonnesEEZ+landedvalueEEZ+tonnesEEZsp+landedvalueEEZsp+tonnesFEsp+tonnesFE+landedvalueFE+catchdepFEsp+landdepFEsp+spvalueFE, data=depth)
summary(m)

