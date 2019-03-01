##Run Data SAU if needed to upload Final_SAU_FE
##OPEN FE DATA (from Data_SAU.R using list_FE.csv)
#Read input file: the FE species catched
##Elenaupdated March 2019

library(tidyverse)

Final_SAU_FE <- read.csv("data/Final_SAU_FE.csv", stringsAsFactors=FALSE)
ReviewDat_Merge_SAU <- read.csv("data/ReviewDat_Merge_SAU.csv", stringsAsFactors=FALSE)


##FILTER SAU DATASETS: 5 last years, landings, NAs
Final_SAU_FE <- filter(Final_SAU_FE, year > 2009, catch_type=="Landings")

#dataframe sp - EEZ catch
counts <- Final_SAU_FE %>%
  group_by(fishing_entity, year) %>%
  tally  


# 1. FISHING ENTITIES#### 

#add fishing entities landings and catches per species to ReviewDat 

#Catches and Landings per fishing entity, EEZ and species (mean across years)
tonlandFEspyear <- Final_SAU_FE %>%
  group_by(fishing_entity, area_name, year, scientific_name) %>%
  summarise(tonnesFEspyear=sum(tonnes,na.rm=T),
            landedvalueFEspyear=sum(landed_value, na.rm=T))

tonlandFEsp <- tonlandFEspyear %>% #borrar bis
  group_by(fishing_entity, area_name, scientific_name) %>%
  summarise(tonnesFEsp=mean(tonnesFEspyear,na.rm=T),
            landedvalueFEsp=mean(landedvalueFEspyear, na.rm=T))

ReviewDatFB_SAU3  <- merge(ReviewDat_Merge_SAU, tonlandFEsp, by=c("area_name","scientific_name"), all.x=TRUE)

#Verification of EEZ names and species
a<-unique(ReviewDatFB_SAU3$area_name)
b<-unique(ReviewDatFB_SAU3$scientific_name)
c<-unique(tonlandFEsp$area_name)
d<-unique(tonlandFEsp$scientific_name)
a %in% c    #checking EEZ names match, all TRUE -> they all macth
spmiss<-b[which(!b %in% d)]   ## list of unmatching(lost) species, 29 spp no macth, 116 spp macthed (total:142spp)
#write.csv(spmiss, file="data/3listspmiss.csv") ##list of lost species

#total catch per species for Fishing entities (sum across species and eezs)
tonlandFEspT<- tonlandFEsp %>%
              group_by(fishing_entity, scientific_name) %>%
              summarise(tonnesFEspT=sum(tonnesFEsp,na.rm=T),
                        landedvalueFEspT=sum(landedvalueFEsp, na.rm=T))

ReviewDatFB_SAU4 <- merge(ReviewDatFB_SAU3, tonlandFEspT, by=c("fishing_entity", "scientific_name"), all.x=TRUE)


#Total catch per fishing entity (mean across fishing entities)
tonlandFE<- tonlandFEspT %>%
  group_by(fishing_entity) %>%
  summarise(tonnesFE=sum(tonnesFEspT,na.rm=T),
            landedvalueFE=sum(landedvalueFEspT, na.rm=T))

ReviewDatFB_SAU5 <- merge(ReviewDatFB_SAU4, tonlandFE, by=c("fishing_entity"), all.x=TRUE)

#Total catch per fishing entity in EEZ (annual mean 2010-2014)
tonlandFEEZyear <- Final_SAU_FE %>%
  group_by(fishing_entity, area_name, year) %>%
  summarise(tonnesFEEZyear=sum(tonnes,na.rm=T),
            landedvalueFEZZyear=sum(landed_value, na.rm=T))

tonlandFEEZ <- tonlandFEEZyear %>% 
  group_by(fishing_entity, area_name) %>%
  summarise(tonnesFEEZ=mean(tonnesFEEZyear,na.rm=T),
            landedvalueFEEZ=mean(landedvalueFEZZyear, na.rm=T))

Biblio_data <- merge(ReviewDatFB_SAU5, tonlandFEEZ, by=c("fishing_entity", "area_name"), all.x=TRUE)
#Database_full: Biblio_data with SAU data on EEZ and FE:


#2. CATCH DEPENDENCY OF FISHING ENTITIES

#2.1. Species catch dependency on the area
Biblio_data$catchdepFEsp <- Biblio_data$tonnesFEsp/Biblio_data$tonnesFEspT #the dependence of the country species catches on the EEZ species catches
Biblio_data$landdepFEsp <-  Biblio_data$landedvalueFEsp/Biblio_data$landedvalueFEspT #the dependence of the country total SP Value on the EEZ SP catch value 

range(Biblio_data$catchdepFEsp, na.rm=TRUE)
range(Biblio_data$landdepFEsp, na.rm=TRUE) #is the same relation

#2.2. Country dependency on the species in the area

Biblio_data$catchdepFE <- Biblio_data$tonnesFEsp/Biblio_data$tonnesFE #the dependence of the country species catches on the EEZ species catches
range(Biblio_data$catchdepFE, na.rm=TRUE)

#2.3. Country dependency on the area

Biblio_data$catchdepFEEZ <- Biblio_data$tonnesFEEZ/Biblio_data$tonnesEEZ 
range(Biblio_data$catchdepFEEZ, na.rm=TRUE)

##3VALUE OF SPECIES FOR FISHING ENTITIES

###3.1 Species VAlue in FE: landed value/tonnes

Biblio_data$spvalueFE  <- Biblio_data$landedvalueFEsp/Biblio_data$tonnesFEsp
range(Biblio_data$spvalueFE, na.rm=TRUE)
quantile(Biblio_data$spvalueFE, na.rm=TRUE)

##4. CATCH PRESSURE IN EEZ

Biblio_data$catchpresEEZsp <- Biblio_data$tonnesEEZsp/Biblio_data$tonnesEEZ
Biblio_data$landpresEEZsp <- Biblio_data$landedvalueEEZsp/Biblio_data$landedvalueEEZ

range(Biblio_data$catchpresEEZsp, na.rm=TRUE)

#4.1 prices in EEZs (value of 1 tone species in EEZ, is it ok?)
Biblio_data$spvalueEEZ <- Biblio_data$landedvalueEEZsp /Biblio_data$tonnesEEZsp
range(Biblio_data$spvalueEEZ, na.rm=TRUE)
quantile(Biblio_data$spvalueEEZ, na.rm=TRUE)

##5. OUTPUT FILE###
write.csv(Biblio_data, file = "data/Biblio_database_full.csv", row.names = F)

