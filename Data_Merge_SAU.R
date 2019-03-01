##Elena Ojea, February 2019
#Code for EEZ and species list from the review - matching EEZ data
#input files: 
# - Integration output: ReviewDatsp.csv biblio data + fishbase species info from integration_fishbase.R code
# - Data_EEZ_SAU output: Final_SAU_EEZ
#output files:
# - ReviewDat_Merge_SAU with EEZ catches
# - list_FE

#Download the required libraries
library(dplyr)
library(tidyr)
library(ggplot2)

##1. OPEN DATASETS EEZ and Review with Fishbase (feb 18)
#Read input file: our review database with the fishbase inputs
ReviewDatFB <- read.csv("data/ReviewDatsp.csv", stringsAsFactors=FALSE, header=T) ## biblio_database + fishbase from script integration_fishbase.R
 
#Read input file: the EEZ species catched
Final_SAU_EEZ.raw <- read.csv("data/Final_SAU_EEZ.csv", stringsAsFactors=FALSE, header=T)

##FILTER SAU DATASETS: 5 last years, landings, NAs
Final_SAU_EEZ <- filter(Final_SAU_EEZ.raw, year > 2009, catch_type=="Landings")


##2. MATCH SPECIES NAMES IN REVIEW AND SAU####
##Check list of un-matchig names
Sp_ReviewDatFB <- as.character(unique(ReviewDatFB$b_scientific_name))  #list species in review
Sp_SAU <- as.character(unique(Final_SAU_EEZ$scientific_name))      #list species in SAU

#compare species in Review and SAU
matchsp <- Sp_ReviewDatFB %in% Sp_SAU
table(matchsp) ## 33 spp no macth, 109 spp macthed (total:142spp) *will be updated with new code March 2019
spmiss1 <- Sp_ReviewDatFB[matchsp==FALSE] ## list of unmatching(lost) species


##3. MATCH COLUMNS and EEZ NAMES IN REVIEW AND SAU (area_name)####

#recode colnames in ReviewDatFB to match Final_SAU_EEZ names
colnames(ReviewDatFB)[which(names(ReviewDatFB) == "b_scientific_name")] <- "scientific_name"
colnames(ReviewDatFB)[which(names(ReviewDatFB) == "eez_countries")] <- "area_name"

##Check list of un-matchig EEZs
EEZ_ReviewDatFB  <- unique(ReviewDatFB$area_name) #final list of EEZs in the Review dataset
EEZ_SAU <- unique(Final_SAU_EEZ$area_name)
EEZ_ReviewDatFB %in% EEZ_SAU
#EEZs of the review are now the same as in the SAU database
identical(sort(unique(ReviewDatFB$area_name)),sort(unique(Final_SAU_EEZ$area_name)))

##5. MERGE: ADD TOTAL CATCH AND LANDINGS BY EEZ####
#check observations for EEZ data
counts <- Final_SAU_EEZ %>%
  group_by(area_name, year) %>%
  tally

#dataframe total EEZ catch 
#sum all catches per area_name and year
tonlandEEZyear<-Final_SAU_EEZ %>%
            group_by(area_name,year) %>%
            summarise(tonnesEEZyear=sum(tonnes,na.rm = T),
                      landedvalueEEZyear=sum(landed_value,na.rm = T))
#take mean value across years (2010-2014)    
tonlandEEZ<- tonlandEEZyear %>%
  group_by(area_name) %>%
  summarise(tonnesEEZ=mean(tonnesEEZyear,na.rm = T),
            landedvalueEEZ=mean(landedvalueEEZyear,na.rm = T))          
#merge
ReviewDatFB_SAU1 <- merge(ReviewDatFB, tonlandEEZ, by=c("area_name"), all.x=TRUE)


##4. MERGE: ADD TOTAL CATCH AND LANDINGS BY EEZ AND SP####
#gives mean value across years (2010-2014 ) annual tonnes (2010-2014) 
tonlandEEZspyear<-Final_SAU_EEZ %>%
  group_by(area_name, year, scientific_name) %>%
  summarise(tonnesEEZspyear=sum(tonnes,na.rm = T),
            landedvalueEEZspyear=sum(landed_value,na.rm = T))
tonlandEEZsp<-tonlandEEZspyear %>%
  group_by(area_name, scientific_name) %>%
  summarise(tonnesEEZsp=mean(tonnesEEZspyear,na.rm = T),
            landedvalueEEZsp=mean(landedvalueEEZspyear,na.rm = T))
#merge: add total EEZ catches and landings per SP to ReviewDat
ReviewDatFB_SAU2 <- merge(ReviewDatFB_SAU1, tonlandEEZsp, by=c("area_name","scientific_name"), all.x=TRUE)


##5. Doble-check
##Species name match - tonlandEEZsp
splist <- unique(tonlandEEZsp$scientific_name)
matchsp2<- Sp_ReviewDatFB %in% splist
table(matchsp2) ## 33 spp no macth, 109 spp macthed (total:142spp). Same resultas above
spmiss2 <- Sp_ReviewDatFB[matchsp2==FALSE] ## list of unmatching(lost) species
##To chek the matches among lists and edit the list of spp lost
identical(spmiss1,spmiss2)
#write.csv(spmiss2, file="data/2listspmiss.csv") ##list of lost species


#check missing species in tonnesEEZsp and landedvalueEEZsp
#Not all the species have data for all the years/EEZs
na1 <- is.na(ReviewDatFB_SAU2$tonnesEEZsp)
table(na1) #we miss 200 observations, False 395, True 200
na2 <- is.na(ReviewDatFB_SAU2$landedvalueEEZsp)
table(na2) #we miss 200 observations, False 395, True 200


##6. OUTPUT FILES####
list_FE <- unique(Final_SAU_EEZ$fishing_entity)
write.csv(list_FE, "data/list_FE.csv", row.names = F)
write.csv(ReviewDatFB_SAU2, "data/ReviewDat_Merge_SAU.csv", row.names = F)
