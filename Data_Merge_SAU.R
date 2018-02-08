##Elena Ojea
#Code for EEZ and species list from the review - matching EEZ data
#input files: 
# - Biblio_database.csv for the list of EEZs and species per EEZ with fishbase (ReviewDatsp)
# - Final_SAU_EEZ
#output files:
# - ReviewDat_Merge_SAU with EEZ catches
# - list_FE

#Download the required libraries
library(dplyr)
library(tidyr)
library(ggplot2)

##OPEN DATASETS EEZ and Review with Fishbase (feb 18)

#Read input file: our review database with the fishbase inputs
ReviewDatFB.raw <- read.csv("data/ReviewDatsp.csv", stringsAsFactors=FALSE, header=T, sep = ",") ## biblio_database + fishbase from script integration_fishbase.R


colnames(ReviewDatFB.raw)
ReviewDatFB <- ReviewDatFB.raw [, 2:167]   

#Read input file: the EEZ species catched
Final_SAU_EEZ.raw <- read.csv("data/Final_SAU_EEZ.csv", stringsAsFactors=FALSE, header=T, sep = ";")

##FILTER SAU DATASETS: 5 last years, landings, NAs
Final_SAU_EEZ <- filter(Final_SAU_EEZ.raw, year > 2009, catch_type=="Landings")



## 1. MATCH SPECIES NAMES IN REVIEW AND SAU####

##Check list of un-matchig names
Sp_ReviewDatFB <- as.character(unique(ReviewDatFB$b_scientific_name))  #list species in review
Sp_SAU <- as.character(unique(Final_SAU_EEZ$scientific_name))      #list species in SAU

#compare species in Review and SAU
matchsp <- Sp_ReviewDatFB %in% Sp_SAU
table1 <- data.frame(matchsp, Sp_ReviewDatFB)
table2 <- subset(table1, table1$matchsp==FALSE) ##species from review database missing in Sea Around Us
table2  #list of unmatching species

#remove blank spaces from variable values in species
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
ReviewDatFB$b_scientific_name <- trim(ReviewDatFB$b_scientific_name)

#species level changes
ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Atheresthes\240stomias"] <- "Atheresthes stomias"
ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Lepidopsetta\240polyxystra"] <- "Lepidopsetta bilineata"
ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Atherina presbyter"] <- "Atherina boyeri"
ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Loligo opalescens"] <- "Doryteuthis opalescens"
ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Stenotomus caprinus"] <- "Stenotomus chrysops"
ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Clupea pallasii"] <- "Clupea pallasii pallasii"
ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Scophthalmidae"] <- "Scophthalmus aquosus"
ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Isopsetta isolepis"] <- "Eopsetta jordani"

#genrus level changes
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Seriola quinqueradiata"] <- "Seriola"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Sebastes viviparus"] <- "Sebastes"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Enchelyopus cimbrius"] <- "Gaidropsarus"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Bathyraja violacea"] <- "Raja"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Leucoraja erinacea"] <- "Raja"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Alosa pseudoharengus"] <- "Alosa"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Alosa sapidissima"] <- "Alosa"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Pseudopleuronectes americanus"] <- "Pleuronectidae"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Scophthalmus aquosus"] <- "Scophthalmidae"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Lophius americanus"] <- "Lophiidae"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Paralichthys dentatus"] <- "Paralichthyidae"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Hippoglossina oblonga"] <- "Paralichthyidae"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Gaidropsarus macrophthalmus"] <- "Gaidropsarus"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Myoxocephalus octodecemspinosus"] <- "Cottidae"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Loligo pealeii"] <- "Loligo"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Sebastes fasciatus"] <- "Sebastes"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Leucoraja ocellata"] <- "Raja"       #because they are <90cm
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Sebastes saxicola"] <- "Sebastes"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Serranus atrobranchus"] <- "Serranus"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Penaeus duorarum"] <- "Penaeidae"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Penaeus aztecus"] <- "Penaeidae"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Amblyraja radiata"] <- "Raja"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Sebastes ruberrimus"] <- "Sebastes"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Sebastes brevispinis"] <- "Sebastes"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Raja rhina"] <- "Rajidae"  #greater than 90cm"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Raja binoculata"] <- "Rajidae"  #greater than 90cm"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Pandalus platyceros"] <- "Pandalus"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Isopsetta isolepis"] <- "Pleuronectidae"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Paralithodes platypus"] <- "Lithodidae"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Pandalus eous"] <- "Pandalus"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Paralithodes aequispinus"] <- "Lithodidae"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Chionoecetes bairdi"] <- "Chionoecetes"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Berryteuthis magister"] <- "Mollusca"  #general but present in the alaska/subartic
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Urophycis regia"] <- "Phycis"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Buglossidium luteum"] <- "Soleidae"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Arnoglossus laterna"] <- "Bothidae"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Peprilus burti"] <- "Stromateidae"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Diplectrum bivittatum"] <- "Serranus"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Cyclopsetta chittendeni"] <- "Paralichthyidae"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Lithodes aequispinus"] <- "Lithodidae"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Sebastolobus alascanus"] <- "Sebastidae"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Chlamys islandica"] <- "Chlamys"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Anchoa hepsetus"] <- "Anchoviella"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Doryteuthis opalescens"] <- "Loligo"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Merluccius productus"] <- "Merluccius"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Sebastes crameri"] <- "Sebastes"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Sebastes goodei"] <- "Sebastes"
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Sebastes paucispinis"] <- "Sebastes"

##Check list of un-matchig names
Sp_ReviewDatFB <- as.character(unique(ReviewDatFB$b_scientific_name))  #list species in review
Sp_SAU <- as.character(unique(Final_SAU_EEZ$scientific_name))      #list species in SAU

#compare species in Review and SAU
matchsp <- Sp_ReviewDatFB %in% Sp_SAU
table1 <- data.frame(matchsp, Sp_ReviewDatFB)
table2 <- subset(table1, table1$matchsp==FALSE) ##species from review database missing in Sea Around Us
table2  #list of unmatching species (should be 3 when including genrus updates, "Chlamys islandica" is not in Final_SAU_EEZ but it is present n Final_SAU_FE so I dont change it)

#Final list of matching species in ReviewDatFB
Sp_ReviewDatFB <- as.character(subset(table1, table1$matchsp==TRUE)[,2]) 



## 2. MATCH EEZ NAMES IN REVIEW AND SAU (area_name)####

#correct EEZ names in Review Database
ReviewDatFB$eez_countries[ReviewDatFB$eez_countries == "USA (Alaska, Arctic)"] <- "USA (Alaska-Subarctic)"
ReviewDatFB$eez_countries[ReviewDatFB$eez_countries == "Spain (mainland, Med and Gulf of Cadiz)"] <- "Spain (mainland Med and Gulf of Cadiz)"
ReviewDatFB$eez_countries[ReviewDatFB$site == "Northeast US shelf"] <- "USA (East Coast)"
ReviewDatFB$eez_countries[ReviewDatFB$eez_countries == "Denmark (North Sea), Germany (North Sea), Netherlands, Norway, United Kingdom (UK), Belgium, Sweden (West Coast)"] <- "Denmark (North Sea), Germany (North Sea), Netherlands, Norway, United Kingdom (UK), Belgium"
ReviewDatFB$eez_countries[ReviewDatFB$eez_countries == "Canada (East Coast)\n"] <- "Canada (East Coast)"
ReviewDatFB$eez_countries[ReviewDatFB$eez_countries == "United Kindom (UK)"] <- "United Kingdom (UK)"

##some observations have no EEZ. I put a EEZ based on location and http://www.marineregions.org/eezmapper.php
##[6] Spain(Northwest), [166-175] France (Atlantic), [176,177] Norway; [178-189] US North East coast. 
##REPASAR FILAS SI SE MODIFICA LA BASE ORIGINAL
ReviewDatFB$eez_countries[6] <- "Spain (Northwest)" 
ReviewDatFB$eez_countries[166:175] <- rep("France (Atlantic Coast)", 10)
ReviewDatFB$eez_countries[176:177] <- rep("Norway", 2)
ReviewDatFB$eez_countries[178:189] <- rep("USA (East Coast)", 12)

#split EEZs and multiply the rows for each EEZ
ReviewDatFB <- ReviewDatFB %>% 
  mutate(eez_countries = strsplit(as.character(eez_countries), ",")) %>% 
  unnest(eez_countries)

#recode colnames in ReviewDatFB to match Final_SAU_EEZ names
colnames(ReviewDatFB)[12] <- "scientific_name"
colnames(ReviewDatFB)[166] <- "area_name"  ##need to change if going back to biblio_database without fishbase

#remove blank spaces from eez variable names in Review
ReviewDatFB$area_name <- trim(ReviewDatFB$area_name)

##Check list of un-matchig EEZs

EEZ_ReviewDatFB  <- unique(ReviewDatFB$area_name) #final list of EEZs in the Review dataset
EEZ_SAU <- unique(Final_SAU_EEZ$area_name)
EEZ_ReviewDatFB %in% EEZ_SAU



# 3. ADD TOTAL CATCH AND LANDINGS BY EEZ####
#EEZs of the review are now the same as in the SAU database
identical(sort(unique(ReviewDatFB$area_name)),sort(unique(Final_SAU_EEZ$area_name)))

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
            
            

ReviewDatFB_SAU1 <- merge(ReviewDatFB, tonlandEEZ, by=c("area_name"), all.x=TRUE)



# 4. ADD TOTAL CATCH AND LANDINGS BY EEZ AND SP####
#gives mean value across years (2010-2014 ) annual tonnes (2010-2014) 
tonlandEEZsp<-Final_SAU_EEZ %>%
  group_by(area_name, scientific_name) %>%
  summarise(tonnesEEZsp=mean(tonnes,na.rm = T),
            landedvalueEEZsp=mean(landed_value,na.rm = T))

splist <- unique(tonlandEEZsp$scientific_name)
Sp_ReviewDatFB %in% splist

#add total EEZ catches and landings per SP to ReviewDat
#add total EEZ landings and landings per sp to ReviewDat
ReviewDatFB_SAU2 <- merge(ReviewDatFB_SAU1, tonlandEEZsp, by=c("area_name","scientific_name"), all.x=TRUE)

#check missing species in tonnesEEZsp and landedvalueEEZsp
na1 <- is.na(ReviewDatFB_SAU2$tonnesEEZsp)
table(na1) #we miss 183 observations
na2 <- is.na(ReviewDatFB_SAU2$landedvalueEEZsp)
table(na2) #we miss 183 observations



# 5. OUTPUT FILES####
list_FE <- unique(Final_SAU_EEZ$fishing_entity)
#write.csv(list_FE, "data/list_FE.csv")
#write.csv(ReviewDatFB_SAU2, "data/ReviewDat_Merge_SAU.csv")
