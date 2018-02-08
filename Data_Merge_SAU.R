##Elena Ojea
#Code for EEZ and species list from the review - matching EEZ data
#input files: 
# - Biblio_database.csv for the list of EEZs and species per EEZ with fishbase (ReviewDat_SAUsp)
# - Final_SAU_EEZ
#output files:
# - ReviewDat_SAU_Merge_SAU with EEZ catches
# - list_FE

#Download the required libraries
library(dplyr)
library(tidyr)
library(ggplot2)

##OPEN DATASETS EEZ and Review with Fishbase (feb 18)

#Read input file: our review database
ReviewDat.raw <- read.csv("data/biblio_database.csv", stringsAsFactors=FALSE, header=T, sep = ",")

colnames(ReviewDat.raw)
ReviewDat_SAU <- ReviewDat.raw[, 1:68] #to erase empty columns

#Read input file: the EEZ species catched
Final_SAU_EEZ.raw <- read.csv("data/Final_SAU_EEZ.csv", stringsAsFactors=FALSE, header=T, sep = ";")

##FILTER SAU DATASETS: 5 last years, landings, NAs
Final_SAU_EEZ <- filter(Final_SAU_EEZ.raw, year > 2009, catch_type=="Landings")



## 1. MATCH SPECIES NAMES IN REVIEW AND SAU####

##Check list of un-matchig names
Sp_ReviewDat_SAU <- as.character(unique(ReviewDat_SAU$b_scientific_name))  #list species in review
Sp_SAU <- as.character(unique(Final_SAU_EEZ$scientific_name))      #list species in SAU

#compare species in Review and SAU
matchsp <- Sp_ReviewDat_SAU %in% Sp_SAU
table1 <- data.frame(matchsp, Sp_ReviewDat_SAU)
table2 <- subset(table1, table1$matchsp==FALSE) ##species from review database missing in Sea Around Us
table2  #list of unmatching species

#remove blank spaces from variable values in species
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
ReviewDat_SAU$b_scientific_name <- trim(ReviewDat_SAU$b_scientific_name)

#species level changes
ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Atheresthes\240stomias"] <- "Atheresthes stomias"
ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Lepidopsetta\240polyxystra"] <- "Lepidopsetta bilineata"
ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Atherina presbyter"] <- "Atherina boyeri"
ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Loligo opalescens"] <- "Doryteuthis opalescens"
ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Stenotomus caprinus"] <- "Stenotomus chrysops"
ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Clupea pallasii"] <- "Clupea pallasii pallasii"
ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Scophthalmidae"] <- "Scophthalmus aquosus"
ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Isopsetta isolepis"] <- "Eopsetta jordani"

#genrus level changes
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Seriola quinqueradiata"] <- "Seriola"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Sebastes viviparus"] <- "Sebastes"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Enchelyopus cimbrius"] <- "Gaidropsarus"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Bathyraja violacea"] <- "Raja"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Leucoraja erinacea"] <- "Raja"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Alosa pseudoharengus"] <- "Alosa"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Alosa sapidissima"] <- "Alosa"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Pseudopleuronectes americanus"] <- "Pleuronectidae"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Scophthalmus aquosus"] <- "Scophthalmidae"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Lophius americanus"] <- "Lophiidae"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Paralichthys dentatus"] <- "Paralichthyidae"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Hippoglossina oblonga"] <- "Paralichthyidae"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Gaidropsarus macrophthalmus"] <- "Gaidropsarus"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Myoxocephalus octodecemspinosus"] <- "Cottidae"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Loligo pealeii"] <- "Loligo"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Sebastes fasciatus"] <- "Sebastes"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Leucoraja ocellata"] <- "Raja"       #because they are <90cm
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Sebastes saxicola"] <- "Sebastes"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Serranus atrobranchus"] <- "Serranus"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Penaeus duorarum"] <- "Penaeidae"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Penaeus aztecus"] <- "Penaeidae"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Amblyraja radiata"] <- "Raja"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Sebastes ruberrimus"] <- "Sebastes"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Sebastes brevispinis"] <- "Sebastes"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Raja rhina"] <- "Rajidae"  #greater than 90cm"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Raja binoculata"] <- "Rajidae"  #greater than 90cm"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Pandalus platyceros"] <- "Pandalus"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Isopsetta isolepis"] <- "Pleuronectidae"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Paralithodes platypus"] <- "Lithodidae"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Pandalus eous"] <- "Pandalus"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Paralithodes aequispinus"] <- "Lithodidae"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Chionoecetes bairdi"] <- "Chionoecetes"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Berryteuthis magister"] <- "Mollusca"  #general but present in the alaska/subartic
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Urophycis regia"] <- "Phycis"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Buglossidium luteum"] <- "Soleidae"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Arnoglossus laterna"] <- "Bothidae"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Peprilus burti"] <- "Stromateidae"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Diplectrum bivittatum"] <- "Serranus"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Cyclopsetta chittendeni"] <- "Paralichthyidae"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Lithodes aequispinus"] <- "Lithodidae"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Sebastolobus alascanus"] <- "Sebastidae"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Chlamys islandica"] <- "Chlamys"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Anchoa hepsetus"] <- "Anchoviella"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Doryteuthis opalescens"] <- "Loligo"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Merluccius productus"] <- "Merluccius"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Sebastes crameri"] <- "Sebastes"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Sebastes goodei"] <- "Sebastes"
#ReviewDat_SAU$b_scientific_name[ReviewDat_SAU$b_scientific_name=="Sebastes paucispinis"] <- "Sebastes"

##Check list of un-matchig names
Sp_ReviewDat_SAU <- as.character(unique(ReviewDat_SAU$b_scientific_name))  #list species in review
Sp_SAU <- as.character(unique(Final_SAU_EEZ$scientific_name))      #list species in SAU

#compare species in Review and SAU
matchsp <- Sp_ReviewDat_SAU %in% Sp_SAU
table1 <- data.frame(matchsp, Sp_ReviewDat_SAU)
table2 <- subset(table1, table1$matchsp==FALSE) ##species from review database missing in Sea Around Us
table2  #list of unmatching species (should be 3 when including genrus updates, "Chlamys islandica" is not in Final_SAU_EEZ but it is present n Final_SAU_FE so I dont change it)

#Final list of matching species in ReviewDat_SAU
Sp_ReviewDat_SAU <- as.character(subset(table1, table1$matchsp==TRUE)[,2]) 



## 2. MATCH EEZ NAMES IN REVIEW AND SAU (area_name)####

#correct EEZ names in Review Database
ReviewDat_SAU$eez_countries[ReviewDat_SAU$eez_countries == "USA (Alaska, Arctic)"] <- "USA (Alaska-Subarctic)"
ReviewDat_SAU$eez_countries[ReviewDat_SAU$eez_countries == "Spain (mainland, Med and Gulf of Cadiz)"] <- "Spain (mainland Med and Gulf of Cadiz)"
ReviewDat_SAU$eez_countries[ReviewDat_SAU$site == "Northeast US shelf"] <- "USA (East Coast)"
ReviewDat_SAU$eez_countries[ReviewDat_SAU$eez_countries == "Denmark (North Sea), Germany (North Sea), Netherlands, Norway, United Kingdom (UK), Belgium, Sweden (West Coast)"] <- "Denmark (North Sea), Germany (North Sea), Netherlands, Norway, United Kingdom (UK), Belgium"
ReviewDat_SAU$eez_countries[ReviewDat_SAU$eez_countries == "Canada (East Coast)\n"] <- "Canada (East Coast)"
ReviewDat_SAU$eez_countries[ReviewDat_SAU$eez_countries == "United Kindom (UK)"] <- "United Kingdom (UK)"

##some observations have no EEZ. I put a EEZ based on location and http://www.marineregions.org/eezmapper.php
##[6] Spain(Northwest), [166-175] France (Atlantic), [176,177] Norway; [178-189] US North East coast. 
##REPASAR FILAS SI SE MODIFICA LA BASE ORIGINAL
ReviewDat_SAU$eez_countries[6] <- "Spain (Northwest)" 
ReviewDat_SAU$eez_countries[166:175] <- rep("France (Atlantic Coast)", 10)
ReviewDat_SAU$eez_countries[176:177] <- rep("Norway", 2)
ReviewDat_SAU$eez_countries[178:189] <- rep("USA (East Coast)", 12)

#split EEZs and multiply the rows for each EEZ
ReviewDat_SAU <- ReviewDat_SAU %>% 
  mutate(eez_countries = strsplit(as.character(eez_countries), ",")) %>% 
  unnest(eez_countries)

#recode colnames in ReviewDat_SAU to match Final_SAU_EEZ names
colnames(ReviewDat_SAU)[12] <- "scientific_name"
colnames(ReviewDat_SAU)[166] <- "area_name"  ##need to change if going back to biblio_database without fishbase

#remove blank spaces from eez variable names in Review
ReviewDat_SAU$area_name <- trim(ReviewDat_SAU$area_name)

##Check list of un-matchig EEZs

EEZ_ReviewDat_SAU  <- unique(ReviewDat_SAU$area_name) #final list of EEZs in the Review dataset
EEZ_SAU <- unique(Final_SAU_EEZ$area_name)
EEZ_ReviewDat_SAU %in% EEZ_SAU



# 3. ADD TOTAL CATCH AND LANDINGS BY EEZ####
#EEZs of the review are now the same as in the SAU database
identical(sort(unique(ReviewDat_SAU$area_name)),sort(unique(Final_SAU_EEZ$area_name)))

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
            
            

ReviewDat_SAU <- merge(ReviewDat_SAU, tonlandEEZ, by=c("area_name"), all.x=TRUE)



# 4. ADD TOTAL CATCH AND LANDINGS BY EEZ AND SP####
#gives mean value across years (2010-2014 ) annual tonnes (2010-2014) 
tonlandEEZsp<-Final_SAU_EEZ %>%
  group_by(area_name, scientific_name) %>%
  summarise(tonnesEEZsp=mean(tonnes,na.rm = T),
            landedvalueEEZsp=mean(landed_value,na.rm = T))

splist <- unique(tonlandEEZsp$scientific_name)
Sp_ReviewDat_SAU %in% splist

#add total EEZ catches and landings per SP to ReviewDat_SAU
#add total EEZ landings and landings per sp to ReviewDat_SAU
ReviewDat_SAU <- merge(ReviewDat_SAU, tonlandEEZsp, by=c("area_name","scientific_name"), all.x=TRUE)

#check missing species in tonnesEEZsp and landedvalueEEZsp
na1 <- is.na(ReviewDat_SAU$tonnesEEZsp)
table(na1) #we miss 183 observations
na2 <- is.na(ReviewDat_SAU$landedvalueEEZsp)
table(na2) #we miss 183 observations



# 5. OUTPUT FILES####
list_FE <- unique(Final_SAU_EEZ$fishing_entity)
#write.csv(list_FE, "data/list_FE.csv")
#write.csv(ReviewDat_SAU, "data/ReviewDat_Merge_SAU.csv")
