##december 17
##Elena Ojea
#Code for EEZ and species list from the review - matching EEZ data
#input files: 
# - Biblio_database.csv for the list of EEZs and species per EEZ
# - Final_SAU_FE and Final_SAU_EEZ
#output file: ReviewDat with EEZ and FE catches

#Download the required libraries
library(dplyr)
library(tidyr)
library(ggplot2)

##OPEN DATASETS

#Read input file: our review database
ReviewDat <- read.csv("data/biblio_database1.csv", stringsAsFactors=FALSE,  header=T, sep = ";")
colnames(ReviewDat)
ReviewDat <- ReviewDat[, 1:68] #to erase empty columns

#Read input file: the EEZ species catched
Final_SAU_EEZ <- read.csv("data/Final SAU EEZ.csv", stringsAsFactors=FALSE, header=T, sep = ";")

#Read input file: the FE species catched
Final_SAU_FE <- read.csv("data/Final SAU FE.csv", stringsAsFactors=FALSE)


##FILTER SAU DATASETS: 5 last years, landings, NAs

Final_SAU_EEZ <- filter(Final_SAU_EEZ, year > 2009, catch_type=="Landings")
Final_SAU_EEZ[rowSums(is.na(Final_SAU_EEZ)) != ncol(Final_SAU_EEZ),]

Final_SAU_FE <- filter(Final_SAU_FE, year > 2009, catch_type=="Landings")
#Final_SAU_FE[rowSums(is.na(Final_SAU_FE)) != ncol(Final_SAU_FE),]


## 1. MATCH SPECIES NAMES IN REVIEW AND SAU

##Check list of un-matchig names

Sp_ReviewDat <- as.character(unique(ReviewDat$b_scientific_name))  #list species in review
Sp_SAU <- as.character(unique(Final_SAU_EEZ$scientific_name))      #list species in SAU

#compare species in Review and SAU
matchsp <- Sp_ReviewDat %in% Sp_SAU
table1 <- data.frame(matchsp, Sp_ReviewDat)
table2 <- subset(table1, table1$matchsp==FALSE) ##species from review database missing in Sea Around Us
table2  #list of unmatching species

#remove blank spaces from variable values in species

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
ReviewDat$b_scientific_name <- trim(ReviewDat$b_scientific_name)

#species level changes
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Atheresthes\240stomias"] <- "Atheresthes stomias"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Lepidopsetta\240polyxystra"] <- "Lepidopsetta bilineata"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Atherina presbyter"] <- "Atherina boyeri"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Loligo opalescens"] <- "Doryteuthis opalescens"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Stenotomus caprinus"] <- "Stenotomus chrysops"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Clupea pallasii"] <- "Clupea pallasii pallasii"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Scophthalmidae"] <- "Scophthalmus aquosus"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Isopsetta isolepis"] <- "Eopsetta jordani"

#genrus level changes
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Seriola quinqueradiata"] <- "Seriola"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Sebastes viviparus"] <- "Sebastes"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Enchelyopus cimbrius"] <- "Gaidropsarus"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Bathyraja violacea"] <- "Raja"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Leucoraja erinacea"] <- "Raja"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Alosa pseudoharengus"] <- "Alosa"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Alosa sapidissima"] <- "Alosa"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Pseudopleuronectes americanus"] <- "Pleuronectidae"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Scophthalmus aquosus"] <- "Scophthalmidae"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Lophius americanus"] <- "Lophiidae"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Paralichthys dentatus"] <- "Paralichthyidae"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Hippoglossina oblonga"] <- "Paralichthyidae"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Gaidropsarus macrophthalmus"] <- "Gaidropsarus"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Myoxocephalus octodecemspinosus"] <- "Cottidae"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Loligo pealeii"] <- "Loligo"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Sebastes fasciatus"] <- "Sebastes"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Leucoraja ocellata"] <- "Raja"       #because they are <90cm
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Sebastes saxicola"] <- "Sebastes"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Serranus atrobranchus"] <- "Serranus"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Penaeus duorarum"] <- "Penaeidae"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Penaeus aztecus"] <- "Penaeidae"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Amblyraja radiata"] <- "Raja"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Sebastes ruberrimus"] <- "Sebastes"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Sebastes brevispinis"] <- "Sebastes"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Raja rhina"] <- "Rajidae"  #greater than 90cm"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Raja binoculata"] <- "Rajidae"  #greater than 90cm"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Pandalus platyceros"] <- "Pandalus"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Isopsetta isolepis"] <- "Pleuronectidae"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Paralithodes platypus"] <- "Lithodidae"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Pandalus eous"] <- "Pandalus"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Paralithodes aequispinus"] <- "Lithodidae"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Chionoecetes bairdi"] <- "Chionoecetes"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Berryteuthis magister"] <- "Mollusca"  #general but present in the alaska/subartic
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Urophycis regia"] <- "Phycis"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Buglossidium luteum"] <- "Soleidae"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Arnoglossus laterna"] <- "Bothidae"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Peprilus burti"] <- "Stromateidae"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Diplectrum bivittatum"] <- "Serranus"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Cyclopsetta chittendeni"] <- "Paralichthyidae"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Lithodes aequispinus"] <- "Lithodidae"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Sebastolobus alascanus"] <- "Sebastidae"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Chlamys islandica"] <- "Chlamys"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Anchoa hepsetus"] <- "Anchoviella"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Doryteuthis opalescens"] <- "Loligo"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Merluccius productus"] <- "Merluccius"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Sebastes crameri"] <- "Sebastes"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Sebastes goodei"] <- "Sebastes"
#ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Sebastes paucispinis"] <- "Sebastes"

##Check list of un-matchig names

Sp_ReviewDat <- as.character(unique(ReviewDat$b_scientific_name))  #list species in review
Sp_SAU <- as.character(unique(Final_SAU_EEZ$scientific_name))      #list species in SAU
Sp_SAU_FE <- as.character(unique(Final_SAU_FE$scientific_name))      #list species in SAU

#compare species in Review and SAU
matchsp <- Sp_ReviewDat %in% Sp_SAU
table1 <- data.frame(matchsp, Sp_ReviewDat)
table2 <- subset(table1, table1$matchsp==FALSE) ##species from review database missing in Sea Around Us
table2  #list of unmatching species (should be 3 when including genrus updates, "Chlamys islandica" is not in Final_SAU_EEZ but it is present n Final_SAU_FE so I dont change it)

#Final list of matching species in ReviewDAt
Sp_ReviewDat <- as.character(subset(table1, table1$matchsp==TRUE)[,2]) 

#write.csv(Sp_ReviewDat, "data/Sp_ReviewDat.csv")




## 2. MATCH EEZ NAMES IN REVIEW AND SAU (area_name)

#correct EEZ names in Review Database

ReviewDat$eez_countries[ReviewDat$eez_countries == "USA (Alaska, Arctic)"] <- "USA (Alaska-Subarctic)"
ReviewDat$eez_countries[ReviewDat$eez_countries == "Spain (mainland, Med and Gulf of Cadiz)"] <- "Spain (mainland Med and Gulf of Cadiz)"
ReviewDat$eez_countries[ReviewDat$site == "Northeast US shelf"] <- "USA (East Coast)"
ReviewDat$eez_countries[ReviewDat$eez_countries == "Denmark (North Sea), Germany (North Sea), Netherlands, Norway, United Kingdom (UK), Belgium, Sweden (West Coast)"] <- "Denmark (North Sea), Germany (North Sea), Netherlands, Norway, United Kingdom (UK), Belgium"
ReviewDat$eez_countries[ReviewDat$eez_countries == "Canada (East Coast)\n"] <- "Canada (East Coast)"
ReviewDat$eez_countries[ReviewDat$eez_countries == "United Kindom (UK)"] <- "United Kingdom (UK)"

##some observations have no EEZ. I put a EEZ based on location and http://www.marineregions.org/eezmapper.php
##[6] Spain(Northwest), [166-175] France (Atlantic), [176,177] Norway; [178-189] US North East coast. 
##REPASAR FILAS SI SE MODIFICA LA BASE ORIGINAL
ReviewDat$eez_countries[6] <- "Spain (Northwest)" 
ReviewDat$eez_countries[166:175] <- rep("France (Atlantic Coast)", 10)
ReviewDat$eez_countries[176:177] <- rep("Norway", 2)
ReviewDat$eez_countries[178:189] <- rep("USA (East Coast)", 12)

#split EEZs and multiply the rows for each EEZ
ReviewDat <- ReviewDat %>% 
  mutate(eez_countries = strsplit(as.character(eez_countries), ",")) %>% 
  unnest(eez_countries)

#recode colnames in ReviewDat to match Final_SAU_EEZ names
colnames(ReviewDat)[12] <- "scientific_name"
colnames(ReviewDat)[68] <- "area_name"

#remove blank spaces from eez variable names in Review
ReviewDat$area_name <- trim(ReviewDat$area_name)

##Check list of un-matchig EEZs

EEZ_ReviewDat  <- unique(ReviewDat$area_name) #final list of EEZs in the Review dataset
EEZ_SAU <- unique(Final_SAU_EEZ$area_name)

EEZ_ReviewDat %in% EEZ_SAU  


#write.csv(EEZ_ReviewDat, "data/EEZ_ReviewDat")


##ADD TOTAL CATCH AND LANDINGS BY EEZ
#select the EEZs of the review in the SAU database:

Final_SAU_EEZ <- subset(Final_SAU_EEZ, Final_SAU_EEZ$area_name %in% EEZ_ReviewDat)
#dataEEZ$area_name %in% EEZ_ReviewDat

#check observations for EEZ data
counts <- Final_SAU_EEZ %>%
  group_by(area_name, year) %>%
  tally
#delete year 2012 for the low number of observations as compared to other years
Final_SAU_EEZ <- subset(Final_SAU_EEZ, Final_SAU_EEZ$year!=2012)

#dataframe total EEZ catch

tonnesEEZ <- aggregate(tonnes ~ area_name, Final_SAU_EEZ, sum)
landedvalueEEZ <-  aggregate(landed_value ~ area_name, Final_SAU_EEZ, sum)
colnames(tonnesEEZ) <- c("area_name", "tonnesEEZ")
colnames(landedvalueEEZ) <- c("area_name", "landedvalueEEZ")

ReviewDat <- merge(ReviewDat, tonnesEEZ, by=c("area_name"), all.x=TRUE)
ReviewDat <- merge(ReviewDat, landedvalueEEZ, by=c("area_name"), all.x=TRUE)

##ADD TOTAL CATCH AND LANDINGS BY EEZ AND SP

#dataframe sp - EEZ catch

counts <- Final_SAU_FE %>%
  group_by(fishing_entity, year) %>%
  tally


tonnesEEZsp <- aggregate(tonnes ~ area_name+scientific_name, Final_SAU_EEZ, mean)
landedvalueEEZsp <-  aggregate(landed_value ~ area_name+scientific_name, Final_SAU_EEZ, mean)
colnames(tonnesEEZsp) <- c("area_name", "scientific_name", "tonnesEEZsp")
colnames(landedvalueEEZsp) <- c("area_name", "scientific_name","landedvalueEEZsp")


splist <- unique(tonnesEEZsp$scientific_name)
Sp_ReviewDat %in% splist

#add total EEZ catches and landings per SP to ReviewDat

ReviewDat <- merge(ReviewDat, tonnesEEZsp, by=c("area_name","scientific_name"), all.x=TRUE)

#check missing species in tonnesEEZsp
na <- is.na(ReviewDat$tonnesEEZsp)
table(na) #we miss 144 observations

#add total EEZ landings and landings per sp to ReviewDat

ReviewDat <- merge(ReviewDat, landedvalueEEZsp, by=c("area_name","scientific_name"), all.x=TRUE)
na <- is.na(ReviewDat$landedvalueEEZsp)
table(na) #we miss 144 observations


### FISHING ENTITIES 

#add fishing entities to ReviewDat by species and average years (mean across years)

Final_SAU_FE <- Final_SAU_FE %>%
  group_by(fishing_entity, area_name, scientific_name) %>%
  summarize(tonnesFEsp=mean(tonnes), landedvalueFEsp=mean(landed_value))

ReviewDat <- merge(ReviewDat, Final_SAU_FE, by=c("area_name","scientific_name"), all.x=TRUE)


#total catch per species for Fishing entities (sum across species and fishing entities)

tonnesFEspT<- aggregate(tonnesFEsp ~ fishing_entity + scientific_name, Final_SAU_FE, sum)
landedvalueFEspT <-  aggregate(landedvalueFEsp ~ fishing_entity+scientific_name, Final_SAU_FE, sum)
colnames(tonnesFEspT) <- c("fishing_entity", "scientific_name", "tonnesFEspT")
colnames(landedvalueFEspT) <- c("fishing_entity", "scientific_name", "landedvalueFEspT")

ReviewDat <- merge(ReviewDat, tonnesFEspT, by=c("fishing_entity", "scientific_name"), all.x=TRUE)
ReviewDat <- merge(ReviewDat, landedvalueFEspT, by=c("fishing_entity", "scientific_name"), all.x=TRUE)



#Total catch per fishing entity (sum across fishing entities)


tonnesFE<- aggregate(tonnesFEsp ~ fishing_entity, Final_SAU_FE, sum)
landedvalueFE <-  aggregate(landedvalueFEsp ~ fishing_entity, Final_SAU_FE, sum)
colnames(tonnesFE) <- c("fishing_entity", "tonnesFE")
colnames(landedvalueFE) <- c("fishing_entity", "landedvalueFE")

ReviewDat <- merge(ReviewDat, tonnesFE, by=c("fishing_entity"), all.x=TRUE)
ReviewDat <- merge(ReviewDat, landedvalueFE, by=c("fishing_entity"), all.x=TRUE)



write.csv(ReviewDat, file = "data/ReviewDat.csv")




##GRAPHS for VISUALIZING CATCH DATA

##A figure of fishing countries and fishing entities in the Review:

tonnesFEf<- aggregate(tonnesFEsp 67y~ fishing_entity+area_name, Final_SAU_FE, sum)
landedvalueFEf <-  aggregate(landedvalueFEsp ~ fishing_entity+area_name, Final_SAU_FE, sum)
colnames(tonnesFEf) <- c("fishing_entity", "area_name", "tonnesFE_EEZ")
colnames(landedvalueFEf) <- c("fishing_entity", "area_name", "landedvalueFE_EEZ")

FE_EEZ <- merge(tonnesFEf,landedvalueFEf)

FE_EEZ <- subset(FE_EEZ, FE_EEZ$area_name %in% EEZ_ReviewDat)

write.csv(FE_EEZ, file = "data/FE_EEZ.csv")


f1 <- ggplot(FE_EEZ, aes(x=fishing_entity, y=area_name))+
  geom_point(aes(size=tonnesFE_EEZ, colour=landedvalueFE_EEZ))+
  xlab("Fishing countries")+
  ylab("EEZ")+
  theme(axis.text.x = element_text(angle=-45, hjust= 0.06))+
  scale_color_gradient(low = "blue", high = "red")


