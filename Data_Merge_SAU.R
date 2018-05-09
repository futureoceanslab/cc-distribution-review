##Elena Ojea
#Code for EEZ and species list from the review - matching EEZ data
#input files: 
# - ReviewDatsp.csv biblio data + fishbase species info from integration_fishbase.R code
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
ReviewDatFB.raw <- read.csv("data/ReviewDatsp.csv", stringsAsFactors=FALSE, header=T) ## biblio_database + fishbase from script integration_fishbase.R
 
#Read input file: the EEZ species catched
Final_SAU_EEZ.raw <- read.csv("data/Final_SAU_EEZ.csv", stringsAsFactors=FALSE, header=T)

##FILTER SAU DATASETS: 5 last years, landings, NAs
Final_SAU_EEZ <- filter(Final_SAU_EEZ.raw, year > 2009, catch_type=="Landings")

##CLEAN OUR REVIEW-DATABASE
##delete blank columns
colnames(ReviewDatFB.raw)
ReviewDatFB <- ReviewDatFB.raw [, 2:167]   
#detele blank spaces in Species scientific name to match review_database-SAU_database
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
ReviewDatFB$b_scientific_name <- trim(ReviewDatFB$b_scientific_name)

##Check spp names to match review_database-fishbase_database
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Atheresthes\240stomias"] <- "Atheresthes stomias"??
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Lepidopsetta\240polyxystra"] <- "Lepidopsetta polyxystra"??
ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Clupea pallasii"] <- "Clupea pallasii pallasii"
ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Loligo opalescens"] <- "Doryteuthis opalescens"##!
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Stenotomus caprinus"] <- "Stenotomus chrysops"???
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Scophthalmidae"] <- "Scophthalmus aquosus" ##Family??
#ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Isopsetta isolepis"] <- "Eopsetta jordani"  
ReviewDatFB$b_scientific_name[ReviewDatFB$b_scientific_name=="Loligo pealeii"] <- "Doryteuthis pealeii"##!

## MATCH SPECIES NAMES IN REVIEW AND SAU####
##Check list of un-matchig names
Sp_ReviewDatFB <- as.character(unique(ReviewDatFB$b_scientific_name))  #list species in review
Sp_SAU <- as.character(unique(Final_SAU_EEZ$scientific_name))      #list species in SAU

#compare species in Review and SAU
matchsp <- Sp_ReviewDatFB %in% Sp_SAU
table(matchsp) ## 33 spp no macth, 112 spp macthed (total:145spp)
spmiss <- Sp_ReviewDatFB[matchsp==FALSE] ## list of unmatching(lost) species

##To chek the matches among lists and edit the list of spp lost
#write.csv(ReviewDatFB, file="data/listSpReviewDatFB.csv")
#write.csv(matchsp, file="data/listSpmatchsp.csv")
write.csv(spmiss, file="data/2listspmiss.csv") ##list of lost species

#Final list of matching species in ReviewDatFB ??
#Sp_ReviewDatFB <- as.character(subset(table1, table1$matchsp==TRUE)[,2]) ??

##MATCH EEZ NAMES IN REVIEW AND SAU (area_name)####
#split EEZs and multiply the rows for each EEZ
ReviewDatFB <- ReviewDatFB %>% 
  mutate(eez_countries = strsplit(as.character(eez_countries), "-")) %>% 
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
tonlandEEZspyear<-Final_SAU_EEZ %>%
  group_by(area_name, year, scientific_name) %>%
  summarise(tonnesEEZspyear=sum(tonnes,na.rm = T),
            landedvalueEEZspyear=sum(landed_value,na.rm = T))
tonlandEEZsp<-tonlandEEZspyear %>%
  group_by(area_name, scientific_name) %>%
  summarise(tonnesEEZsp=mean(tonnesEEZspyear,na.rm = T),
            landedvalueEEZsp=mean(landedvalueEEZspyear,na.rm = T))

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
