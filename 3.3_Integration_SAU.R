##################################################################
##### This script integrates our database with the SAU EEZ and Fishing  
##### Entity (FE) databases. It also creates a FE-list to know which 
##### FEs should be downloaded from the SAU website.
##### 03/2019
##### INPUT FILES: SAU_dataset_EEZ.csv, biblio_database3.csv,
##### data_FE_SAU/ files, FE_list.csv,
##### OUTPUT FILES: biblio_database_full.csv, dall_FE_area.csv
##################################################################

library(tidyverse)

########1. Read databases####
#SAU Database EEZ
Final_SAU_EEZ <- read.csv("data/SAU_dataset_EEZ.csv")
##Our review database 
ReviewDatFB <- read.csv("data/biblio_database3.csv", stringsAsFactors = F) ## biblio_database without duplicates with EEZ structure

########2. Verify EEZ names####
##MATCH COLUMNS and EEZ NAMES IN REVIEW AND SAU (area_name)
#recode colnames in ReviewDatFB to match Final_SAU_EEZ names
colnames(ReviewDatFB)[which(names(ReviewDatFB) == "stock_EEZ_country")] <- "area_name"

##Check list of un-matchig EEZs
EEZ_ReviewDatFB  <- unique(ReviewDatFB$area_name) #final list of EEZs in the Review dataset
EEZ_SAU <- unique(Final_SAU_EEZ$area_name)
EEZ_ReviewDatFB %in% EEZ_SAU
ReviewDatFB[ReviewDatFB$area_name == "Portugal (mailand)", 3] <- "Portugal (mainland)" 
EEZ_ReviewDatFB  <- unique(ReviewDatFB$area_name) #final list of EEZs in the Review dataset
EEZ_SAU <- unique(Final_SAU_EEZ$area_name)
EEZ_ReviewDatFB %in% EEZ_SAU
#EEZs of the review are now the same as in the SAU database
identical(sort(unique(ReviewDatFB$area_name)), sort(unique(as.character(Final_SAU_EEZ$area_name))))
#result needs to be TRUE

########4. Species Doble-check####
##Species name match - tonlandEEZsp
splist <- unique(Final_SAU_EEZ$scientific_name)
matchsp <- unique(ReviewDatFB$scientific_name) %in% splist
table(matchsp) ## 46 spp no macth, 147 spp macthed (total:193spp). Same results
spmiss0 <- unique(ReviewDatFB$scientific_name)[matchsp==F]

rm(EEZ_SAU, EEZ_ReviewDatFB, splist, matchsp)  

########5. list of FE (to download FE data from SAU)####
list_FE <- unique(Final_SAU_EEZ$fishing_entity)
write.csv(list_FE, row.names = F, "data/list_FE.csv")

rm(Final_SAU_EEZ, list_FE)

########6. Download SAU Databases FE ("data_EEZ_SAU.csv")####
#Download manually from the SAU website the fishing entities (FEs)'s databases
#from list_FE

########7. DATA ON SAU FISHING ENTITIES####
#the downloaded files are merged in one database with this code:

path <- "data/data_FE_SAU/"
l <- list.files(path, pattern = ".csv")
#l <- l0[-c(7, 24)] # to delete certain files. E.g. column 7(France) and 24

# below func does importing and creation of new columns
func <- function(i){
  df <- read.csv(paste0(path,l[i]))
  names <- unlist(strsplit(l[i], ".", fixed = T))
  df$fishing_entity <- names[1]
  return(df)
}

# l1 shall have each of the dataframes individually with new columns attached
l1 <- lapply(1:length(l), func)
# here we combine all dataframes together
Final_SAU_FE <- do.call(rbind.data.frame, l1) #combine the datasets on fishing entities total catch
rm(l1, l, func, path)

########8. MERGE BIBLIO-EEZ with DATA FE####

##FILTER SAU DATASETS: 5 last years, landings, NAs
Final_SAU_FE <- filter(Final_SAU_FE, year > 2009, catch_type == "Landings")

#dataframe sp - EEZ catch
counts <- Final_SAU_FE %>%
            group_by(fishing_entity, year) %>%
            tally  
rm(counts)

########9. FISHING ENTITIES VARIABLES#### 
#add fishing entities landings and catches per species to ReviewDat 
Final_SAU_FE$area_name <- as.character(Final_SAU_FE$area_name)
Final_SAU_FE$scientific_name <- as.character(Final_SAU_FE$scientific_name)
#write.csv(Final_SAU_FE, "data/SAU_all.csv", row.names = F)

#Catches and Landings per fishing entity, EEZ and species (mean across years)
tonlandFEspyear <- Final_SAU_FE %>%
                    group_by(fishing_entity, area_name, year, scientific_name) %>% #, fishing_sector
                    summarise(tonnesFEspyear = sum(tonnes, na.rm = T),
                              landedvalueFEspyear = sum(landed_value, na.rm = T))

tonlandFEsp <- tonlandFEspyear %>% #borrar bis
                group_by(fishing_entity, area_name, scientific_name) %>% #, fishing_sector
                summarise(tonnesFEsp = mean(tonnesFEspyear, na.rm = T),
                          landedvalueFEsp = mean(landedvalueFEspyear, na.rm = T))

ReviewDatFB_SAU1  <- left_join(ReviewDatFB, tonlandFEsp, by = c("area_name","scientific_name"))

#Verification of EEZ names and species
a<-unique(ReviewDatFB_SAU1$area_name)
b<-unique(ReviewDatFB_SAU1$scientific_name)
c<-unique(tonlandFEsp$area_name)
d<-unique(tonlandFEsp$scientific_name)
a %in% c    #checking EEZ names match, all TRUE -> they all macth
spmiss<-b[which(!b %in% d)]   ## list of unmatching(lost) species, 43 spp no macth, 3 new species!!
spmiss0[which(spmiss0 %in% spmiss == F)]#three species that were not in EEZ SAU but are in FE SAU
rm(a, b, c, d, spmiss, spmiss0)


#Total catch per fishing entity (sum across fishing entities)
#GENERAL DATABASE
tonlandFE <- tonlandFEsp %>%
              group_by(fishing_entity) %>%
              summarise(tonnesFE = sum(tonnesFEsp, na.rm = T),
                        landedvalueFE = sum(landedvalueFEsp, na.rm = T))

Biblio_data <- left_join(ReviewDatFB_SAU1, tonlandFE, by = c("fishing_entity"))


#CATCH DEPENDENCY OF FISHING ENTITIES
##CREATION OF VARIABLES for DEPEDENCE

# CATCH DEPENDENCY AND VALUE OF FISHING ENTITIES
# Country dependency on the species in the area
#CATCH
Biblio_data$catchdepFE <- Biblio_data$tonnesFEsp/Biblio_data$tonnesFE #the dependence of the country catches on the EEZ species catches
range(Biblio_data$catchdepFE, na.rm = T)
#VALUE
Biblio_data$valuedepFE <- Biblio_data$landedvalueFEsp/Biblio_data$landedvalueFE #the dependence of the country catches on the EEZ species catches
range(Biblio_data$valuedepFE, na.rm = T)


##5. OUTPUT FILES####
write.csv(Biblio_data, file = "data/biblio_database_full.csv", row.names = F)
write.csv(tonlandFEsp, file = "data/dall_FE_area.csv", row.names = F)