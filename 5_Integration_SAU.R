##Integration SAU
##Elena Ojea, March 2019
##INPUT FILES: data_EEZ_SAU/, ReviewDatsp.csv, data_FE_SAU/
##OUTPUT FILES: Biblio_database_full.csv

library(tidyverse)

########1. Download SAU Database EEZ ("data_EEZ_SAU/")

########2. DATA ON SAU EEZs
path <- "data/data_EEZ_SAU/"
l <- list.files(path, pattern=".csv")

func <- function(i){
  df <- read.csv(paste0(path,l[i]))
  if (length(df) == 17) {
    df <- df[-which(names(df) == "end_use_type")]  
  }
  return(df)
}

l1 <- lapply(1:length(l), func)
final <- do.call(rbind, l1)

#verification number of EEZ names and number of rows
unique(final$area_name)
suma <- 0
for (n in l1) {
  suma <- suma + nrow(n)
} 

##FILTER SAU DATASETS: 5 last years, landings, NAs
Final_SAU_EEZ <- filter(final, year > 2009, catch_type == "Landings")
rm(final, l, l1, suma, path, func, n)

########3. MERGE BIBLIO WITH SAU_EEZ

##3.1. OPEN our review database with the fishbase inputs
ReviewDatFB <- read.csv("data/ReviewDatsp.csv", stringsAsFactors = F) ## biblio_database + fishbase from script integration_fishbase.R

##3.2. MATCH SPECIES NAMES IN REVIEW AND SAU####
##Check list of un-matchig names
Sp_ReviewDatFB <- as.character(unique(ReviewDatFB$b_scientific_name))  #list species in review
Sp_SAU <- as.character(unique(Final_SAU_EEZ$scientific_name))      #list species in SAU

#compare species in Review and SAU
matchsp <- Sp_ReviewDatFB %in% Sp_SAU
table(matchsp) ## 33 spp no macth, 109 spp macthed (total:142spp) *will be updated with new code March 2019
spmiss1 <- Sp_ReviewDatFB[matchsp == F] ## list of unmatching(lost) species


##3.3 MATCH COLUMNS and EEZ NAMES IN REVIEW AND SAU (area_name)####

#recode colnames in ReviewDatFB to match Final_SAU_EEZ names
colnames(ReviewDatFB)[which(names(ReviewDatFB) == "b_scientific_name")] <- "scientific_name"
colnames(ReviewDatFB)[which(names(ReviewDatFB) == "eez_countries")] <- "area_name"

##Check list of un-matchig EEZs
EEZ_ReviewDatFB  <- unique(ReviewDatFB$area_name) #final list of EEZs in the Review dataset
EEZ_SAU <- unique(Final_SAU_EEZ$area_name)
EEZ_ReviewDatFB %in% EEZ_SAU
#EEZs of the review are now the same as in the SAU database
identical(sort(unique(ReviewDatFB$area_name)), sort(unique(as.character(Final_SAU_EEZ$area_name))))

##3.4 MERGE: ADD TOTAL CATCH AND LANDINGS BY EEZ####
#check observations for EEZ data
counts <- Final_SAU_EEZ %>%
            group_by(area_name, year) %>%
            tally
rm(counts, EEZ_ReviewDatFB, EEZ_SAU, matchsp, Sp_SAU)

#dataframe total EEZ catch 
#sum all catches per area_name and year
tonlandEEZyear <- Final_SAU_EEZ %>%
                    group_by(area_name,year) %>%
                    summarise(tonnesEEZyear = sum(tonnes, na.rm = T),
                              landedvalueEEZyear = sum(landed_value, na.rm = T))
                  #take mean value across years (2010-2014)    
tonlandEEZ <- tonlandEEZyear %>%
                group_by(area_name) %>%
                summarise(tonnesEEZ = mean(tonnesEEZyear, na.rm = T),
                          landedvalueEEZ = mean(landedvalueEEZyear, na.rm = T))          
#merge
ReviewDatFB_SAU1 <- merge(ReviewDatFB, tonlandEEZ, by = c("area_name"), all.x = T)
rm(tonlandEEZyear, tonlandEEZ, ReviewDatFB)

##3.5. MERGE: ADD TOTAL CATCH AND LANDINGS BY EEZ AND SP####
#gives mean value across years (2010-2014 ) annual tonnes (2010-2014) 
tonlandEEZspyear <- Final_SAU_EEZ %>%
                      group_by(area_name, year, scientific_name) %>%
                      summarise(tonnesEEZspyear = sum(tonnes, na.rm = T),
                                landedvalueEEZspyear = sum(landed_value, na.rm = T))
tonlandEEZsp <- tonlandEEZspyear %>%
                  group_by(area_name, scientific_name) %>%
                  summarise(tonnesEEZsp = mean(tonnesEEZspyear, na.rm = T),
                            landedvalueEEZsp = mean(landedvalueEEZspyear, na.rm = T))
#merge: add total EEZ catches and landings per SP to ReviewDat
ReviewDatFB_SAU2 <- merge(ReviewDatFB_SAU1, tonlandEEZsp, by = c("area_name","scientific_name"), all.x = T)

##3.6 Doble-check
##Species name match - tonlandEEZsp
splist <- unique(tonlandEEZsp$scientific_name)
matchsp2 <- Sp_ReviewDatFB %in% splist
table(matchsp2) ## 33 spp no macth, 109 spp macthed (total:142spp). Same resultas above
spmiss2 <- Sp_ReviewDatFB[matchsp2 == F] ## list of unmatching(lost) species
##To chek the matches among lists and edit the list of spp lost
identical(spmiss1,spmiss2)
#write.csv(spmiss2, file = "data/2listspmiss.csv") ##list of lost species

rm(ReviewDatFB_SAU1, tonlandEEZspyear, tonlandEEZsp, Sp_ReviewDatFB)

#check missing species in tonnesEEZsp and landedvalueEEZsp
#Not all the species have data for all the years/EEZs
na1 <- is.na(ReviewDatFB_SAU2$tonnesEEZsp)
table(na1) #we miss 200 observations, False 395, True 200
na2 <- is.na(ReviewDatFB_SAU2$landedvalueEEZsp)
table(na2) #we miss 200 observations, False 395, True 200

rm(na1, na2, spmiss1, spmiss2, matchsp2, splist)

##3.7. list of FE
list_FE <- unique(Final_SAU_EEZ$fishing_entity)
write.csv(list_FE, row.names = F, "data/list_FE.csv")

rm(Final_SAU_EEZ, list_FE)

########4. Download SAU Database FE ("data_EEZ_SAU.csv")

########5. DATA ON SAU FISHING ENTITIES
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




########6. MERGE BIBLIO-EEZ with DATA FE

##FILTER SAU DATASETS: 5 last years, landings, NAs
Final_SAU_FE <- filter(Final_SAU_FE, year > 2009, catch_type == "Landings")

#dataframe sp - EEZ catch
counts <- Final_SAU_FE %>%
            group_by(fishing_entity, year) %>%
            tally  


# 1. FISHING ENTITIES#### 

#add fishing entities landings and catches per species to ReviewDat 

#Catches and Landings per fishing entity, EEZ and species (mean across years)
tonlandFEspyear <- Final_SAU_FE %>%
                    group_by(fishing_entity, area_name, year, scientific_name) %>%
                    summarise(tonnesFEspyear = sum(tonnes, na.rm = T),
                              landedvalueFEspyear = sum(landed_value, na.rm = T))

tonlandFEsp <- tonlandFEspyear %>% #borrar bis
                group_by(fishing_entity, area_name, scientific_name) %>%
                summarise(tonnesFEsp = mean(tonnesFEspyear, na.rm = T),
                          landedvalueFEsp = mean(landedvalueFEspyear, na.rm = T))

ReviewDatFB_SAU3  <- merge(ReviewDatFB_SAU2, tonlandFEsp, by = c("area_name","scientific_name"), all.x = T)

#Verification of EEZ names and species
#a<-unique(ReviewDatFB_SAU3$area_name)
#b<-unique(ReviewDatFB_SAU3$scientific_name)
#c<-unique(tonlandFEsp$area_name)
#d<-unique(tonlandFEsp$scientific_name)
#a %in% c    #checking EEZ names match, all TRUE -> they all macth
#spmiss<-b[which(!b %in% d)]   ## list of unmatching(lost) species, 29 spp no macth, 116 spp macthed (total:142spp)

#total catch per species for Fishing entities (sum across species and eezs)
tonlandFEspT<- tonlandFEsp %>%
                group_by(fishing_entity, scientific_name) %>%
                summarise(tonnesFEspT=sum(tonnesFEsp, na.rm = T),
                          landedvalueFEspT=sum(landedvalueFEsp, na.rm = T))

ReviewDatFB_SAU4 <- merge(ReviewDatFB_SAU3, tonlandFEspT, by = c("fishing_entity", "scientific_name"), all.x = T)


#Total catch per fishing entity (mean across fishing entities)
tonlandFE<- tonlandFEspT %>%
              group_by(fishing_entity) %>%
              summarise(tonnesFE = sum(tonnesFEspT, na.rm = T),
                        landedvalueFE = sum(landedvalueFEspT, na.rm = T))

ReviewDatFB_SAU5 <- merge(ReviewDatFB_SAU4, tonlandFE, by = c("fishing_entity"), all.x = T)

#Total catch per fishing entity in EEZ (annual mean 2010-2014)
tonlandFEEZyear <- Final_SAU_FE %>%
                    group_by(fishing_entity, area_name, year) %>%
                    summarise(tonnesFEEZyear = sum(tonnes, na.rm = T),
                              landedvalueFEZZyear = sum(landed_value, na.rm = T))

tonlandFEEZ <- tonlandFEEZyear %>% 
                group_by(fishing_entity, area_name) %>%
                summarise(tonnesFEEZ = mean(tonnesFEEZyear, na.rm = T),
                          landedvalueFEEZ = mean(landedvalueFEZZyear, na.rm = T))

Biblio_data <- merge(ReviewDatFB_SAU5, tonlandFEEZ, by=c("fishing_entity", "area_name"), all.x = T)


#2. CATCH DEPENDENCY OF FISHING ENTITIES

#2.1. Species catch dependency on the area
Biblio_data$catchdepFEsp <- Biblio_data$tonnesFEsp/Biblio_data$tonnesFEspT #the dependence of the country species catches on the EEZ species catches
Biblio_data$landdepFEsp <-  Biblio_data$landedvalueFEsp/Biblio_data$landedvalueFEspT #the dependence of the country total SP Value on the EEZ SP catch value 

range(Biblio_data$catchdepFEsp, na.rm = T)
range(Biblio_data$landdepFEsp, na.rm = T) #is the same relation

#2.2. Country dependency on the species in the area

Biblio_data$catchdepFE <- Biblio_data$tonnesFEsp/Biblio_data$tonnesFE #the dependence of the country species catches on the EEZ species catches
range(Biblio_data$catchdepFE, na.rm = T)

#2.3. Country dependency on the area

Biblio_data$catchdepFEEZ <- Biblio_data$tonnesFEEZ/Biblio_data$tonnesEEZ 
range(Biblio_data$catchdepFEEZ, na.rm = T)

##3VALUE OF SPECIES FOR FISHING ENTITIES

###3.1 Species VAlue in FE: landed value/tonnes

Biblio_data$spvalueFE  <- Biblio_data$landedvalueFEsp/Biblio_data$tonnesFEsp
range(Biblio_data$spvalueFE, na.rm = T)
quantile(Biblio_data$spvalueFE, na.rm = T)

##4. CATCH PRESSURE IN EEZ

Biblio_data$catchpresEEZsp <- Biblio_data$tonnesEEZsp/Biblio_data$tonnesEEZ
Biblio_data$landpresEEZsp <- Biblio_data$landedvalueEEZsp/Biblio_data$landedvalueEEZ

range(Biblio_data$catchpresEEZsp, na.rm = T)

#4.1 prices in EEZs (value of 1 tone species in EEZ, is it ok?)
Biblio_data$spvalueEEZ <- Biblio_data$landedvalueEEZsp /Biblio_data$tonnesEEZsp
range(Biblio_data$spvalueEEZ, na.rm = T)
quantile(Biblio_data$spvalueEEZ, na.rm = T)

##5. OUTPUT FILE###
write.csv(Biblio_data, file = "data/biblio_database_full.csv", row.names = F)