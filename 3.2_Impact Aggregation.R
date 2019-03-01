#####################################
# IMPACT AGGREGATION CODE ###########
#FOL, February 28th 2019 #####
#####################################


#Libraries

#library(dplyr)
library(tidyr)
library(cluster)
library(datasets)
library(graphics)
library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(dplyr)
library(tidyr)


#Open database (Biblio_database.csv)

ReviewDat <- read.csv("data/biblio_database.csv", stringsAsFactors=FALSE, header=T, sep = ",", fileEncoding="latin1")

#1. CLEAN DATABASE: Clean database (from integration fishbase.R script) - to delete after finalizing 1. Duplications)
##delete blank columns
colnames(ReviewDat)
#detele blank spaces in Species scientific name to match review_database-fishbase_database
trim.trailing <- function (x) sub("\\s+$", "", x)
ReviewDat$b_scientific_name <- trim.trailing(ReviewDat$b_scientific_name )
##Check spp names to match review_database-fishbase_database
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Atheresthes\240stomias"] <- "Atheresthes stomias"##??
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Lepidopsetta\240polyxystra"] <- "Lepidopsetta polyxystra"##??
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Clupea pallasii"] <- "Clupea pallasii pallasii"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Loligo pealeii"] <- "Doryteuthis pealeii"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Loligo opalescens"] <- "Doryteuthis opalescens"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Litopenaeus setiferus"] <- "Penaeus setiferus" 
## We dodn´t update the name of this specie (Litopenaeus setiferus) because we miss one match-sp. But we should keep it in mind to show with the final results
ReviewDat$rfishbase_species_code[ReviewDat$rfishbase_species_code=="322"] <- "308"
ReviewDat$rfishbase_species_code[ReviewDat$rfishbase_species_code=="3"] <- NA


####2. IMPACT TYPES (from distributional_impacts) - DELETE IN FINAL SCRIPT because is in 1. Duplications

#Factors
ReviewDat$researcher<-as.factor(ReviewDat$researcher)
ReviewDat$id_study<-as.factor(ReviewDat$id_study)
ReviewDat$cc<-as.factor(ReviewDat$cc)
ReviewDat$b_impact<-as.factor(ReviewDat$b_impact)
ReviewDat$b_direction<-as.factor(ReviewDat$b_direction)
ReviewDat$tax_group<-as.factor(ReviewDat$tax_group)

#Numeric
ReviewDat$b_value<-as.numeric(as.character(ReviewDat$b_value), na.omit=TRUE)
ReviewDat$b_years<-as.numeric(as.character(ReviewDat$b_years))

#change codes to a short description; in our review center of grav separated from center of biomass
levels(ReviewDat$b_impact)<-c("lat shift center of grav", #1
                          "lat shift center of bio",  #2
                          "depth shift", #3
                          "boundary lat shift center of grav", #4
                          "boundary lat shift center of bio", #5
                          "long shift center of grav", #7
                          "long shift center of bio", #8
                          "lat and long shift center of grav", #9
                          "shift in area occupied") #11

levels(ReviewDat$b_direction)<-c("lat shift north center of grav", #1
                             "lat shift south center of grav", #2
                             "boundary lat shift north center of grav", #3
                             "max lat shift north center of grav", #4
                             "max lat shift south center of grav",#5
                             "min lat shift north center of grav", #6
                             "shifting deeper", #7
                             "shifting shallower", #8
                             "area expansion", #9
                             "area contraction", #10
                             "lat shift north center of bio", #11
                             "lat shift south center of bio", #12
                             "shift southweastwards center of grav", #13
                             "shift northeastwards center of grav", #14
                             "shift westwards center of grav", #19
                             "boundary lat shift east center of bio", #20
                             "boundary lat shift north center of bio") #21

levels(ReviewDat$cc)<-c("AMO", #10
                    "climate velocity", #11
                    "sst", #2
                    "sst,bt,AMO", #2,3,10
                    "sst,bt", #2,3
                    "bt") #3



##AGGREGATION OF IMPACTS (EEZ, Species name, impact type)

#What's the data like? Tabe with EEZ, species and Values
mytable <- table(ReviewDat$eez_codes, ReviewDat$b_scientific_name, ReviewDat$b_impact)

##Split impacts into different EEZs: create an observation per EEZ 

ReviewDat <- ReviewDat %>% 
  mutate(eez_countries = strsplit(as.character(eez_countries), "-")) %>% 
  unnest(eez_countries)

#remove blank spaces from eez_countries
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
ReviewDat$eez_countries <- trim(ReviewDat$eez_countries)
#unique(ReviewDat$eez_countries)
#unique(ReviewDat$b_scientific_name)
#levels(ReviewDat$b_impact)

#Summarize impacts by Specie, eez and impact type
sp_eez_impact <- ReviewDat %>%
  group_by(eez_countries, b_scientific_name, b_impact) %>%
  summarise(n())
  
colnames(sp_eez_impact) <- c("eez_countries", "b_scientific_name", "b_impact", "N")

#SUBSET with the species and EEZs where we have duplicates
repeated_sp_eez_impact <- subset(sp_eez_impact, N >1) 

#N review dat species in EEZs -> 629
#dim(sp_eez_impact)
#N species repeated in a EEZ -< 90
#dim(repeated_sp_eez_impact)


##AGGREGATION

#first I select the variables in dataframe we need (simplify dataframe)
df <- ReviewDat %>%  select(id_obs, site, id_study, eez_countries, b_scientific_name, cc, b_impact, b_value)  %>%
  group_by(eez_countries, b_scientific_name, b_impact) %>% mutate(total = n())


ddc<-df[df$total==1,]#subsets those without duplicates
ddd<-df[df$total==2,]#subsets those with duplicates

#here I combine b_value to calculate the mean:
ddd<- ddd %>% group_by(eez_countries, b_scientific_name, b_impact) %>% mutate(b_value_x = mean(b_value, na.rm = TRUE))  #mean value of b_value in eez-sp-impact

#here I aggregate the info from the rest of columns:
ddd<- ddd %>% group_by(eez_countries, b_scientific_name, b_value_x) %>% summarise(id_obs = paste(id_obs, collapse = " "), #all values of id_obs
                                                                                  site = paste(site, collapse = " "),     #all values of site
                                                                                  id_study = paste(id_study, collapse = " "), #all values of id_study
                                                                                  b_impact=paste(b_impact, collapse=","),
                                                                                  cc = paste(cc, collapse = " "),  #all values of cc
                                                                                  b_value = paste(b_value, collapse = " "),  #all values of original impact b_value
                                                                                  total=mean(total))

colnames(ddd)           #change column name to have b_value aggregated and be able to merge with non duplicated data
colnames(ddd)[which(names(ddd) == "b_value")] <- "b_value_original"
colnames(ddd)[which(names(ddd) == "b_value_x")] <- "b_value"
colnames(ddd)[which(names(ddd) == "total")] <- "duplications"
colnames(ddc)[which(names(ddc) == "total")] <- "duplications"

ddc$duplications[ddc$duplications==1] <- 0
ddd$duplications[ddd$duplications==2] <- 1

#Now I merge the subsets of non-duplications (ddc) and with duplications removed/averaged (ddd)

ReviewDat <- merge(ddc, ddd, all=TRUE)
write.csv(ReviewDat, "biblio_databse3.csv")






