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

ReviewDat <- read.csv("data/biblio_database2.csv", stringsAsFactors=FALSE, header=T, sep = ",", fileEncoding="latin1")


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
df <- ReviewDat %>%  select(id_obs, site, id_study, eez_countries, b_scientific_name, cc, b_impact, b_value, rfishbase_species_code)  %>%
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
                                                                                  total=mean(total),
                                                                                  rfishbase_species_code=sum(rfishbase_species_code))

colnames(ddd)           #change column name to have b_value aggregated and be able to merge with non duplicated data
colnames(ddd)[which(names(ddd) == "b_value")] <- "b_value_original"
colnames(ddd)[which(names(ddd) == "b_value_x")] <- "b_value"
colnames(ddd)[which(names(ddd) == "total")] <- "duplications"
colnames(ddc)[which(names(ddc) == "total")] <- "duplications"

ddc$duplications[ddc$duplications==1] <- 0
ddd$duplications[ddd$duplications==2] <- 1

#Now I merge the subsets of non-duplications (ddc) and with duplications removed/averaged (ddd)

ReviewDat <- merge(ddc, ddd, all=TRUE)
write.csv(ReviewDat, "data/biblio_databse3.csv")






