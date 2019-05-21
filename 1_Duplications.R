#################################
##### FOL
##### 04/05/2019
#################################

library(tidyverse)

#1. Open database
data <- read.table("data/biblio_database.csv", header= T, sep=";")

#2. Change species names  (to match with fishbase database)
#detele blank spaces in Species scientific name to match review_database-fishbase_database
trim.trailing <- function (x) sub("\\s+$", "", x)
data$scientific_name <- trim.trailing(data$scientific_name)
##Check spp names to match review_database-fishbase_database
data$scientific_name[data$scientific_name == "Atheresthes\240stomias"] <- "Atheresthes stomias"##??
data$scientific_name[data$scientific_name == "Lepidopsetta\240polyxystra"] <- "Lepidopsetta polyxystra"##??
data$scientific_name[data$scientific_name == "Clupea pallasii"] <- "Clupea pallasii pallasii"
data$scientific_name[data$scientific_name == "Loligo pealeii"] <- "Doryteuthis pealeii"
data$scientific_name[data$scientific_name == "Loligo opalescens"] <- "Doryteuthis opalescens"
data$scientific_name[data$scientific_name == "Litopenaeus setiferus"] <- "Penaeus setiferus" 
## We dodnÂ´t update the name of this specie (Litopenaeus setiferus) because we miss one match-sp. But we should keep it in mind to show with the final results
data$fishbase_id_species[data$fishbase_id_species == "322"] <- "308"
data$fishbase_id_species[data$fishbase_id_species == "3"] <- NA

#3. Change formats
#Factors
data$id_study <- as.factor(data$id_study)
data$id_obs <- as.factor(data$id_obs)
data$year_publication <- as.factor(data$year_publication)


#Numeric
data$years_data <- as.numeric(as.character(data$years_data), na.omit = T)


#4. Find duplications
#4.1 Select rows with same species 
data_sp <- data %>% 
  group_by(scientific_name) %>% 
  filter(n() > 1) #more than 1 observation per species

#4.2. Select rows with same EEZ and impact response
commas <- data_sp %>% 
  filter(str_detect(stock_EEZ_code, ",")) #rows with more than one EEZ

commas_str <- as.character(commas$stock_EEZ_code)  #save names of these EEZs  
commas_str <- unique(commas_str) #keep one repetition of each EEZ
commas_vec <- unlist(strsplit(commas_str, ",")) #split EEZ names

data_dupl <- data_sp %>% #single duplications: "eez" vs "eez-eez"
  group_by(scientific_name, response, time_span) %>% 
  filter(stock_EEZ_code %in% commas_vec)

data_eez_all <- data_sp %>% #more duplications "eez-eez" vs "eez-eez"
  group_by(scientific_name, response, stock_EEZ_code, time_span)  %>% 
  filter(n() > 1)

if (dim(data_dupl)[1] > 0 ) { #merge duplicates
  duplications1 <- rbind(data_dupl, data_eez_all)
} else {
  duplications1 <- data_eez_all
}

#4.3. Select rows already having same EEZ and impact response
# but also having same database
commas2 <- duplications1 %>% 
  filter(str_detect(stock_location_data_source, ",")) #rows with more than one database

commas_str2 <- as.character(commas2$stock_location_data_source)  #save names of these databases 
commas_str2 <- unique(commas_str2) #keep one repetition of each database
commas_vec2 <- unlist(strsplit(commas_str2, ",")) #split databases names

data_dupl2 <- duplications1 %>% #single duplications: "datab" vs "datab-datab"
  group_by(scientific_name, response, time_span) %>% 
  filter(stock_location_data_source %in% commas_vec2)

data_datab_all <- duplications1 %>% #more duplications "datab-datab" vs "datab-datab"
  group_by(scientific_name, response, stock_location_data_source, time_span)  %>% 
  filter(n() > 1)

if (dim(data_dupl2)[1] > 0 ) { #merge duplications
  duplications2 <- rbind(data_dupl2, data_datab_all)
} else {
  duplications2 <- data_datab_all
}

#4.4. Create final duplications database called "remove"
duplications3 <- duplications2 %>% 
  group_by(scientific_name) %>%
  filter(stock_replicates == "no") #papers with only 1 stock

remove1 <- duplications3 %>% 
  group_by(scientific_name) %>%
  slice(which.min(year_publication)) #older papers' duplications

remove2 <- duplications2 %>% 
  group_by(scientific_name) %>%
  filter(stock_replicates == "yes") #papers with more than 1 stock

remove <- rbind(remove1, remove2)

#4.5. Delete duplicated values from original database
data_end <- anti_join(data, remove, by = "id_obs")


#5. Save a "cleaner" database
write.csv(data_end, row.names = F, "biblio_database1.csv")
