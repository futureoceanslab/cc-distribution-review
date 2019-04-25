#################################
##### FOL
##### 28/02/2019
#################################

library(tidyverse)

#1. Open database
data <- read.csv("data/biblio_database.csv")

#2. Change species names  (to match with fishbase database)
#detele blank spaces in Species scientific name to match review_database-fishbase_database
trim.trailing <- function (x) sub("\\s+$", "", x)
data$b_scientific_name <- trim.trailing(data$b_scientific_name )
##Check spp names to match review_database-fishbase_database
data$b_scientific_name[data$b_scientific_name == "Atheresthes\240stomias"] <- "Atheresthes stomias"##??
data$b_scientific_name[data$b_scientific_name == "Lepidopsetta\240polyxystra"] <- "Lepidopsetta polyxystra"##??
data$b_scientific_name[data$b_scientific_name == "Clupea pallasii"] <- "Clupea pallasii pallasii"
data$b_scientific_name[data$b_scientific_name == "Loligo pealeii"] <- "Doryteuthis pealeii"
data$b_scientific_name[data$b_scientific_name == "Loligo opalescens"] <- "Doryteuthis opalescens"
data$b_scientific_name[data$b_scientific_name == "Litopenaeus setiferus"] <- "Penaeus setiferus" 
## We dodnÂ´t update the name of this specie (Litopenaeus setiferus) because we miss one match-sp. But we should keep it in mind to show with the final results
data$rfishbase_species_code[data$rfishbase_species_code == "322"] <- "308"
data$rfishbase_species_code[data$rfishbase_species_code == "3"] <- NA

#3. Change formats
#Factors
data$researcher <- as.factor(data$researcher)
data$id_study <- as.factor(data$id_study)
data$cc <- as.factor(data$cc)
data$b_impact <- as.factor(data$b_impact)
data$b_direction <- as.factor(data$b_direction)
data$tax_group <- as.factor(data$tax_group)

#Numeric
data$b_value <- as.numeric(as.character(data$b_value), na.omit = T)
data$b_years <- as.numeric(as.character(data$b_years))

#4. Change codes to a short description; in the original review center of grav separated from center of biomass
data$b_impact_original <- data$b_impact
levels(data$b_impact) <- c("lat shift center of grav", #1
                           "lat shift center of bio",  #2
                           "depth shift", #3
                           "boundary lat shift center of grav", #4
                           "boundary lat shift center of bio", #5
                           "long shift center of grav", #7
                           "long shift center of bio", #8
                           "lat and long shift center of grav", #9
                           "shift in area occupied", #11
                           "depth range shift", #12
                           "latitude range shift") #13

data$b_direction_original <- data$b_direction
levels(data$b_direction) <- c("lat shift north center of grav", #1
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
                              "boundary lat shift north center of bio", #21
                              "depth range expansion", #22
                              "depth range contraction", #23
                              "lat range expansion center of grav", #24
                              "lat range contraction center of grav") #25

data$cc_original <- data$cc
levels(data$cc) <- c("AMO,NAO,CO2", #10,12,16
                     "climate velocity", #11
                     "ssta", #13
                     "composite index", #15
                     "sst", #2
                     "sst,CO2", #2,16
                     "sst,bt", #2,3
                     "sst,bt,AMO,NAO,biomass", #2,3,10,12,14
                     "sst, AMO", #2,10
                     "bt") #3
                      

#5. Find duplications
#5.1 Select rows with same species 
data_sp <- data %>% 
  group_by(b_scientific_name) %>% 
  filter(n() > 1) #more than 1 observation per species

#5.2. Select rows with same EEZ and impact response
commas <- data_sp %>% 
  filter(str_detect(eez_codes, ",")) #rows with more than one EEZ

commas_str <- as.character(commas$eez_codes)  #save names of these EEZs  
commas_str <- unique(commas_str) #keep one repetition of each EEZ
commas_vec <- unlist(strsplit(commas_str, ",")) #split EEZ names

data_dupl <- data_sp %>% #single duplications: "eez" vs "eez-eez"
              group_by(b_scientific_name, b_impact, b_years) %>% 
              filter(eez_codes %in% commas_vec)

data_eez_all <- data_sp %>% #more duplications "eez-eez" vs "eez-eez"
                  group_by(b_scientific_name, b_impact, eez_codes, b_years)  %>% 
                  filter(n() > 1)

if (dim(data_dupl)[1] > 0 ) { #merge duplicates
  duplications1 <- rbind(data_dupl, data_eez_all)
} else {
  duplications1 <- data_eez_all
}

#5.3. Select rows already having same EEZ and impact response
# but also having same database
commas2 <- duplications1 %>% 
            filter(str_detect(fish_data_source, ",")) #rows with more than one database

commas_str2 <- as.character(commas2$fish_data_source)  #save names of these databases 
commas_str2 <- unique(commas_str2) #keep one repetition of each database
commas_vec2 <- unlist(strsplit(commas_str2, ",")) #split databases names

data_dupl2 <- duplications1 %>% #single duplications: "datab" vs "datab-datab"
                group_by(b_scientific_name, b_impact, b_years) %>% 
                filter(fish_data_source %in% commas_vec2)

data_datab_all <- duplications1 %>% #more duplications "datab-datab" vs "datab-datab"
                    group_by(b_scientific_name, b_impact, fish_data_source, b_years)  %>% 
                    filter(n() > 1)

if (dim(data_dupl2)[1] > 0 ) { #merge duplications
  duplications2 <- rbind(data_dupl2, data_datab_all)
} else {
  duplications2 <- data_datab_all
}

#5.4. Create final duplications database called "remove"
duplications3 <- duplications2 %>% 
                    group_by(b_scientific_name) %>%
                    filter(paper_stock == 0) #papers with only 1 stock

remove1 <- duplications3 %>% 
              group_by(b_scientific_name) %>%
              slice(which.min(study_year)) #older papers' duplications

remove2 <- duplications2 %>% 
              group_by(b_scientific_name) %>%
              filter(paper_stock == 1) #papers with more than 1 stock

remove <- rbind(remove1, remove2)

#5.5. Delete duplicated values from original database
data_end <- anti_join(data, remove, by = "id_obs")


#6. Save a "cleaner" database
write.csv(data_end, row.names = F, "data/biblio_database1.csv")