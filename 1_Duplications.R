#################################
##### FOL
##### 28/02/2019
#################################

library(tidyverse)

#1. Open database
data <- read.table("data/biblio_database.csv", header = T, sep = ",")

<<<<<<< HEAD
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
=======
  #1. CLEAN DATABASE: Clean database (from integration fishbase.R script) - to delete after finalizing 1. Duplications)
  ##delete blank columns
  colnames(table)
#detele blank spaces in Species scientific name to match review_database-fishbase_database
trim.trailing <- function (x) sub("\\s+$", "", x)
table$b_scientific_name <- trim.trailing(table$b_scientific_name )
##Check spp names to match review_database-fishbase_database
table$b_scientific_name[table$b_scientific_name=="Atheresthes\240stomias"] <- "Atheresthes stomias"##??
table$b_scientific_name[table$b_scientific_name=="Lepidopsetta\240polyxystra"] <- "Lepidopsetta polyxystra"##??
table$b_scientific_name[table$b_scientific_name=="Clupea pallasii"] <- "Clupea pallasii pallasii"
table$b_scientific_name[table$b_scientific_name=="Loligo pealeii"] <- "Doryteuthis pealeii"
table$b_scientific_name[table$b_scientific_name=="Loligo opalescens"] <- "Doryteuthis opalescens"
table$b_scientific_name[table$b_scientific_name=="Litopenaeus setiferus"] <- "Penaeus setiferus" 
## We dodnÂ´t update the name of this specie (Litopenaeus setiferus) because we miss one match-sp. But we should keep it in mind to show with the final results
table$rfishbase_species_code[table$rfishbase_species_code=="322"] <- "308"
table$rfishbase_species_code[table$rfishbase_species_code=="3"] <- NA


####2. IMPACT TYPES (from distributional_impacts) - DELETE IN FINAL SCRIPT because is in 1. Duplications
>>>>>>> 761e91e1ec1cd4650b14d6bfab8aad23a6e9be03

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

#3. Change codes to a short description; in the original review center of grav separated from center of biomass
data$b_impact_i <- data$b_impact
levels(data$b_impact) <- c("lat shift center of grav", #1
                           "lat shift center of bio",  #2
                           "depth shift", #3
                           "boundary lat shift center of grav", #4
                           "boundary lat shift center of bio", #5
                           "long shift center of grav", #7
                           "long shift center of bio", #8
                           "lat and long shift center of grav", #9
                           "shift in area occupied") #11

data$b_direction_i <- data$b_direction
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
                              "boundary lat shift north center of bio") #21

data$cc_i <- data$cc
levels(data$cc) <- c("AMO", #10
                     "climate velocity", #11
                     "sst", #2
                     "sst,bt,AMO", #2,3,10
                     "sst,bt", #2,3
                     "bt") #3

#4.Save a cleaner database
write.csv(data, row.names = F, "data/biblio_databse1.csv")
