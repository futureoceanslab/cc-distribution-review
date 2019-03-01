#################################
##### FOL
##### 28/02/2019
#################################

library(tidyverse)

#1. Open database
data <- read.csv("data/biblio_database.csv", sep = ";")

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
                     "bt",#3
                     "ssta", #13
                     "sst, AMO") #2,10

#2. Find duplications
#2.1 Select rows with same species 
data_sp <- data %>% 
  group_by(b_scientific_name) %>% 
  filter(n() > 1)

#TABLA PRUEBAS
#id <- c(1,1,3,4,5,6,7,8,9)
#foo <- c("a.b","a.b","a.b","b","c","c","d","e","e")
#dt <- as.data.frame(cbind(id, foo))
###############

#2.2. Select rows with same EEZ and impact response
commas <- data_sp %>% 
  filter(str_detect(eez_codes, ","))

commas_str <- as.character(commas$eez_codes)          

commas_str <- unique(commas_str)

commas_str

commas_vec <- unlist(strsplit(commas_str, ","))

dat2 <- data_sp %>% 
  group_by(b_scientific_name, b_impact) %>% 
  filter(eez_codes %in% commas_vec)

dat3 <- dat[duplicated(dat$AGE),]

inner_join(dat2,dat3)

#4.Save a cleaner database
write.csv(data, row.names = F, "data/biblio_databse1.csv")