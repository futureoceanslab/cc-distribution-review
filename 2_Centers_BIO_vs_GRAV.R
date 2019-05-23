#################################
##### FOL
##### 28/02/2019
#################################

library(rockchalk)
library(tidyverse)

##############
#1. Statistics #
##############
#depth and area are all observations from center of biomass

#to date boundary latitude only 1 obs for biomass so no suidata for a statistical test

#lat+long, long not done

data <- read.csv("data/biblio_database1.csv")

#FOR LATITUDE: 
data$b_impact_original <- as.factor(data$b_impact_original)
data$b_years <- as.numeric(as.character(data$b_years))

latitude1 <- subset(data, b_impact_original == "1", select = c(b_value, b_impact_original)) #center of grav
latitude2 <- subset(data, b_impact_original == "2", select = c(b_value, b_impact_original)) #center of bio
latitude <- rbind(latitude1,latitude2)
latitude$b_impact_original <- factor(latitude$b_impact_original)

tapply(latitude$b_value, latitude$b_impact_original, mean)
tapply(latitude$b_value, latitude$b_impact_original, var)

boxplot(latitude$b_value ~ latitude$b_impact_original)

#Normality and Homocedasticity
hist(latitude$b_value) #Kinda of N distribution
hist(log(latitude$b_value)) #More N distribution

tapply(latitude$b_value, latitude$b_impact_original, shapiro.test) # NO NORMAL DISTRI
bartlett.test(latitude$b_value, latitude$b_impact_original) #HOMOCEDASTICITY 

boxplot(latitude$b_value ~ latitude$b_impact_original)
boxplot(log(latitude$b_value + 1) ~ latitude$b_impact_original)

#NO PARAMETRIC TEST - not very strong test
kruskal.test(b_value ~ b_impact_original, data = latitude) # < 0.05

#RUNNING ANOVAS NO TRANS AND LOG TRANS
m1 <- aov(latitude$b_value ~ latitude$b_impact_original)
b_value_transformed <- log(102 + latitude$b_value) #get rid of negative values we add a constant

m2<-aov(b_value_transformed ~ latitude$b_impact_original)

summary(m1)
summary(m2) #No significant differences. OK to combine them


#############
#2. COMBINATION
#############
#new colums with center of grav combined with center of biomass
data["b_impact_combine"] <- data$b_impact
data$b_impact_combine <- combineLevels(data$b_impact_combine, levs = c("lat shift center of grav", "lat shift center of bio"), newLabel = c("lat shift"))
data$b_impact_combine <- combineLevels(data$b_impact_combine, levs = c("boundary lat shift center of grav", "boundary lat shift center of bio"), newLabel = c("boundary lat shift"))
data$b_impact_combine <- combineLevels(data$b_impact_combine, levs = c("long shift center of grav", "long shift center of bio"), newLabel = c("long shift"))
levels(data$b_impact_combine)[levels(data$b_impact_combine) == "lat and long shift center of grav"] <- "lat and long shift"

data["b_direction_combine"] <- data$b_direction
data$b_direction_combine <- combineLevels(data$b_direction_combine, levs = c("lat shift north center of grav", "lat shift north center of bio"), newLabel = c("lat shift north"))
data$b_direction_combine <- combineLevels(data$b_direction_combine, levs = c("lat shift south center of grav", "lat shift south center of bio"), newLabel = c("lat shift south"))
data$b_direction_combine <- combineLevels(data$b_direction_combine, levs = c("boundary lat shift north center of grav", "boundary lat shift north center of bio"), newLabel = c("boundary lat shift north"))
levels(data$b_direction_combine)[levels(data$b_direction_combine) == "max lat shift north center of grav"] <- "max lat shift north"
levels(data$b_direction_combine)[levels(data$b_direction_combine) == "max lat shift south center of grav"] <- "max lat shift south"
levels(data$b_direction_combine)[levels(data$b_direction_combine) == "min lat shift north center of grav"] <- "min lat shift north"
levels(data$b_direction_combine)[levels(data$b_direction_combine) == "shift southweastwards center of grav"] <- "shift southweastwards"
levels(data$b_direction_combine)[levels(data$b_direction_combine) == "shift northeastwards center of grav"] <- "shift northeastwards"
levels(data$b_direction_combine)[levels(data$b_direction_combine) == "shift westwards center of grav"] <- "shift westwards"
levels(data$b_direction_combine)[levels(data$b_direction_combine) == "boundary lat shift east center of bio"] <- "boundary lat shift east"


#############
#3. DUPLICATIONS
#############
#3.1 Select rows with same species 
data_sp <- data %>% 
  group_by(b_scientific_name) %>% 
  filter(n() > 1)

#3.2. Select rows with same EEZ and impact response
commas <- data_sp %>% 
  filter(str_detect(eez_codes, ",")) #rows with more than one EEZ

commas_str <- as.character(commas$eez_codes)  #save names of these EEZs  
commas_str <- unique(commas_str) #keep one repetition of each EEZ
commas_vec <- unlist(strsplit(commas_str, ",")) #split EEZ names

data_dupl <- data_sp %>% #single duplications: "eez" vs "eez-eez"
  group_by(b_scientific_name, b_impact_combine, b_years) %>% 
  filter(eez_codes %in% commas_vec)

data_eez_all <- data_sp %>% #more duplications "eez-eez" vs "eez-eez"
  group_by(b_scientific_name, b_impact_combine, eez_codes, b_years)  %>% 
  filter(n() > 1)

if (dim(data_dupl)[1] > 0 ) { #merge duplicates
  duplications1 <- rbind(data_dupl, data_eez_all)
} else {
  duplications1 <- data_eez_all
}

#3.3. Select rows already having same EEZ and impact response
# but also having same database
commas2 <- duplications1 %>% 
  filter(str_detect(fish_data_source, ",")) #rows with more than one database

commas_str2 <- as.character(commas2$fish_data_source)  #save names of these databases 
commas_str2 <- unique(commas_str2) #keep one repetition of each database
commas_vec2 <- unlist(strsplit(commas_str2, ",")) #split databases names

data_dupl2 <- duplications1 %>% #single duplications: "datab" vs "datab-datab"
  group_by(b_scientific_name, b_impact_combine, b_years) %>% 
  filter(fish_data_source %in% commas_vec2)

data_datab_all <- duplications1 %>% #more duplications "datab-datab" vs "datab-datab"
  group_by(b_scientific_name, b_impact_combine, fish_data_source, b_years)  %>% 
  filter(n() > 1)

if (dim(data_dupl2)[1] > 0 ) { #merge duplications
  duplications2 <- rbind(data_dupl2, data_datab_all)
} else {
  duplications2 <- data_datab_all
}

#3.4. Create final duplications database called "remove"
duplications3 <- duplications2 %>% 
  group_by(b_scientific_name) %>%
  filter(paper_stock == 0) #papers with only 1 stock

remove1 <- duplications3 %>% 
  group_by(b_scientific_name) %>%
  slice(which.max(study_year)) #older papers' duplications

remove2 <- duplications2 %>% 
  group_by(b_scientific_name) %>%
  filter(paper_stock == 1) #papers with more than 1 stock

remove <- rbind(remove1, remove2)

#3.5. Delete duplicated values from original database
data_end <- anti_join(data, remove, by = "id_obs")


#4. NEW DATABASE
write.csv(data_end, row.names = F, "data/biblio_database2.csv")