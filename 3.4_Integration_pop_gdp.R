##################################################################
##### This script integrates our database with gdp and pop data
##### from the World Bank.
##### 01/05/2020
##### INPUT FILES: biblio_database_full0.csv
##### OUTPUT FILES: biblio_database_full.csv
##################################################################

library(tidyverse)
library(WDI)


#####1. Open Biblio_data with SAU data on EEZ and FE####
data <- read.csv("data/biblio_database_full0.csv")
data$fishing_entity <- as.character(data$fishing_entity)


#####2. World pop datapercountry####

#download pop per country in2017
pop <- WDI(indicator = 'SP.POP.TOTL', country = "all", start = 2014, end = 2017)
colnames(pop)[2:3] <- c("fishing_entity", "pop")

#detect names mismatches between data and pop, 7 found
unique(data$fishing_entity)[which(unique(data$fishing_entity) %in% unique(pop$fishing_entity) == F)]

#correct the country names in pop (to keep detail in database_full and avoid 
#creation of more duplicates)
unique(pop$fishing_entity)[grep("United", unique(pop$fishing_entity))]
pop$fishing_entity[pop$fishing_entity == "United States"] <- "USA"
unique(pop$fishing_entity)[grep("Faroe", unique(pop$fishing_entity))]
pop$fishing_entity[pop$fishing_entity == "Faroe Islands" ] <- "Faeroe Isl"
unique(pop$fishing_entity)[grep("Korea", unique(pop$fishing_entity))]
pop$fishing_entity[pop$fishing_entity == "Korea, Rep."] <- "Korea (South)"
#bear in mind that St Pierre & Miquelon and Azore Islands need to be associated with
#France and Portugal after merging the databases. This is to avoid duplications

countries_data <- unique(data$fishing_entity)
pop_data <- subset(pop, fishing_entity %in% countries_data)
pop_data <- pop_data[, c(2,3)]

miss_val <- pop_data %>% #there are no missing values
              group_by(fishing_entity) %>%
              summarise(count = n())

pop_data <- pop_data %>%
              group_by(fishing_entity) %>%
              summarise(pop = mean(pop, na.rm = T))

data <- left_join(data, pop_data, by = "fishing_entity")
#BEAR in mind these 2!!!!
data$pop[data$fishing_entity == "Saint Pierre & Miquelon (France)"] <- pop_data$pop[pop_data$fishing_entity == "France"]
data$pop[data$fishing_entity == "Azores Isl"] <- pop_data$pop[pop_data$fishing_entity == "Portugal"]


######3. GDP per country (in US $)####
gdp <- WDI(indicator = 'NY.GDP.MKTP.CD', country = "all", start = 2014, end = 2017)
colnames(gdp)[2:3] <- c("fishing_entity", "gdp")

#detect names mismatches between data and gdp, 7 found, same as before!!
unique(data$fishing_entity)[which(unique(data$fishing_entity) %in% unique(gdp$fishing_entity) == F)]

#correct the country names in gdp (to keep detail in database_full and avoid 
#creation of more duplicates)
gdp$fishing_entity[gdp$fishing_entity == "United States"] <- "USA"
gdp$fishing_entity[gdp$fishing_entity == "Faroe Islands" ] <- "Faeroe Isl"
gdp$fishing_entity[gdp$fishing_entity == "Korea, Rep."] <- "Korea (South)"
#bear in mind again St Pierre & Miquelon and Azore Islands 

gdp_data <- subset(gdp, fishing_entity %in% countries_data)
gdp_data <- gdp_data[, c(2,3)]

miss_val2 <- gdp_data %>% #there are no missing values
              group_by(fishing_entity) %>%
              summarise(count = n())

gdp_data <- gdp_data %>%
              group_by(fishing_entity) %>%
              summarise(gdp = mean(gdp, na.rm = T))

data <- left_join(data, gdp_data, by = "fishing_entity")
#BEAR in mind these 2!!!!
data$gdp[data$fishing_entity == "Saint Pierre & Miquelon (France)"] <- gdp_data$gdp[gdp_data$fishing_entity == "France"]
data$gdp[data$fishing_entity == "Azores Isl"] <- gdp_data$gdp[gdp_data$fishing_entity == "Portugal"]


######4. Relative indicators calculation####

#Per capita sp catches: catches by FE in EEZ of impacted species/pop in country
data$catchpercapita <- data$tonnesFEsp/data$pop
range(data$catchpercapita, na.rm = T)

#Relative landed value per GDP
data$landedperGDP <- data$landedvalueFEsp/data$gdp
range(data$landedperGDP, na.rm = T)


######5. OUTPUT FILE####
write.csv(data, file = "data/biblio_database_full.csv", row.names = F)