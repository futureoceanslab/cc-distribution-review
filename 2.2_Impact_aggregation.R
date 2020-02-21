#####################################
# IMPACT AGGREGATION CODE ###########
#FOL, February 28th 2019 #####
#####################################

library(tidyverse)

data <- read.csv("data/biblio_database1.csv")

##AGGREGATION OF IMPACTS (EEZ, Species name, impact type)

#What's the data like? Tabe with EEZ, species, impact type and number of observations
mytable <- as.data.frame(table(data$stock_EEZ_code, data$scientific_name, data$response))

##Split impacts into different EEZs: create an observation per EEZ 
ReviewDat <- data %>% 
              mutate(stock_EEZ_country = strsplit(as.character(stock_EEZ_country), "-")) %>% 
              unnest(stock_EEZ_country)

#remove blank spaces from eez_countries
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
ReviewDat$stock_EEZ_country <- trim(ReviewDat$stock_EEZ_country)
#unique(ReviewDat$stock_EEZ_country)
#unique(ReviewDat$scientific_name)
#levels(ReviewDat$response)

# #Summarize impacts by species, eez and impact type
sp_EEZ_impact <- ReviewDat %>%
                  group_by(stock_EEZ_country, scientific_name, response) %>%
                  summarise(n())

colnames(sp_EEZ_impact)[4] <- "N"
# 
# #SUBSET with the species and EEZs where we have duplicates
repeated_sp_EEZ_impact <- subset(sp_EEZ_impact, N > 1)

#N review dat species in EEZs -> 551
#dim(sp_EEZ_impact)
#N species repeated in a EEZ -> 85
#dim(repeated_sp_EEZ_impact)

##AGGREGATION

#first I select the variables in dataframe we need (simplify dataframe)
df <- ReviewDat %>%  
        select(id_obs, id_study, stock_EEZ_country, 
               scientific_name, cc_driver_detail, response, direction,
               decadal_change, fishbase_id_species, FAO_area)  %>%
        group_by(stock_EEZ_country, scientific_name, response) %>% 
        mutate(total = n())

ddc <- df[df$total == 1,] #subsets those without duplicates
ddd <- df[df$total >= 2,] #subsets those with duplicates

#here I combine decadal_change to calculate the mean (to get 1 impact value per eez):
ddd <- ddd %>% 
        group_by(stock_EEZ_country, scientific_name, response) %>% 
        mutate(decadal_change_x = mean(decadal_change, na.rm = T))  #mean value of decadal change in eez-sp-impact

#here I aggregate the info from the rest of columns:
ddd2 <- ddd %>% 
          group_by(stock_EEZ_country, scientific_name, response) %>% 
          summarise(id_obs = paste(id_obs, collapse = "-"), #all values of id_obs
                    id_study = paste(id_study, collapse = "-"),
                    cc_driver_detail = paste(cc_driver_detail, collapse = "-"),
                    decadal_change = paste(decadal_change, collapse = "-"),
                    direction = paste(direction, collapse = "-"),  # ME adding this 
                    fishbase_id_species = paste(fishbase_id_species, collapse = "-"),
                    total = mean(total),
                    decadal_change_x = mean(decadal_change_x),
                    FAO_area = paste(unique(FAO_area)))

colnames(ddd2) #change column name to have decadal_change aggregated and be able to merge with non duplicated data
colnames(ddd2)[which(names(ddd2) == "decadal_change")] <- "decadal_change_original"
colnames(ddd2)[which(names(ddd2) == "decadal_change_x")] <- "decadal_change"
colnames(ddd2)[which(names(ddd2) == "total")] <- "duplicated_times"
colnames(ddc)[which(names(ddc) == "total")] <- "duplicated_times"
colnames(ddc)[which(names(ddc) == "decadal_change")] <- "decadal_change_original"

ddc$duplicated_times[ddc$duplicated_times == 1] <- 0
ddd2$duplicated_times[ddd2$duplicated_times >= 2] <- 1

ddc$decadal_change <- ddc$decadal_change_original

#determine observations with opposite b_directions
ddc$verification_dir <- 0 #same direction

a <- ddd2 %>%
      filter(str_detect(direction, "deeper.*shallower")) 
b <- ddd2 %>%
      filter(str_detect(direction, "polewards.*towards the equator")) 
dirs <- rbind(a, b)

ddd2$verification_dir <- 0 #same direction
ddd2[ddd2$id_obs %in% dirs$id_obs, which(colnames(ddd2)=="verification_dir")] <- 1 #opposite directions

#Now I merge the subsets of non-duplications (ddc) and with duplications removed/averaged (ddd)
data_end <- merge(ddc, ddd2, all = T)
write.csv(data_end, row.names = F, "data/biblio_database2.csv")