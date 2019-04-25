#####################################
# IMPACT AGGREGATION CODE ###########
#FOL, February 28th 2019 #####
#####################################

library(tidyverse)

#Open database (Biblio_database.csv)
data <- read.csv("data/biblio_database2.csv", stringsAsFactors = F)


##AGGREGATION OF IMPACTS (EEZ, Species name, impact type)

#What's the data like? Tabe with EEZ, species and Values
mytable <- as.data.frame(table(data$eez_codes, data$b_scientific_name, data$b_impact))

##Split impacts into different EEZs: create an observation per EEZ 

ReviewDat <- data %>% 
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
                  group_by(eez_countries, b_scientific_name, b_impact_combine) %>%
                  summarise(n())
  
colnames(sp_eez_impact) <- c("eez_countries", "b_scientific_name", "b_impact_combine", "N")

#SUBSET with the species and EEZs where we have duplicates
repeated_sp_eez_impact <- subset(sp_eez_impact, N > 1) 

#N review dat species in EEZs -> 629
#dim(sp_eez_impact)
#N species repeated in a EEZ -> 90
#dim(repeated_sp_eez_impact)


##AGGREGATION

#first I select the variables in dataframe we need (simplify dataframe)
df <- ReviewDat %>%  
        select(id_obs, 
               id_study, eez_countries, 
               b_scientific_name, 
               cc, b_impact_combine, b_direction_combine,
               b_value, rfishbase_species_code)  %>%
        group_by(eez_countries, b_scientific_name, b_impact_combine) %>% 
        mutate(total = n())


ddc <- df[df$total == 1,] #subsets those without duplicates
ddd <- df[df$total >= 2,] #subsets those with duplicates

#here I combine b_value to calculate the mean (to get 1 b_impact value per eez):
ddd <- ddd %>% 
        group_by(eez_countries, b_scientific_name, b_impact_combine) %>% 
        mutate(b_value_x = mean(b_value, na.rm = T))  #mean value of b_value in eez-sp-impact

#here I aggregate the info from the rest of columns:
ddd2 <- ddd %>% 
          group_by(eez_countries, b_scientific_name, b_impact_combine) %>% 
          summarise(id_obs = paste(id_obs, collapse = "-"), #all values of id_obs
                    id_study = paste(id_study, collapse = "-"),
                    cc = paste(cc, collapse = "-"),
                    b_direction_combine = paste(b_direction_combine, collapse = "-"),
                    b_value = paste(b_value, collapse = "-"),
                    rfishbase_species_code = unique(rfishbase_species_code),
                    total = mean(total),
                    b_value_x = mean(b_value_x))

colnames(ddd2) #change column name to have b_value aggregated and be able to merge with non duplicated data
colnames(ddd2)[which(names(ddd2) == "b_value")] <- "b_value_original"
colnames(ddd2)[which(names(ddd2) == "b_value_x")] <- "b_value"
colnames(ddd2)[which(names(ddd2) == "total")] <- "duplicated_times"
colnames(ddc)[which(names(ddc) == "total")] <- "duplicated_times"
colnames(ddc)[which(names(ddc) == "b_value")] <- "b_value_original"

ddc$duplicated_times[ddc$duplicated_times == 1] <- 0
ddd2$duplicated_times[ddd2$duplicated_times >= 2] <- 1

ddc$b_value <- ddc$b_value_original

#determine observations with opposite b_directions
ddc$verification_b_dir <- 0 #same direction

a <- ddd2 %>%
      filter(str_detect(b_direction_combine, "deeper.*shallower"))
b <- ddd2 %>%
      filter(str_detect(b_direction_combine, "north.*south"))
dirs <- rbind(a, b)

ddd2$verification_b_dir <- 0 #same direction
ddd2[ddd2$id_obs %in% dirs$id_obs, 12] <- 1 #opposite directions

#Now I merge the subsets of non-duplications (ddc) and with duplications removed/averaged (ddd)
data_end <- merge(ddc, ddd2, all = T)
write.csv(data_end, row.names = F, "data/biblio_database3.csv")
