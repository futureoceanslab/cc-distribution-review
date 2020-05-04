##This script creates 1 SAU database for EEZs
##INPUT FILES: data_EEZ_SAU/
##OUTPUT FILES: SAU_dataset_EEZ.csv

library(tidyverse)

########1. Download SAU Databases by EEZ (into "data_EEZ_SAU/")####
#We manuallly download each EEZ database from the SAU website
#We determine the EEZs that need to be downloaded from our biblio_database
EEZ <- read.csv("data/biblio_database2.csv")
EEZ_list <- unique(as.character(EEZ$stock_EEZ_country))#27 EEZs

#once we have downloaded EEZs in the "data_EEZ_SAU/" folder
########2. Create a unique database of SAU EEZs####
path <- "data/data_EEZ_SAU/"
l <- list.files(path, pattern=".csv")

func <- function(i){
  df <- read.csv(paste0(path,l[i]))
  if (length(df) == 17) {
    df <- df[-which(names(df) == "end_use_type")]  
  }
  return(df)
}

l1 <- lapply(1:length(l), func)
final <- do.call(rbind, l1)

#verification number of EEZ names and number of rows
unique(final$area_name)
suma <- 0
for (n in l1) {
  suma <- suma + nrow(n)
} 

##FILTER SAU DATASETS: 5 last years, landings, NAs
Final_SAU_EEZ <- filter(final, year > 2009, catch_type == "Landings")
rm(final, l, l1, suma, path, func, n)

write.csv(Final_SAU_EEZ, row.names = F, "data/SAU_dataset_EEZ.csv")