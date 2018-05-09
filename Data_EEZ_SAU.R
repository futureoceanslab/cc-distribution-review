#Creation of the EEZ Catches dataset: Final_SAU_EEZ.csv
#source: EEZs that appear in the review database (included in Biblio_database.ppt)
#source catch data: Sea Around Us
#Iratxe Rubio, May 18

library(tidyverse)
path <- "data/data_EEZ_SAU/"
l <- list.files(path, pattern=".csv")

func <- function(i){
  df <- read.csv(paste0(path,l[i]))
  return(df)
}

l1 <- lapply(1:length(l), func)
final <- bind_rows(l1)

#write.csv(final, file = "data/Final_SAU_EEZ.csv",row.names = FALSE)

#verification number of EEZ names and number of rows
unique(final$area_name)
suma<-0
for (n in l1) {
    suma<-suma + nrow(n)
} 
