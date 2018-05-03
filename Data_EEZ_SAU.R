library(tidyverse)
path <- "data/data_EEZ_SAU/"
l <- list.files(path, pattern=".csv")

func <- function(i){
  df <- read.csv(paste0(path,l[i]))
  return(df)
}

l1 <- lapply(1:length(l), func)
final <- bind_rows(l1)

#verification number of EEZ names and number of rows
unique(final$area_name)
suma<-0
for (n in l1) {
    suma<-suma + nrow(n)
} 
