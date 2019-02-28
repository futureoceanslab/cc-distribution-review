library(tidyverse)

table <- read.table("data/biblio_database.csv", header = T, sep = ",")

table %>%
  distinct(var1, var2, .keep_all = TRUE)
