library(tidyverse)

table <- read.table("data/biblio_database.csv", header = T, sep = ",")

table %>%
  distinct(var1, var2, .keep_all = TRUE)

##Subset of biblio_database without duplicated data
##table1<- table1%>%
##  filter(table1$duplicate=="1")