library(tidyverse)

table <- read.table("data/biblio_database.csv", header = T, sep = ",", fileEncoding="latin1")

table %>%
  distinct(b_scientific_name, eez_countries, "...", .keep_all = TRUE)

####Impact types (from distributional_impacts) - DELETE IN FINAL SCRIPT because is in 1. Duplications

#Factors
table$researcher<-as.factor(table$researcher)
table$id_study<-as.factor(table$id_study)
table$cc<-as.factor(table$cc)
table$b_impact<-as.factor(table$b_impact)
table$b_direction<-as.factor(table$b_direction)
table$tax_group<-as.factor(table$tax_group)

#Numeric
table$b_value<-as.numeric(as.character(table$b_value), na.omit=TRUE)
table$b_years<-as.numeric(as.character(table$b_years))

#change codes to a short description; in our review center of grav separated from center of biomass
levels(table$b_impact)<-c("lat shift center of grav", #1
                              "lat shift center of bio",  #2
                              "depth shift", #3
                              "boundary lat shift center of grav", #4
                              "boundary lat shift center of bio", #5
                              "long shift center of grav", #7
                              "long shift center of bio", #8
                              "lat and long shift center of grav", #9
                              "shift in area occupied") #11

levels(table$b_direction)<-c("lat shift north center of grav", #1
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

levels(table$cc)<-c("AMO", #10
                        "climate velocity", #11
                        "sst", #2
                        "sst,bt,AMO", #2,3,10
                        "sst,bt", #2,3
                        "bt") #3

write.csv(table, "biblio_databse1.csv")