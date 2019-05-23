##R code for FISH base and Database Review integration
##Author: Elena ojea
##Date: April 7th 2017
##Updated March 1st 2019

library(rfishbase)
library(tidyverse)

#open data: reviewdatabase

ReviewDat <- read.csv("data/biblio_database3.csv")

##Create a list of our species scientific names
SpReviewOriginal <- unique(as.character(ReviewDat$b_scientific_name)) # Review_database. list of species from original database (145)
SpCodes <- as.vector(unique(ReviewDat$rfishbase_species_code)) #Fishbase-spp codes. there are MISSING CODES (122 sp-codes included here) ( there is 1 NA)
SpReview <- unlist(species_names(SpCodes)[,2]) #Fishbase-spp names. only select the species where we have SpCodes (122 spp names matched)
spdiff <- SpReviewOriginal %in% SpReview
table(spdiff) # Synthesis of matches and mismatches between review-fishbase (122 true, 23 false)
spmiss <- SpReviewOriginal[spdiff == F] #species not included in the SpReview to take into account

##To chek the matches among lists and edit the list of spp lost
#write.csv(SpReviewOriginal, file="data/listSpReviewOriginal.csv")
#write.csv(SpReview, file="data/listSpReview.csv")
#write.csv(spmiss, file="data/1listspmiss.csv") ##list of lost species

##GET INFO FROM FISHBASE
##getting SPECIES LEVEL data from Fishbase to our species list (122 spp out of 146 spp)
#speciesDat <- species(SpReview, fields=c("SpecCode", "PriceCateg", "Vulnerability")) #examples for specific variables
speciesDat <- species(SpReview) #for all  fishbase data


##INCLUDE SPECIES DATA into the REVIEW DATABASE
#ID variable -SpecCode - to join the databases REVIEW and SPECIESDAT
colnames(ReviewDat)[which(names(ReviewDat) == "rfishbase_species_code")] <- "SpecCode"  #to put the same variable name in both databases to join them

##MERGING
#Review+speciesdat
ReviewDat$SpecCode <- as.integer(ReviewDat$SpecCode)
ReviewDatsp <- left_join(ReviewDat, speciesDat, by = "SpecCode")

#Select relevant fishbase data
ReviewDatsp <-  subset(ReviewDatsp, select = c(id_obs, id_study, eez_countries, b_scientific_name, 
                                           cc, b_impact_combine, b_value, SpecCode, duplicated_times, b_value_original, 
                                           DemersPelag, Importance, PriceCateg, PriceReliability, MainCatchingMethod))

#Save the selected data (speciesdat) of fishbase with our reviewdata
write.csv(ReviewDatsp, file = "data/ReviewDatsp.csv")
