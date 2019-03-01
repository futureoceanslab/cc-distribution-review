##R code for FISH base and Database Review integration
##Author: Elena ojea
##Date: April 7th 2017
##Updated March 1st 2019


#install libraries
library(rfishbase)
library(cluster)
library(datasets)
library(graphics)
library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)

#open data: reviewdatabase

ReviewDat <- read_csv("data/biblio_databse3.csv")

##Create a list of our species scientific names
SpReviewOriginal <- unique(as.character(ReviewDat$b_scientific_name)) # Review_database. list of species from original database (145)
SpCodes<-as.vector(unique(ReviewDat$rfishbase_species_code)) #Fishbase-spp codes. there are MISSING CODES (122 sp-codes included here) ( there is 1 NA)
SpReview <- unlist(species_names(SpCodes)[,2]) #Fishbase-spp names. only select the species where we have SpCodes (122 spp names matched)
spdiff <- SpReviewOriginal %in% SpReview
table(spdiff) # Synthesis of matches and mismatches between review-fishbase (122 true, 23 false)
spmiss <- SpReviewOriginal[spdiff==FALSE] #species not included in the SpReview to take into account

##To chek the matches among lists and edit the list of spp lost
#write.csv(SpReviewOriginal, file="data/listSpReviewOriginal.csv")
#write.csv(SpReview, file="data/listSpReview.csv")
write.csv(spmiss, file="data/1listspmiss.csv") ##list of lost species

##GET INFO FROM FISHBASE
##getting SPECIES LEVEL data from Fishbase to our species list (122 spp out of 146 spp)
#speciesDat <- species(SpReview, fields=c("SpecCode", "PriceCateg", "Vulnerability")) #examples for specific variables
speciesDat <- species(SpReview) #for all  fishbase data
#getting STOCK LEVEL data from Fishbase to our species list (123 spp out of 146 spp)
stockdat <- stocks(SpReview)

##INCLUDE STOCK and SPECIES DATA into the REVIEW DATABASE
##Selection of interesting variables to add at the STOCK LEVEL

fb_variables <- c("StockCode", "StockDefs", "LocalUnique", "IUCN_Code", "Protected", "Resilience", 
                  "StocksRefNo", "EnvTemp", "Abundance", "CountryComp", "Catches", "FAOAqua", "SpecCode")
                #sciname is not a variable anymore
mystockdat <- stockdat[, fb_variables]   #selection of database from which I extract fb_variables

#ID variable -StockCode - to join the databases REVIEW and STOCKDAT
colnames(ReviewDat)[22] <- "StockCode"  #to put the same variable name in both databases to join them
#Put the variables of both databases  in the same format (as character)
mystockdat$StockCode<-as.character(mystockdat$StockCode)
ReviewDat$StockCode<-as.character(ReviewDat$StockCode)

#ID variable -SpecCode - to join the databases REVIEW and SPECIESDAT
colnames(ReviewDat)[23] <- "SpecCode"  #to put the same variable name in both databases to join them

##MERGING
# Review+stockdat
ReviewDatst <- left_join(ReviewDat, mystockdat, by = "StockCode")
#Review+speciesdat
ReviewDat$SpecCode<-as.integer(ReviewDat$SpecCode)
ReviewDatsp <- left_join(ReviewDat, speciesDat, by = "SpecCode")

#Save the FULL data (stockdat and speciesdat) of fishbase with our reviewdata
#write.csv(ReviewDatst, file = "data/ReviewDatst.csv") #for the stocks data
#write.csv(ReviewDatsp, file = "data/ReviewDatsp.csv") #for the species data
