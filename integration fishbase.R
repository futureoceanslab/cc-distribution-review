##R code for FISH base and Database Review integration
##Author: Elena ojea
##Date: April 7th 2017
##output: two csv files with stock and species data (ReviewDatst.csv", "ReviewDatsp.csv")


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

#ReviewDat <- read.csv("~/OneDrive/CLOCK_TEAM/03_FUTURE OCEANS/FO_BIBLIO/FO_DATA_ANALYSIS/R for fishbase review/biblio_database.csv")
#ReviewDat <- read.csv("C:/Users/alba.aguion/OneDrive/CLOCK_TEAM/03_FUTURE OCEANS/FO_BIBLIO/FO_DATA_ANALYSIS/R for fishbase review/biblio_database.csv")
ReviewDat <- read.csv("data/biblio_database.csv", stringsAsFactors=FALSE, header=T, sep = ";")


#TO REMOVE NICOLAS ET AL. - values too different from the rest of the observations
ReviewDat<-subset(ReviewDat, ! id_study=="6") 

#detele blank spaces in Species scientific name
trim.trailing <- function (x) sub("\\s+$", "", x)
ReviewDat$b_scientific_name <- trim.trailing(ReviewDat$b_scientific_name )

##Create a list of our species scientific names and check names
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Atheresthes\240stomias"] <- "Atheresthes stomias"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Lepidopsetta\240polyxystra"] <- "Lepidopsetta polyxystra"
SpReviewOriginal <- unique(as.character(ReviewDat$b_scientific_name)) #list of species from original database (146)
SpCodes<-as.vector(unique(ReviewDat$rfishbase_species_code)) #there are MISSING CODES (124 sp included here)
SpReview <- unlist(species_names(SpCodes))  #only select the species where we have SpCodes
spdiff <- SpReviewOriginal %in% SpReview
table(spdiff)
spmiss <- SpReviewOriginal[spdiff==FALSE] #species not included in the SpReview
#spmiss #species missing in the Spcode merging 


##getting SPECIES LEVEL data for our species list

#speciesDat <- species(SpReview, fields=c("SpecCode", "PriceCateg", "Vulnerability")) #for specific variables
speciesDat <- species(SpReview) #for all  fishbase data

#getting STOCK LEVEL data for our species list
stockdat <- stocks(SpReview)

##Now I save the new databases for fishbase data
#write.csv(speciesDat, file = "speciesDat.csv")
#write.csv(stockdat, file = "stockdat.csv")



##INCLUDE STOCK and SPECIES DTA into the REVIEW DATABASE


##here I select the variables I am interested to add at the STOCK LEVEL

fb_variables <- c("StockCode", "sciname", "StockDefs", "LocalUnique", "IUCN_Code", "Protected", "Resilience",
                  "StocksRefNo", "EnvTemp", "Abundance", "CountryComp", "Catches", "FAOAqua", "SpecCode")

mystockdat <- stockdat[, fb_variables]   #select the database for the fb_variables I want

##now I add the variables to the review database
#cleaning of the database
colnames(ReviewDat)
ReviewDat<-ReviewDat[, 1:67] #to delete empty columns

#ID variable -StockCode - to join the databases REVIEW and STOCKDAT
colnames(ReviewDat)[22] <- "StockCode"  #to put the same variable name in both databases

#put the variable sin the same format (character)
mystockdat$StockCode<-as.character(mystockdat$StockCode)
ReviewDat$StockCode<-as.character(ReviewDat$StockCode)

#ID variable -SpecCode - to join the databases REVIEW and SPECIESDAT
colnames(ReviewDat)[23] <- "SpecCode"  #to put the same variable name in both databases

#merging of review+stockdat
ReviewDatst <- left_join(ReviewDat, mystockdat, by = "StockCode")

#merging of review+speciesdat
ReviewDatsp <- left_join(ReviewDat, speciesDat, by = "SpecCode")



#Now I save the FULL data of fishbase with our reviewdata
#write.csv(ReviewDatst, file = "ReviewDatst.csv") #for the stocks data
#write.csv(ReviewDatsp, file = "ReviewDatsp.csv") #for the species data






##Additional codes for variable selection and NA omission

##Removing variables  with NA values 
#nasumsp <- colSums(is.na(speciesDat))   # to know how many missing values we have in the fishbase variables
#varselection_sp <- subset(nasumsp, nasumsp==0)
#variables_sp_fb <- row.names(as.data.frame(varselection_sp)) #list of variables from fishbase atthe stock level with no NAs

#to select  variables at the STOCK LEVEL
#dim(stockdat)
#variables <- colnames(stockdat)
#nasum <- colSums(is.na(stockdat))   # to know how many missing values we have in the fishbase variables
#varselection_stock <- subset(nasum, nasum==0) #choose the fishbase variables with no missing values
#variables_stock_fb <- row.names(as.data.frame(varselection_stock)) #list of variables from fishbase atthe stock level with no NAs

#to avoid variables with NAs in the stock and species data
#mystockdat   <- stockdat[, variables_stock_fb]   #select the non NA variables in stockdat 
#myspeciesdat <- speciesDat[, variables_sp_fb]    #select the non NA variables in speciesDat

#to create the final database with the selected variables:
#ReviewDat <- left_join(ReviewDat, myspeciesdat, by = "SpecCode")
#ReviewDat <- left_join(ReviewDat, mystockdat, by = "StockCode")


