##R code for Sea Around Us catch data into for our EEZs: CatchDatsp.csv for each species catches in EEZ 
##Author: Elena Ojea, Alba Aguion, Iratxe Rubio
##Date: June 15th 2017

#Directory: /OneDrive/CLOCK_TEAM/03_FUTURE OCEANS/FO_BIBLIO/FO_DATABASES
setwd("~/OneDrive/CLOCK_TEAM/03_FUTURE OCEANS/FO_BIBLIO/FO_DATA_ANALYSIS/R for Sea Around Us Data catches")

library(dplyr)
library(tidyr)

#info for Denmark (North Sea), Germany (North Sea), Netherlands, Norway, UK, Belgium, Sweden (West Coast), Japan (Main islands), Korea (South) and Spain (Northwest)
data.1<-read.csv("Final SAU EEZ.csv", header=T, sep = ";")
data.1<-tbl_df(data.1)

#select data from 2010 to 2014
dat <- filter(data.1, data.1$catch_type=="Landings")
dat[rowSums(is.na(dat)) != ncol(dat),]


#select the list of species from the review database
ReviewDat <- read.csv("~/OneDrive/CLOCK_TEAM/03_FUTURE OCEANS/FO_BIBLIO/FO_DATA_ANALYSIS/R for Sea Around Us Data catches/biblio_database.csv", stringsAsFactors=FALSE)
SpReview<-as.character(unique(ReviewDat$b_scientific_name))

ReviewDat$eez_countries[ReviewDat$eez_countries == "USA (Alaska, Arctic)"] <- "USA (Alaska-Subarctic)"
ReviewDat$eez_countries[ReviewDat$eez_countries == "Spain (mainland, Med and Gulf of Cadiz)"] <- "Spain (mainland Med and Gulf of Cadiz)"
ReviewDat$eez_countries[ReviewDat$eez_countries == "USA (West Coast)"] <- "USA (East Coast)"
ReviewDat$eez_countries[ReviewDat$eez_countries == "Denmark (North Sea), Germany (North Sea), Netherlands, Norway, United Kingdom (UK), Belgium, Sweden (West Coast)"] <- "Denmark (North Sea), Germany (North Sea), Netherlands, Norway, United Kingdom (UK), Belgium"



#adapt names from datacountries$b_scientific_name to SAU

ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Seriola quinqueradiata"] <- "Seriola"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Sebastes viviparus"] <- "Sebastes"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Enchelyopus cimbrius"] <- "Gaidropsarus"
ReviewDat[43,11] <- "Atheresthes stomias"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Bathyraja violacea"] <- "Raja"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Leucoraja erinacea"] <- "Raja"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Alosa pseudoharengus"] <- "Alosa"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Alosa sapidissima"] <- "Alosa"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Pseudopleuronectes americanus"] <- "Pleuronectidae"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Scophthalmus aquosus"] <- "Scophthalmidae"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Lophius americanus"] <- "Lophiidae"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Paralichthys dentatus"] <- "Paralichthyidae"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Hippoglossina oblonga"] <- "Paralichthyidae"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Gaidropsarus macrophthalmus"] <- "Gaidropsarus"
ReviewDat[162,11] <- "Lepidopsetta bilineata"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Atherina presbyter"] <- "Atherina boyeri"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Myoxocephalus octodecemspinosus"] <- "Cottidae"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Loligo pealeii"] <- "Loligo"

#check the species list
SpReview <- as.character(unique(ReviewDat$b_scientific_name))
SpSeaAroundUs <- as.character(unique(dat$scientific_name))

#como no hay match entre species names en ReviewDat y dat, elimino las que no estan
matchsp <- SpReview %in% SpSeaAroundUs
table1 <- data.frame(matchsp, SpReview)
table2 <- subset(table1, table1$matchsp==FALSE) ##species from review database missing in Sea Around Us


#Create a new dataframe for the eez_countries info, keeping the id_obs
EezCountries <- select(ReviewDat, id_obs, b_scientific_name, eez_countries)

#EezMissing <- subset(EezCountries, EezCountries$b_scientific_name %in% table2$SpReview)

#delete strange values in eez_country
#EezCountries$eez_countries[EezCountries$eez_countries=="1"] <- NA
#EezCountries$eez_countries[EezCountries$eez_countries=="10"] <- NA
#EezCountries$eez_countries[EezCountries$eez_countries=="11"] <- NA

#multiply the observations based on the number of eez_countries

DataCountries <- EezCountries %>% 
  mutate(eez_countries = strsplit(as.character(eez_countries), ",")) %>% 
  unnest(eez_countries)
colnames(DataCountries) <- c("id_obs","scientific_name", "area_name")


#remove blank spaces from variable values
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
DataCountries$area_name <- trim(DataCountries$area_name)
DataCountries$scientific_name <- trim(DataCountries$scientific_name)
dat$area_name <- as.character(dat$area_name)
dat$scientific_name <- as.character(dat$scientific_name)

#make eez area_names comparable among databases
EEZlist_review <- unique(DataCountries$area_name)
EEZlist_SAU <- unique(dat$area_name)

EEZlist_review %in% EEZlist_SAU  


##ojo que elimino las especies que no hacen match con sea around us!
DataCountriesSP <- subset(DataCountries, DataCountries$scientific_name %in% SpSeaAroundUs)

EEZlist_review <- unique(DataCountriesSP$area_name)
EEZlist_SAU <- unique(dat$area_name)

EEZlist_review %in% EEZlist_SAU

SPlist_review <- unique(DataCountriesSP$scientific_name)
SPlist_SAU <- unique(dat$scientific_name)


##calculate the total catch of a specie on each EEZ

SPCATCH <- list()
N <- dim(DataCountriesSP)[1]  #for the lenght of the loop
DataCountriesSP$id <- c(1:N)

for (i in 1:N)  #loop row by row to fill in tonnes and catches
{
  ID <- DataCountriesSP$id[i]
  #select eez observation
  EEZi <- DataCountriesSP$area_name[i]
  #select species observation
  SPi <- as.character(DataCountriesSP$scientific_name[i])
  {
    #subset Sea Around Us data
    SAU_EEZ <- subset(dat, dat$area_name==EEZi) 
    if (SPi %in% SAU_EEZ$scientific_name)
    {
      SAU_EEZ_SP <- subset(SAU_EEZ, SAU_EEZ$scientific_name==SPi)
      #create the data with landings and profits
      tonnes <- aggregate(SAU_EEZ_SP$tonnes, by=list(SAU_EEZ_SP$year), FUN=mean)
      landedvalue <- aggregate(SAU_EEZ_SP$landed_value, by=list(SAU_EEZ_SP$year), FUN=mean)
      colnames(tonnes) <- c("year", "tonnes")
      colnames(landedvalue) <- c("year", "landedvalue")
      #id, species, area_name and landings and value per country
      n<-dim(tonnes)[1]
      id_obs <- (rep(DataCountriesSP$id_obs[i], n))
      scientific_name <- rep(SPi,n)
      year <- tonnes[,1]
      tonnescatch <- tonnes[,2]
      area_name <- rep(SAU_EEZ_SP$area_name[i],n)
      landedvaluecatch <- landedvalue[,2]
      spcatch <- data.frame(id_obs, scientific_name, area_name, year, tonnescatch, landedvaluecatch)
    } 
  }
  SPCATCH[[i]] <- spcatch
}

#now I copy all the CATCH data in one table, that has the id_obs that matches ReviewDat

CatchDataEEZsp <- do.call(rbind, lapply(SPCATCH, data.frame, stringsAsFactors=FALSE))



###Now I save the file into CatchDAtEEZ.cvs for the countries dependency graphs

write.csv(CatchDataEEZsp, file = "CatchDatEEZsp.csv")
