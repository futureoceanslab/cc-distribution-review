##R code for Sea Around Us catch data into for our EEZs: CatchDat.csv for each EEZ catches
##Author: Elena Ojea, Alba Aguion, Iratxe Rubio
##Date: April 20th 2017

#Directory: /OneDrive/CLOCK_TEAM/03_FUTURE OCEANS/FO_BIBLIO/FO_DATABASES
library(dplyr)
library(tidyr)

#info for Denmark (North Sea), Germany (North Sea), Netherlands, Norway, UK, Belgium, Sweden (West Coast), Japan (Main islands), Korea (South) and Spain (Northwest)
data.1<-read.csv("data/Final SAU EEZ.csv", header=T, sep = ";")
data.1<-tbl_df(data.1)

#select data from 2010 to 2014 (because we average data for this period?)
data.2 <- filter(data.1, year > 2009)  
dat <- filter(data.2, catch_type=="Landings")
dat[rowSums(is.na(dat)) != ncol(dat),]

#select the list of species from the review database
ReviewDat <- read.csv("data/biblio_database.csv", stringsAsFactors=FALSE)
SpReview<-as.character(unique(ReviewDat$b_scientific_name))

ReviewDat$eez_countries[ReviewDat$eez_countries == "USA (Alaska, Arctic)"] <- "USA (Alaska-Subarctic)"
ReviewDat$eez_countries[ReviewDat$eez_countries == "Spain (mainland, Med and Gulf of Cadiz)"] <- "Spain (mainland Med and Gulf of Cadiz)"
ReviewDat$eez_countries[ReviewDat$site == "Northeast US shelf"] <- "USA (East Coast)"
ReviewDat$eez_countries[ReviewDat$eez_countries == "Denmark (North Sea), Germany (North Sea), Netherlands, Norway, United Kingdom (UK), Belgium, Sweden (West Coast)"] <- "Denmark (North Sea), Germany (North Sea), Netherlands, Norway, United Kingdom (UK), Belgium"
ReviewDat$eez_countries[ReviewDat$eez_countries == "Canada (East Coast)\n"] <- "Canada (East Coast)"

##some observations have no EZ. I put a EEZ based on location and http://www.marineregions.org/eezmapper.php
##[6] Spain(Northwest), [166-175] France (Atlantic), [176,177] Norway; [178-189] US North East coast. 

ReviewDat$eez_countries[6] <- "Spain (Northwest)" 
ReviewDat$eez_countries[166:175] <- rep("France (Atlantic Coast)", 10)
ReviewDat$eez_countries[176:177] <- rep("Norway", 2)
ReviewDat$eez_countries[178:189] <- rep("USA (East Coast)", 12)


#adapt names from datacountries$b_scientific_name to SAU (using WORMS)

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
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Loligo opalescens"] <- "Doryteuthis opalescens"

##these species do not appear in SAU so we use the more generic family
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Sebastes fasciatus"] <- "Sebastes"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Leucoraja ocellata"] <- "Raja"       #because they are <90cm
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Sebastes saxicola"] <- "Sebastes"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Stenotomus caprinus"] <- "Stenotomus chrysops"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Serranus atrobranchus"] <- "Serranus"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Penaeus duorarum"] <- "Penaeidae"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Penaeus aztecus"] <- "Penaeidae"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Amblyraja radiata"] <- "Raja"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Sebastes ruberrimus"] <- "Sebastes"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Sebastes brevispinis"] <- "Sebastes"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Raja rhina"] <- "Rajidae"  #greater than 90cm"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Raja binoculata"] <- "Rajidae"  #greater than 90cm"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Pandalus platyceros"] <- "Pandalus"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Isopsetta isolepis"] <- "Pleuronectidae"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Paralithodes platypus"] <- "Lithodidae"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Clupea pallasii"] <- "Clupea pallasii pallasii"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Pandalus eous"] <- "Pandalus"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Paralithodes aequispinus"] <- "Lithodidae"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Chionoecetes bairdi"] <- "Chionoecetes"

ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Berryteuthis magister"] <- "Mollusca"  #general but present in the alaska/subartic
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Lepidopsetta\240polyxystra"] <- "Pleuronectidae"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Scophthalmidae"] <- "Scophthalmus aquosus"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Urophycis regia"] <- "Phycis"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Atheresthes\240stomias"] <- "Atheresthes stomias"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Buglossidium luteum"] <- "Soleidae"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Arnoglossus laterna"] <- "Bothidae"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Peprilus burti"] <- "Stromateidae"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Diplectrum bivittatum"] <- "Serranus"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Cyclopsetta chittendeni"] <- "Paralichthyidae"
ReviewDat$b_scientific_name[ReviewDat$b_scientific_name=="Lithodes aequispinus"] <- "Lithodidae"



#remove blank spaces from variable values in species
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
ReviewDat$b_scientific_name <- trim(ReviewDat$b_scientific_name)
dat$scientific_name <- as.character(dat$scientific_name)

#check the species list
SpReview <- as.character(unique(ReviewDat$b_scientific_name))
SpSeaAroundUs <- as.character(unique(dat$scientific_name))


#como no hay match entre species names en ReviewDat y dat, elimino las que no estan
matchsp <- SpReview %in% SpSeaAroundUs
table1 <- data.frame(matchsp, SpReview)
table2 <- subset(table1, table1$matchsp==FALSE) ##species from review database missing in Sea Around Us




#Create a new dataframe for the eez_countries info, keeping the id_obs
EezCountries <- select(ReviewDat, id_obs, b_scientific_name, eez_countries)


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


#now I get a list of matrices with the  information on the  tonnes and landings by country for the last 5 years

CATCH <- list()
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
      tonnes <- aggregate(SAU_EEZ_SP$tonnes, by=list(SAU_EEZ_SP$fishing_entity), FUN=mean)
      landedvalue <- aggregate(SAU_EEZ_SP$landed_value, by=list(SAU_EEZ_SP$fishing_entity), FUN=mean)
      colnames(tonnes) <- c("country", "tonnes")
      colnames(landedvalue) <- c("country", "landedvalue")
      #id, species, area_name and landings and value per country
      n<-dim(tonnes)[1]
      id_obs <- (rep(DataCountriesSP$id_obs[i], n))
      scientific_name <- rep(SPi,n)
      fishing_entity <- tonnes[,1]
      tonnescatch <- tonnes[,2]
      area_name <- rep(SAU_EEZ_SP$area_name[i],n)
      landedvaluecatch <- landedvalue[,2]
      catch <- data.frame(id_obs, scientific_name, area_name, fishing_entity, tonnescatch, landedvaluecatch)
    } 
  }
  CATCH[[i]] <- catch
}



#now I copy all the CATCH data in one table, that has the id_obs that matches ReviewDat

CatchData <- do.call(rbind, lapply(CATCH, data.frame, stringsAsFactors=FALSE))




##now I need the country total landings and catch (mean across years)

TOTALCATCH <- list()
countrycatch <- unique(CatchData$fishing_entity) #country from our list ()
n <- length(countrycatch)

for (i in 1: n)  #loop row by row to fill in total catches later in CatchDat
  
{  
  country <- countrycatch[i] #for each countrycatch
   
    {
      SAU_country <- subset(dat, dat$fishing_entity==country) 
      ttonnes <- aggregate(SAU_country$tonnes, by=list(SAU_country$year), FUN=sum)
      tlandings <- aggregate(SAU_country$landed_value, by=list(SAU_country$year), FUN=sum)
      colnames(ttonnes) <- c("year", "ttonnes")
      colnames(tlandings) <- c("year", "tlandings")
      totallandings <- mean(tlandings$tlandings)
      totaltonnes <- mean(ttonnes$ttonnes)
      totalcatch <- data.frame(country,totaltonnes, totallandings)
      colnames(totalcatch) <-  c("fishing_entity", "totalcatch", "totallandings")
    } 
TOTALCATCH[[i]] <- totalcatch
}

CatchTot <- do.call(rbind, lapply(TOTALCATCH, data.frame, stringsAsFactors=FALSE))




CatchDat <- left_join(CatchData, CatchTot, by="fishing_entity")


###Now I save the file into CatchDAt.cvs for the countries dependency graphs

write.csv(CatchDat, file = "CatchDat.csv")

