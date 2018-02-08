##Run Data SAU if needed to upload Final_SAU_FE
##OPEN FE DATA (from Data_SAU.R using list_FE.csv)
#Read input file: the FE species catched
Final_SAU_FE <- read.csv("data/Final_SAU_FE.csv", stringsAsFactors=FALSE)
ReviewDat_Merge_SAU <- read.csv("data/ReviewDat_Merge_SAU.csv", stringsAsFactors=FALSE)


##FILTER SAU DATASETS: 5 last years, landings, NAs
Final_SAU_FE <- filter(Final_SAU_FE, year > 2009, catch_type=="Landings")

#dataframe sp - EEZ catch
counts <- Final_SAU_FE %>%
  group_by(fishing_entity, year) %>%
  tally  


# 1. FISHING ENTITIES#### 

#add fishing entities landings and catches per species to ReviewDat 

#Catches and Landings per fishing entity, EEZ and species (mean across years)
tonlandFEsp <- Final_SAU_FE %>%
  group_by(fishing_entity, area_name, scientific_name) %>%
  summarise(tonnesFEsp=mean(tonnes,na.rm=T),
            landedvalueFEsp=mean(landed_value, na.rm=T))#Why mean?????

ReviewDatFB_SAU3  <- merge(ReviewDat_Merge_SAU, tonlandFEsp, by=c("area_name","scientific_name"), all.x=TRUE)


#total catch per species for Fishing entities (sum across species and eezs)
tonlandFEspT<- tonlandFEsp %>%
              group_by(fishing_entity, scientific_name) %>%
              summarise(tonnesFEspT=sum(tonnesFEsp,na.rm=T),
                        landedvalueFEspT=sum(landedvalueFEsp, na.rm=T))

ReviewDatFB_SAU4 <- merge(ReviewDatFB_SAU3, tonlandFEspT, by=c("fishing_entity", "scientific_name"), all.x=TRUE)


#Total catch per fishing entity (mean across fishing entities)
tonlandFE<- tonlandFEspT %>%
  group_by(fishing_entity) %>%
  summarise(tonnesFE=sum(tonnesFEspT,na.rm=T),
            landedvalueFE=sum(landedvalueFEspT, na.rm=T))

ReviewDatFB_SAU5 <- merge(ReviewDatFB_SAU4, tonlandFE, by=c("fishing_entity"), all.x=TRUE)



# 2. OUTPUT FILES####
#write.csv(ReviewDatFB_SAU5, file = "data/Biblio_database_full.csv")
