##Run Data SAU if needed to upload Final_SAU_FE
##OPEN FE DATA (from Data_SAU.R using list_FE.csv)
#Read input file: the FE species catched
Final_SAU_FE <- read.csv("data/Final SAU FE.csv", stringsAsFactors=FALSE)
ReviewDat_Merge_SAU <- read.csv("data/ReviewDat_Merge_SAU.csv", stringsAsFactors=FALSE)


##FILTER SAU DATASETS: 5 last years, landings, NAs
Final_SAU_FE <- filter(Final_SAU_FE, year > 2009, catch_type=="Landings")

#dataframe sp - EEZ catch
counts <- Final_SAU_FE %>%
  group_by(fishing_entity, year) %>%
  tally  ##YEAR 2012 removed in ReviewDat_Merge_SaU$tonnesEEZ,landedvalueEEZ!!!!!!


# 1. FISHING ENTITIES#### 

#add fishing entities to ReviewDat by species and average years (mean across years)
Final_SAU_FE <- Final_SAU_FE %>%
  group_by(fishing_entity, area_name, scientific_name) %>%
  summarise(tonnesFEsp=mean(tonnes,na.rm=T),
            landedvalueFEsp=mean(landed_value, na.rm=T))#Why mean?????

ReviewDat <- merge(ReviewDat_Merge_SAU, Final_SAU_FE, by=c("area_name","scientific_name"), all.x=TRUE)


#total catch per species for Fishing entities (sum across species and fishing entities)
tonlandFEspT<- Final_SAU_FE %>%
              group_by(fishing_entity, scientific_name) %>%
              summarise(tonnesFEspT=sum(tonnesFEsp,na.rm=T),
                        landedvalueFEspT=sum(landedvalueFEsp, na.rm=T))

ReviewDat <- merge(ReviewDat, tonlandFEspT, by=c("fishing_entity", "scientific_name"), all.x=TRUE)


#Total catch per fishing entity (sum across fishing entities)
tonlandFE<- Final_SAU_FE %>%
  group_by(fishing_entity) %>%
  summarise(tonnesFE=sum(tonnesFEsp,na.rm=T),
            landedvalueFE=sum(landedvalueFEsp, na.rm=T))

ReviewDat <- merge(ReviewDat, tonlandFE, by=c("fishing_entity"), all.x=TRUE)



# 2. OUTPUT FILES####
write.csv(ReviewDat, file = "data/ReviewDat.csv")