##################################################################
##### This script looks for species names matches between SAU EEZ 
##### database and our biblio_database using Worms database as a
##### as a reference for species names
##### database
##### 30/03/2020
##### INPUT FILES: SAU_dataset_EEZ.csv, biblio_database2.csv
##### OUTPUT FILES: biblio_database3.csv and names to be corrected 
##### in script 1 and biblio_database3 with some species transformed 
##### to genus
##################################################################

library(tidyverse)
library(worrms) #functions to match species names in worms database 

#1. Read databases####
Final_SAU_EEZ <- read.csv("data/SAU_dataset_EEZ.csv")
ReviewDatFB <- read.csv("data/biblio_database2.csv", stringsAsFactors = F) ## biblio_database without duplicates with EEZ structure

##2. MATCH SPECIES NAMES IN REVIEW AND SAU####
##Check list of un-matchig names
Sp_ReviewDatFB <- unique(ReviewDatFB$scientific_name) #list species in review
Sp_SAU <- as.character(unique(Final_SAU_EEZ$scientific_name)) #list species in SAU

#compare species in Review and SAU
matchsp <- Sp_ReviewDatFB %in% Sp_SAU
table(matchsp) ##INITIAL round witout corrections: 80 spp no macth, 127 spp macthed 
#(total:207spp) *will be updated with new observations
#FINAL round (after applying names corrections): 70 spp no match, 132 matched (total 202 spp)
spmiss1 <- Sp_ReviewDatFB[matchsp == F] ## list of unmatching(lost) species 80-->70!!!

#Errors detected after running the loop. The loop stops when there are species
#that do not exist in Worms(*). To see the whole loop process the code 
#in l.27-33 should not be run, nor species names corrections in script 1. 
# spmiss1[53] <- "Dentex macrophthalmus" #typo(*): Dentex macropthalmus
# spmiss1[54] <- "Dicologlossa cuneata" #typo(*): Dicologoglosa cuneata
# spmiss1[57] <- "Illex coindetii" #typo(*): Ilex coindetii
# spmiss1[64] <- "Pseudotolithus senegalensis" #typo(*): Psuedotolithus senegalensis
# spmiss1[80] <- "Amblyraja radiata" #typo(*): Amblyraha radiata
# spmiss1[57] <- "Illex illecebrosus" #replacement with worms sp
# spmiss1[46] <- "Zenopsis conchifer" #replacement with worms sp
# 
# c(spmiss1[53],spmiss1[54],spmiss1[57],spmiss1[64],spmiss1[80],
#   spmiss1[57],spmiss1[46]) %in% Sp_SAU #5 species detected from the loop
# #they have multiple names, one of them matches with the one in SAU
# 
# spmiss1 <- spmiss1[-c(20,23)]#Atheresthes spp.; Chionoecetes hybrid
# #we deletee these species to allow the loop working. The loop stops when there
# #are species that do not exist in Worms

# #We create a loop that iterates through the missing spp of our database and finds
# #alternative names from worms that match with SAU
# name <- spmiss1
# count <- 1
# for (i in name) {
#   if(wm_records_name(i)[5] == "unaccepted"){ #if the scientific name is unaccepted
#     valid_name <- wm_records_name(i)[10] #select the valid accepted name
#     vec_valid_name <- as.character(valid_name)
#     print(paste(count, i))
#     print(vec_valid_name %in% Sp_SAU) #and verify if it matches with SAU names
#   } else { #if the scientific name is accepted
#     name_list <- wm_name2id_(i) #select accepted synonyms
#     id_worms <- name_list[[1]]
#     tryCatch(table_synonyms <- as.matrix(wm_synonyms(id_worms)[,3]), #when synonyms are not found worms function gives an error
#                                 error = function(e) print(paste("WRONG BOOLEAN PRINT. DOES NOT HAVE AN ALTERNATIVE NAME IN WORMS DATABASE!!!!!!!!!!!!!!!!!!!!!!!!! sp number", count)))
#     vec_names <- as.character(as.matrix(table_synonyms))
#     print(paste(count, i))
#     print(vec_names %in% Sp_SAU) #and check if they appear in SAU
#   }
#   count <- count + 1
# }
#then we check the printed results from the loop manually
# #sp Zenopsis conchifera and Illex coindetii give a TRUE
# wm_name2id_("Illex coindetii") #55
# wm_synonyms(140621)[,3] #the first value was TRUE from the loop
# #so we can replace Illex coindetii by Illex illecebrosus
# "Illex illecebrosus" %in% Sp_SAU #verification
# 
# wm_name2id_("Zenopsis conchifera") #44, not accepted name from loop
# wm_records_name("Zenopsis conchifera")[10] #accepted name
# "Zenopsis conchifer" %in% Sp_SAU #verification

#DETECTED CORRECTIONS
# #compare species in Review and SAU, a second time, after corrections found thanks to the loop
# Sp_ReviewDatFB[grep("Dentex macropthalmus", Sp_ReviewDatFB)] <- "Dentex macrophthalmus" #typo: Dentex macropthalmus
# Sp_ReviewDatFB[grep("Dicologoglosa cuneata", Sp_ReviewDatFB)] <- "Dicologlossa cuneata" #typo: Dicologoglosa cuneata
# Sp_ReviewDatFB[grep("Ilex coindetii", Sp_ReviewDatFB)] <- "Illex coindetii" #typo: Ilex coindetii
# Sp_ReviewDatFB[grep("Psuedotolithus senegalensis",Sp_ReviewDatFB)] <- "Pseudotolithus senegalensis" #typo: Psuedotolithus senegalensis
# Sp_ReviewDatFB[grep("Amblyraha radiata", Sp_ReviewDatFB)] <- "Amblyraja radiata" #typo: Amblyraha radiata
# Sp_ReviewDatFB[grep("Illex coindetii", Sp_ReviewDatFB)] <- "Illex illecebrosus" #replacement with worms sp
# Sp_ReviewDatFB[grep("Zenopsis conchifera", Sp_ReviewDatFB)] <- "Zenopsis conchifer" #replacement with worms sp
# #identified from old scripts, they were manually checked
# Sp_ReviewDatFB[grep("Loligo opalescens", Sp_ReviewDatFB)] <- "Doryteuthis opalescens"
# Sp_ReviewDatFB[grep("Loligo pealeii", Sp_ReviewDatFB)] <- "Doryteuthis pealeii"
# Sp_ReviewDatFB[grep("Clupea pallasii", Sp_ReviewDatFB)] <- "Clupea pallasii pallasii"
# Sp_ReviewDatFB[grep("Clupea pallasi", Sp_ReviewDatFB)] <- "Clupea pallasii pallasii"
# #We don't update the name of this species (Litopenaeus setiferus) because it matches SAU

matchsp2 <- Sp_ReviewDatFB %in% Sp_SAU
table(matchsp2) ## from 80 to 70 lost species
spmiss2 <- Sp_ReviewDatFB[matchsp2 == F] ## 

#The next step is to try to match genus
#spmiss2[grep("Atheresthes spp.", spmiss2)] <- "Atheresthes"
spmiss2[grep("Chionoecetes hybrid", spmiss2)] <- NA #this species does not exist in Worms

#this loop iterates through the missing spp of our database and finds matching genus
#with SAU, it creates a genus_list string containing matches
cont <- 1
cont2 <- 1
genus_list <- list()
for (i in spmiss2) {
  print(paste(wm_records_name(i)[18] %in% Sp_SAU, cont, i)) #column containing genus from worms
  if(wm_records_name(i)[18] %in% Sp_SAU == T) {
    genus_list[cont2] <- wm_records_name(i)[18] #genus name from worms
    cont2 <- cont2 + 1
  }
  cont <- cont + 1
}
#replace species names in spmiss2 by genus to match species with SAU
genus_list <- as.character(genus_list)
genus_list %in% Sp_SAU
genus_list <- unique(genus_list)

#add back the non-existent species we deleted for the loop
spmiss2[is.na(spmiss2)] <- "Chionoecetes hybrid" #this species does not exist in Worms

#loop to replace species by genus
spmiss3 <- spmiss2
for (i in 1:length(genus_list)) {
  for (j in 1:length(spmiss2)) {
    if(grepl(genus_list[i], spmiss2[j]) == T) {
      spmiss3[j] <- genus_list[i]
    }
  }
}

matchsp3 <- spmiss3 %in% Sp_SAU
table(matchsp3) ## FIRST round: 64 spp no macth, 143 spp macthed (total:207spp) *will be updated with new observations
#FINAL round: 24 species/genus from 70 are now match
#spmiss3 <- unique(spmiss3)#2 repeated species after correcting typos!

#loop to replace species by genus within database
#positions of unmatching cells within column
pos <- which(ReviewDatFB$scientific_name %in% spmiss2)
data <- ReviewDatFB#for verification purposes

for (i in 1:length(genus_list)) {#loop to replace species in "pos" by genus
  for (j in pos) {
    if(grepl(genus_list[i], ReviewDatFB$scientific_name[j]) == T) {
      ReviewDatFB$scientific_name[j] <- genus_list[i]
    }
  }
}

#Verifications
cont <- as.numeric(table(spmiss1 %in% unique(data$scientific_name[pos])))
length(unique(genus_list))
cont2 <- length(unique(spmiss3))
table(unique(ReviewDatFB$scientific_name[pos]) %in% data$scientific_name[pos])#15 unmatch from genus changes
table(unique(ReviewDatFB$scientific_name) %in% Sp_SAU)#46 vs 147 (193)
i <- table(unique(ReviewDatFB$scientific_name) %in% Sp_SAU)#46 vs 147 (193)
c(cont-cont2) == 202-as.numeric((i[1]+i[2])) #(202 is the species number)
#if TRUE, CHANGES ARE OOOOK!!!!
rm(spmiss1, spmiss2, spmiss3, matchsp, matchsp2, matchsp3, i, j, cont, cont2, pos, genus_list, data, Sp_SAU, Sp_ReviewDatFB)

write.csv(ReviewDatFB, row.names = F, "data/biblio_database3.csv")
