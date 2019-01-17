#Creation of the Fishing Entity Catches dataset: Final_SAU_FE.csv
#source Fishing entity list: Biblio_database
#source catch data: Sea Around Us
#Elena Ojea, december 17


#to select the fishing entities (download the from SAU online) I use the list_FE.csv, obtained from Data_Merge_SAu.csv 
##then, I manually  download each Fishing entity country from SAU webpage and save them in data/SAU-

#the downloaded files are merged in one database with this code:

path <- "data/data_FE_SAU/"
l <- list.files(path, pattern = ".csv")
#l <- l0[-c(7, 24)] # to delete certain files. E.g. column 7(France) and 24

# below func does importing and creation of new columns
func <- function(i){
  df <- read.csv(paste0(path,l[i]))
  names <- unlist(strsplit(l[i], ".", fixed=TRUE))
  df$fishing_entity <- names[1]
  return(df)
}

# l1 shall have each of the dataframes individually with new columns attached
l1 <- lapply(1:length(l), func)
# here we combine all dataframes together
Final_SAU_FE <- do.call(rbind.data.frame, l1) #combine the datasets on fishing entities total catch

write.csv(Final_SAU_FE , file = "data/Final_SAU_FE.csv", row.names = F)
