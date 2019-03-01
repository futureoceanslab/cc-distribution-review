

dataframe <- data.frame(ID=c(15,15,15,16,16,16,16),
                      Environment=c("PRODUCTION","PRODUCTION", "TRAINING",
                                    "PRODUCTION","PRODUCTION", "TRAINING", "STAGING"),
                      Green=c("Yes","No", "Yes","Yes","No", "No", "Yes"),
                      Red=c("No","Yes", "No","No","Yes", "No", "No"),
                      White=c("No","No", "No","No","No", "No", "No"),
                      Black=c("No","No", "No","No","No", "No", "No"),
                      Completed=c("Yes","Yes", "No","Yes","Yes", "No", "No"))


df <- dataset%>%group_by(ID,Environment)%>% mutate(total = n())#add column total for counter of duplicates

ddc<-df[df$total==1,]#subsets those without duplicates
ddd<-df[df$total==2,]#subsets those with duplicates

ddd<- ddd %>% group_by(ID,Environment) %>% summarise_all(funs(max(as.character(.)))) 

merge(ddc, ddd, all=TRUE)


#first I select the variable sin dataframe we need (simplify dataframe)
df <- ReviewDat %>%  select(id_obs, site, id_study, eez_countries, b_scientific_name, rfishbase_species_code, cc, b_impact, b_value)  %>%
      group_by(eez_countries, b_scientific_name, b_impact) %>% mutate(total = n())


ddc<-df[df$total==1,]#subsets those without duplicates
ddd<-df[df$total==2,]#subsets those with duplicates

ddd<- ddd %>% group_by(eez_countries, b_scientific_name, b_impact) %>% mutate(b_value_x = mean(b_value, na.rm = TRUE))  #mean value of b_value in eez-sp-impact

ddd<- ddd %>% group_by(eez_countries, b_scientific_name, b_value_x) %<%                                                                        
                                                                              
                                                                              
                                                                              
                                                                              id_obs = paste(id_obs, collapse = " "), #all values of id_obs
                                                                              site = paste(site, collapse = " "),     #all values of site
                                                                              id_study = paste(id_study, collapse = " "), #all values of id_study
                                                                              eez_codes = paste(eez_codes, collapse = " "), #all values of eez_codes
                                                                              rfishbase_species_code = paste(rfishbase_species_code, collapse = " "), #all values of fishbase sp codes
                                                                              cc = paste(cc, collapse = " "))  #all values of cc
                                                                              
                                                                              

