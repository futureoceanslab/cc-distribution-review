##################################################################
##### This script produces figures for the supplementary material.
##### 01/03/2019
##### INPUT FILES: biblio_database_full.csv, biblio_database.xlsx,
##### EEZ.csv
##### OUTPUT FILES: manuscipt SM figures
##################################################################

library(tidyverse)   
library(ggrepel)#geom_text_repel
library(readxl)

#####0. READ DATA ####
#Open Biblio_data with SAU data on FE
data_original <- read.csv("data/biblio_database_full.csv")
length(unique(data_original$scientific_name)) #193, ok!
data_original$ID <- 1:dim(data_original)[1]

#####1. DATA TYDING ####
# REMOVE NAS IN FISHING ENTITY
remove <- filter(data_original, fishing_entity  == "Unknown Fishing Country")
rm_id <- remove$ID
data <- filter(data_original, !ID %in% rm_id)
length(unique(data$scientific_name)) #193, ok!
allsp <- unique(data$scientific_name)

remove2 <- filter(data_original, is.na(fishing_entity) == T)
rm_id <- remove2$ID
data <- filter(data, !ID %in% rm_id)
#species lost!! No record of FEs fishing that species within that EEZs
sp <- unique(data$scientific_name)
lostsp <- allsp[allsp %in% sp == F]
#We end up with the following number of species:
length(unique(data$scientific_name))

rm(allsp, lostsp, rm_id, sp)

#####2. TABLE ####
data$cc_driver_detail <- as.character(data$cc_driver_detail)
data$cc_driver_detail[data$cc_driver_detail == "sst_anomaly"] <- "SST"
data$cc_driver_detail[data$cc_driver_detail == "sst"] <- "SST"
data$cc_driver_detail[data$cc_driver_detail == "sst, air_temperature"] <- "SST, Air T"
data$cc_driver_detail[data$cc_driver_detail == "sst, bottom_temperature"] <- "SST, BT"
data$cc_driver_detail[data$cc_driver_detail == "climate_velocity"] <- "CV"
data$cc_driver_detail[data$cc_driver_detail == "water_temperature"] <- "Water T"
data$cc_driver_detail[data$cc_driver_detail == "bottom_temperature"] <- "BT"

SI <- data %>%
  group_by(id_study) %>%
  summarise(N = n(),
            Obs_type = paste(response, collapse = "-"),
            Species_num = length(unique(scientific_name)))
SI[grep("5", SI$id_study),]#Change number to filter

data2 <- read_excel("data/biblio_database.xlsx", sheet = 1)

SI2 <- data2 %>%
        group_by(id_study, reference) %>%
        summarise(Site = unique(site),
                  Time_span = paste(time_span, collapse = "-"),
                  Response = paste(response, collapse = "-"))

SI3 <- unique(data2$reference)

#####3. FIGURES ####
data$response <- as.factor(data$response)
levels(data$response)

#Changing a few names for plotting
data$area_name[data$area_name == "Canada (East Coast)"] <- "Can-East"
data$area_name[data$area_name == "Korea (South)"] <- "S-Korea"
data$area_name[data$area_name == "United Kingdom (UK)"] <- "UK"
data$area_name[data$area_name == "USA (Alaska, Subarctic)"] <- "Alaska"
data$area_name[data$area_name == "Japan (main islands)"] <- "Japan"
data$area_name[data$area_name == "France (Atlantic Coast)"] <- "France ATL"
data$area_name[data$area_name == "Spain (Northwest)"] <- "Spain NW"

data$col <- NA
unique(data$fishing_entity)

data$col[data$fishing_entity == "USA" ] <- "black"
data$col[data$fishing_entity == "Canada"] <- "black"
data$col[data$fishing_entity == "Greenland"] <- "black"

data$col[data$fishing_entity == "Namibia"] <- "#CC79A7"
data$col[data$fishing_entity == "Angola"] <- "#CC79A7"
data$col[data$fishing_entity == "South Africa"] <- "#CC79A7"

data$col[data$fishing_entity == "Japan"] <- "orange"
data$col[data$fishing_entity == "Korea (South)"] <- "orange"
data$col[data$fishing_entity == "China"] <- "orange"
data$col[data$fishing_entity == "Russian Federation"] <- "orange"

data$col[data$fishing_entity == "Azores Isl"] <- "#56B4E9" #blue
data$col[data$fishing_entity == "Poland"] <- "#56B4E9"
data$col[data$fishing_entity == "Estonia"] <- "#56B4E9"
data$col[data$fishing_entity == "Saint Pierre & Miquelon (France)"] <- "#56B4E9"
data$col[data$fishing_entity == "Portugal"] <- "#56B4E9"
data$col[data$fishing_entity == "Norway"] <- "#56B4E9"
data$col[data$fishing_entity == "Spain"] <- "#56B4E9"
data$col[data$fishing_entity == "Ireland"] <- "#56B4E9"
data$col[data$fishing_entity == "Germany"] <- "#56B4E9"
data$col[data$fishing_entity == "Faeroe Isl"] <- "#56B4E9"
data$col[data$fishing_entity == "Iceland"] <- "#56B4E9"
data$col[data$fishing_entity == "United Kingdom"] <- "#56B4E9"
data$col[data$fishing_entity == "Sweden"] <- "#56B4E9"
data$col[data$fishing_entity == "Netherlands"] <- "#56B4E9"
data$col[data$fishing_entity == "Denmark"] <- "#56B4E9"
data$col[data$fishing_entity == "France"] <- "#56B4E9"
data$col[data$fishing_entity == "Belgium"] <- "#56B4E9"

lat <- subset (data, response == "latitude")
dep <- subset (data, response == "depth")

lat$fishing_entity[lat$fishing_entity == "St Pierre & Miquelon (Fr)"] <- "St P&M"
dep$fishing_entity[dep$fishing_entity == "St Pierre & Miquelon (Fr)"] <- "St P&M"
lat$fishing_entity[lat$fishing_entity == "Korea (South)"] <- "S-Korea"
dep$fishing_entity[dep$fishing_entity == "Korea (South)"] <- "S-Korea"


#LATITUDE
EEZ <- read.csv("data/EEZ.csv")
#fusionar lat y EEZ
unique(lat$fishing_entity)[which(unique(lat$fishing_entity) %in% unique(EEZ$fishing_entity) == F)] 
lat$fishing_entity[grep("S-Korea", lat$fishing_entity)] <- "Korea (South)"
lat$fishing_entity[grep("St", lat$fishing_entity)] <- "France"
unique(lat$fishing_entity)[which(unique(lat$fishing_entity) %in% unique(EEZ$fishing_entity) == F)] 

lat <- left_join(lat, EEZ, by = "fishing_entity")
lat$AC <- "1-5"
lat$AC[lat$total > 5 & lat$total <= 15] <- "6-15"
lat$AC[lat$total > 15 & lat$total <= 30] <- "16-30"
lat$AC[lat$total > 30] <- ">31"
lat$AC <- factor(lat$AC, levels = c("1-5", "6-15", "16-30", ">31"))

data[rowSums(is.na(data)) > 0, ]

lat$decadal_change <- abs(lat$decadal_change)

png(file = "figuras_definitivas/Figure_SI1i.png", 
    width = 10, height = 6, units = 'in', res = 600)

ggplot(lat, aes(decadal_change, catchdepFE*100,# 
                colour = col, shape = AC,
                label = paste(scientific_name, "\n", fishing_entity, "in", area_name, sep =" "))) + #
  scale_colour_identity() +
  scale_x_continuous(trans = 'sqrt') +
  scale_y_continuous(trans = 'sqrt',
                     labels = scales::number_format(accuracy = 0.1,
                                                    decimal.mark = ',')) +
  geom_point(aes(alpha = 0.7), size = 3) +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.key = element_rect(fill = NA),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 14),
        legend.title =element_text(size = 14)) +
  geom_text_repel(data = subset(lat, catchdepFE*100 > 10),
                  aes(color = col), size = 3) + #
  guides(shape = guide_legend(title = "Adaptive capacity\n(# of EEZs)"),
         alpha = F) +
  labs(y = "Sensitivity\n(Catch dependency %)", #Catch 
       x = "Exposure\n(Latitudinal shift)") 
dev.off()


#DEPTH
dep <- left_join(dep, EEZ, by = "fishing_entity")
dep$AC <- "1-5"
dep$AC[dep$total > 5 & dep$total <= 15] <- "6-15"
dep$AC[dep$total > 15 & dep$total <= 30] <- "16-30"
dep$AC[dep$total > 30] <- ">31"
dep$AC <- factor(dep$AC, levels = c("1-5", "6-15", "16-30", ">31"))

dep$decadal_change <- abs(dep$decadal_change)

png(file = "figuras_definitivas/Figure_SI2i.png", 
    width = 10, height = 6, units = 'in', res = 600)

ggplot(dep, aes(decadal_change, catchdepFE*100,  
                colour = col, shape = AC,
                label = paste(scientific_name, "\n", fishing_entity, "in", area_name, sep =" "))) + #
  scale_colour_identity() +
  scale_x_continuous(trans = 'sqrt') +
  scale_y_continuous(trans = 'sqrt',
                     labels = scales::number_format(accuracy = 0.1,
                                                    decimal.mark = ',')) +
  geom_point(aes(alpha = 0.7), size = 3) +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.key = element_rect(fill = NA),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 14),
        legend.title =element_text(size = 14)) +
  geom_text_repel(data = subset(dep,catchdepFE*100 > 15),
                  aes(color = col), size = 3) +
  guides(shape = guide_legend(title = "Adaptive capacity\n(# of EEZs)"),
         alpha = F) +
  labs(y = "Sensitivity\n(Catch dependency %)", #Catch 
       x = "Exposure\n(Depth shift)") 
dev.off()


#SI3
data$fishing_entity <- as.character(data$fishing_entity)
data$area_name <- as.character(data$area_name)
data$fishing_entity[data$fishing_entity == "Saint Pierre & Miquelon (France)"] <- "St Pierre & Miquelon (Fr)"

d_FE <- data %>% ###our data by FE
  group_by(fishing_entity, area_name, scientific_name) %>%
  summarise(tonnesFEsp = unique(tonnesFEsp),
            landedvalueFEsp = unique(landedvalueFEsp),
            tonnesFE = unique(tonnesFE),
            landedvalueFE = unique(landedvalueFE)) %>%
  group_by(fishing_entity) %>%
  summarise(vulnerable_catch = sum(tonnesFEsp),
            vulnerable_value = sum(landedvalueFEsp),
            tonnesFE = unique(tonnesFE),
            landedvalueFE = unique(landedvalueFE),
            landedvalueFE = unique(landedvalueFE),
            vulnerable_value_prop = round((vulnerable_value/landedvalueFE)*100,2),
            non_vulnerable_value_prop = 100 - vulnerable_value_prop,
            non_vulnerable_value = landedvalueFE - vulnerable_value)

#Changing a few names for plotting
data$fishing_entity <- as.character(data$fishing_entity)
data$area_name <- as.character(data$area_name)
data$fishing_entity[data$fishing_entity == "Saint Pierre & Miquelon (France)"] <- "St Pierre & Miquelon (Fr)"

pl <- d_FE[c("fishing_entity", "vulnerable_value_prop", "non_vulnerable_value_prop")]
pl <- gather(pl, key = "var", value = "measure",
             vulnerable_value_prop, non_vulnerable_value_prop)
pl$fishing_entity <- as.factor(pl$fishing_entity)

names <- pl %>%
  arrange(measure)
names <- names["fishing_entity"]
names <- names[1:27,]
names <- names[-15,]
names[27,] <- "Namibia"
names2 <- rev(names[["fishing_entity"]])

pl2 <- d_FE[c("fishing_entity", "vulnerable_value", "non_vulnerable_value")]
pl2 <- gather(pl2, key = "var", value = "measure",
              vulnerable_value, non_vulnerable_value)
pl2$fishing_entity <- factor(pl$fishing_entity, levels = names2)


png(file = "figuras_definitivas/Figure_SI3.png", 
    width = 10, height = 7, units = 'in', res = 600)

pl %>%
  mutate(fishing_entity = fct_relevel(fishing_entity,
                                      paste(names2))) %>%
  ggplot(aes(fishing_entity, measure)) +
  geom_bar(aes(fill = var), stat = "identity") +
  scale_fill_manual(values = c("grey","#045a8d"),
                    labels = c("Unknown", "Impacted"),
                    guide = guide_legend(reverse = T),
                    name = "Landed value status:") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16, colour = "black"), 
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 18),
        #legend.title = element_blank(),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 20),
        legend.title = element_text(size = 16)) +
  labs(x = "Fishing country", 
       y = "Landed value (%)")
dev.off()