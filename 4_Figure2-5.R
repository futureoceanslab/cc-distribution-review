##################################################################
##### This script produces socioeconomic figures for the manuscript.
##### 01/03/2019
##### INPUT FILES: biblio_database_full.csv, dall_FE_area.csv,
##### biblio_database1.csv
##### OUTPUT FILES: manuscipt figures 2 to 5 and EEZ.csv
##################################################################

library(tidyverse)
library(ggrepel)#geom_text_repel
library(sf) #spatial functions

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

#####2. FIGURE2 ####
d2 <- data %>%
      group_by(area_name, scientific_name, response) %>%
      summarise(n = n()) 

d2 <- d2 %>%
      group_by(response) %>%
      count(area_name)

# Load EEZ polygons, downloaded from
#https://mazu.nceas.ucsb.edu/data/#ohi_regions
#https://www.jamiecmontgomery.com/post/cropping-rasters-down-to-size/
regions <- st_read("data/regions/regions_gcs.shp", quiet = TRUE)
regions <- regions %>% 
  dplyr::filter(rgn_typ == "eez" & regions$ant_typ == "eez")
nam <- unique(regions$rgn_nam)
nam_biblio <- unique(data$area_name)

nam_biblio[nam_biblio %in% nam == F]
nam_biblio[nam_biblio %in% nam == T]

nam[grep("Korea", nam)] 
d2$eez <- d2$area_name
d2$eez[grep("Spain", d2$area_name)] <- "Spain"
d2$eez[grep("Canada", d2$area_name)] <- "Canada"
d2$eez[grep("Germany", d2$area_name)] <- "Germany"
d2$eez[grep("Mexico", d2$area_name)] <- "Mexico"
d2$eez[grep("Portugal", d2$area_name)] <- "Portugal"
d2$eez[grep("Japan", d2$area_name)] <- "Japan"
d2$eez[grep("Chile", d2$area_name)] <- "Chile"
d2$eez[grep("UK", d2$area_name)] <- "United Kingdom"
d2$eez[grep("Denmark", d2$area_name)] <- "Denmark"
d2$eez[grep("Sweden", d2$area_name)] <- "Sweden"
d2$eez[grep("France", d2$area_name)] <- "France"
d2$eez[grep("South Africa", d2$area_name)] <- "South Africa"
d2$eez[grep("USA", d2$area_name)] <- "United States"
d2$eez[grep("Korea", d2$area_name)] <- "South Korea"

unique(d2$eez) %in% nam
d3 <- d2 %>%
  group_by(eez, response) %>%
  summarise(n = sum(n))

colnames(d3)[1] <- "rgn_nam"
d3_lat <- filter(d3, response == "latitude")
d3_dep <- filter(d3, response == "depth")
# eez <- left_join(eez, d3, by = "rgn_nam") #add species number
eez_lat <- left_join(regions, d3_lat, by = "rgn_nam") #add species number
eez_dep <- left_join(regions, d3_dep, by = "rgn_nam") #add species number

#world map
map <- map_data("world")

cooor1 <- read.csv("data/biblio_database1.csv")

#Delete IDs, removed observations (Unknown & NAs FE)
r <- unique(c(remove$id_obs, remove2$id_obs))
r2 <- as.numeric(unlist(str_split(r, "-")))
coor1 <- filter(cooor1, !id_obs %in% r2)

coor_lat <- coor1 %>%
  filter(response == "latitude") %>%
  group_by(lat, long) %>%
  summarise(n = n())

coor_dep <- coor1 %>%
  filter(response == "depth") %>%
  group_by(lat, long) %>%
  summarise(n = n())

coor1_lat <- filter(coor1, response == "latitude")
countries_lat <- unique(coor1_lat$stock_data_country)
countries_lat <- unlist(strsplit(countries_lat, ","))
countries_lat <- unique(gsub(" ", "", countries_lat))
countries_lat[countries_lat == "NewZeland" ] <- "New Zealand"
countries_lat[countries_lat == "SouthAfrica" ] <- "South Africa"
countries_lat[countries_lat == "UnitedKingdom" ] <- "UK"
countries_lat[countries_lat == "Korea(South)" ] <- "South Korea"
countries_lat %in% unique(map$region)

Country_lat <- filter(map, region %in% countries_lat)

coor1_dep <- filter(coor1, response == "depth")
countries_dep <- unique(coor1_dep$stock_data_country)
countries_dep <- unlist(strsplit(countries_dep, ","))
countries_dep <- unique(gsub(" ", "", countries_dep))
countries_dep[countries_dep == "SouthAfrica" ] <- "South Africa"
countries_dep %in% unique(map$region)

Country_dep <- filter(map, region %in% countries_dep)


png(file = "figuras_definitivas/Figure_2ilat.png", 
    width = 13, height = 7, units = 'in', res = 600)

ggplot(eez_lat) +
  geom_sf(aes(fill = n)) +
  scale_fill_gradientn("Number of shifting species", 
                       colours = c("darkgreen", "yellow", "purple"), na.value = "white") + #"lightsteelblue1", "lightskyblue", "#045a8d"
  geom_polygon(data = map, aes(long, lat, group = group), fill = "grey") +
  labs(x = "", y = "") +
  scale_x_continuous(limits = c(), expand = c(0, 0),
                     breaks = c(-180, 180),
                     labels = c("180ºO", "180ºE")) + #remove space between graph and axis
  scale_y_continuous(limits = c(), expand = c(0, 0)) + #remove space between graph and axis
  scale_color_manual(labels = "Study reference point\n Species data origin", values = "black") + #
  geom_polygon(data = Country_lat, aes(long, lat, group = group), fill = "sandybrown", alpha = 0.5) +
  geom_point(data = coor_lat, aes(x =long , y=lat, color = "black"), size = 2) +
  guides(color = guide_legend(title = " ")) +
  theme_classic() + # remove the grey background
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 20)) 

dev.off()

png(file = "figuras_definitivas/Figure_2idep.png", 
    width = 13, height = 7, units = 'in', res = 600)

ggplot(eez_dep) +
  geom_sf(aes(fill = n)) +
  scale_fill_gradientn("Number of shifting species", 
                       colours = c("darkgreen", "yellow", "purple"), na.value = "white") + #"lightsteelblue1", "lightskyblue", "#045a8d"
  geom_polygon(data = map, aes(long, lat, group = group), fill = "grey") +
  labs(x = "", y = "") +
  scale_x_continuous(limits = c(), expand = c(0, 0),
                     breaks = c(-180, 180),
                     labels = c("180ºO", "180ºE")) + #remove space between graph and axis
  scale_y_continuous(limits = c(), expand = c(0, 0)) + #remove space between graph and axis
  scale_color_manual(labels = "Study reference point\n Species data origin", values = "black") + #
  geom_polygon(data = Country_dep, aes(long, lat, group = group), fill = "sandybrown", alpha = 0.5) +
  geom_point(data = coor_dep, aes(x =long , y=lat, color = "black"), size = 2) +
  guides(color = guide_legend(title = " ")) +
  theme_classic() + # remove the grey background
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 20)) 

dev.off()

png(file = "figuras_definitivas/Figure_2ilatbox.png", 
    width = 5, height = 5, units = 'in', res = 600)

coor1_lat$response <- "Latitude"
ggplot(coor1_lat, aes(response, abs(decadal_change))) + 
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16)) +
  xlab("") +
  ylab("Distribution shift (km per decade)") 

dev.off()

png(file = "figuras_definitivas/Figure_2idepbox.png", 
    width = 5, height = 5, units = 'in', res = 600)

coor1_dep$response <- "Depth"
ggplot(coor1_dep, aes(response, abs(decadal_change))) + 
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16))+
  xlab("") +
  ylab("Distribution shift (m per decade)")

dev.off()

#####3. FIGURE 3 ####
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
            vulnerable_catch_prop = round((vulnerable_catch/tonnesFE)*100,2),
            non_vulnerable_catch_prop = 100 - vulnerable_catch_prop,
            non_vulnerable_catch = tonnesFE - vulnerable_catch)

#Changing a few names for plotting
data$fishing_entity <- as.character(data$fishing_entity)
data$area_name <- as.character(data$area_name)
data$fishing_entity[data$fishing_entity == "Saint Pierre & Miquelon (France)"] <- "St Pierre & Miquelon (Fr)"

pl <- d_FE[c("fishing_entity", "vulnerable_catch_prop", "non_vulnerable_catch_prop")]
pl <- gather(pl, key = "var", value = "measure",
             vulnerable_catch_prop, non_vulnerable_catch_prop)
pl$fishing_entity <- as.factor(pl$fishing_entity)

names <- pl %>%
  arrange(measure)
names <- names["fishing_entity"]
names <- names[1:27,]
names <- names[-16,]
names[27,] <- "Namibia"
names2 <- rev(names[["fishing_entity"]])

pl2 <- d_FE[c("fishing_entity", "vulnerable_catch", "non_vulnerable_catch")]
pl2 <- gather(pl2, key = "var", value = "measure",
              vulnerable_catch, non_vulnerable_catch)
pl2$fishing_entity <- as.factor(pl$fishing_entity)


png(file = "figuras_definitivas/Figure_3.png", 
    width = 10, height = 7, units = 'in', res = 600)

pl %>%
  mutate(fishing_entity = fct_relevel(fishing_entity, 
                                      paste(names2))) %>%
  ggplot(aes(fishing_entity, measure)) +
  geom_bar(aes(fill = var), stat = "identity") +
  scale_fill_manual(values = c("grey","#045a8d"),
                    labels = c("Unknown", "Impacted"),
                    guide = guide_legend(reverse = T),
                    name = "Catch status:") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16, colour = "black"), 
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 18),
        #legend.title = element_blank(),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 20),
        legend.title = element_text(size = 16)) +
  labs(x = "Fishing country", 
       y = "Catch (%)")

dev.off()

rm(pl, pl2)

#####4. AC ####
dall_FE_area <- read.csv("data/dall_FE_area.csv")
dall_FE_area <- dall_FE_area %>%
  group_by(fishing_entity, area_name) %>%
  summarise(tonnesFEEZ = sum(tonnesFEsp, na.rm = T),
            landed_valueFEEZ = sum(landedvalueFEsp, na.rm = T))

unique(data$fishing_entity)[which(unique(data$fishing_entity) %in% unique(dall_FE_area$fishing_entity) == F)] 
dall_FE_area$fishing_entity[grep("Pierre", dall_FE_area$fishing_entity)] <- "St Pierre & Miquelon (Fr)"

dall_FE_area <- filter(dall_FE_area, fishing_entity %in% unique(data$fishing_entity))#all our fishing entities


d_area <- data %>% #our data by FE and EEZ
  group_by(fishing_entity, area_name, scientific_name) %>%
  summarise(tonnesFEsp = unique(tonnesFEsp),
            landedvalueFEsp = unique(landedvalueFEsp)) %>%
  group_by(fishing_entity, area_name) %>% 
  summarise(vulnerable_catch = sum(tonnesFEsp, na.rm = T),
            vulnerable_value = sum(landedvalueFEsp, na.rm = T))
d_plot <- left_join(dall_FE_area, d_area, by = c("fishing_entity", "area_name"))
d_plot[is.na(d_plot)] <- 0
d_plot$na_catch <- d_plot$tonnesFEEZ - d_plot$vulnerable_catch
d_plot$na_value <- d_plot$landed_valueFEEZ - d_plot$vulnerable_value

d_FE_subset <- d_FE[c ("fishing_entity", "tonnesFE", "landedvalueFE")] 
d_plot5 <- left_join(d_plot, d_FE_subset, by = "fishing_entity")

#Same names in "fishing_entity" and "area_name"
d_plot5$area_name_simpl <- d_plot5$area_name
unique(d_plot5$fishing_entity)[unique(d_plot5$fishing_entity) %in% unique(d_plot5$area_name_simpl) == F]
d_plot5$area_name_simpl[grep("Faeroe", d_plot5$area_name_simpl)]#Faroe
d_plot5$area_name_simpl[grep("Green", d_plot5$area_name_simpl)]#Greenland
d_plot5$area_name_simpl[grep("Russia", d_plot5$area_name_simpl)]#Russia
d_plot5$area_name_simpl[grep("France", d_plot5$area_name_simpl)]#
d_plot5$area_name_simpl[grep("USA", d_plot5$area_name_simpl)] <- "USA"
d_plot5$area_name_simpl[d_plot5$area_name_simpl == "Greenland (Denmark)"] <- "Greenland"
d_plot5$area_name_simpl[d_plot5$area_name_simpl == "Faeroe Isl. (Denmark)"] <- "Faeroe Isl"
d_plot5$area_name_simpl[grep("Denmark", d_plot5$area_name_simpl)] <- "Denmark"
d_plot5$area_name_simpl[grep("Germany", d_plot5$area_name_simpl)] <- "Germany"
d_plot5$area_name_simpl[grep("Sweden", d_plot5$area_name_simpl)] <- "Sweden"
d_plot5$area_name_simpl[grep("UK", d_plot5$area_name_simpl)] <- "United Kingdom"
d_plot5$area_name_simpl[grep("Japan", d_plot5$area_name_simpl)] <- "Japan"
d_plot5$area_name_simpl[grep("Canada", d_plot5$area_name_simpl)] <- "Canada"
d_plot5$area_name_simpl[grep("Spain", d_plot5$area_name_simpl)] <- "Spain"
d_plot5$area_name_simpl[d_plot5$area_name_simpl == "Saint Pierre & Miquelon (France)"] <- "St Pierre & Miquelon (Fr)"
d_plot5$fishing_entity[d_plot5$fishing_entity == "Saint Pierre & Miquelon (France)"] <- "St Pierre & Miquelon (Fr)"
d_plot5$area_name_simpl[grep("France", d_plot5$area_name_simpl)] <- "France"
d_plot5$area_name_simpl[grep("Africa", d_plot5$area_name_simpl)] <- "South Africa"
d_plot5$area_name_simpl[d_plot5$area_name_simpl == "Azores Isl. (Portugal)"] <- "Azores Isl"
d_plot5$area_name_simpl[grep("Portugal", d_plot5$area_name_simpl)] <- "Portugal"
d_plot5$area_name_simpl[grep("Russia", d_plot5$area_name_simpl)] <- "Russian Federation"
unique(d_plot5$fishing_entity) %in% unique(d_plot5$area_name_simpl)
unique(d_plot5$fishing_entity)[unique(d_plot5$fishing_entity) %in% unique(d_plot5$area_name_simpl) == F]


#add own/other EEZ var
d_plot5$in_out <- "OtherEEZ"
d_plot5 <- as.data.frame(d_plot5)
for (i in 1:dim(d_plot5)[1]) {
  if(identical(d_plot5[i,colnames(d_plot5) == "fishing_entity"], d_plot5[i,colnames(d_plot5) == "area_name_simpl"])==T) {
    d_plot5$in_out[i] <- "OwnEEZ"
  }
}

d_plot5b <- d_plot5 %>%
  group_by(fishing_entity, in_out) %>%
  summarise(eez_number = n(),
            vulnerable_catch = sum(vulnerable_catch, na.rm = T),
            na_catch = sum(na_catch, na.rm = T))

d_plot5b <- left_join(d_plot5b, d_FE_subset, by = "fishing_entity") 

d_EEZ <- d_plot5b[,1:3] %>% 
  spread(key = "in_out",
         value = "eez_number")

d_EEZ$total <- NA
d_EEZ$OtherEEZ[is.na(d_EEZ$OtherEEZ) == T] <- 0
d_EEZ$total <- d_EEZ$OwnEEZ + d_EEZ$OtherEEZ

write.csv(d_EEZ, "data/EEZ.csv", row.names = F)

#####5. FIGURE 4 ####
# Subsets of the impacts depending on the response type lat or depth
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
data$col[data$fishing_entity == "St Pierre & Miquelon (Fr)"] <- "#56B4E9"
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
EEZ <- d_EEZ
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

png(file = "figuras_definitivas/Figure_4ai.png", 
    width = 10, height = 6, units = 'in', res = 600)

lat$decadal_change <- abs(lat$decadal_change)

ggplot(lat, aes(decadal_change, (landedvalueFEsp/landedvalueFE)*100, # catchdepFE*100,
                colour = col, shape = AC,#size = (landedvalueFEsp/landedvalueFE)*100,
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
  geom_text_repel(data = subset(lat, (landedvalueFEsp/landedvalueFE)*100 > 11),#catchdepFE
                  aes(color = col), size = 3) + #
  geom_text_repel(data = subset(lat, decadal_change == 266.51 & fishing_entity == "Angola"),
                  aes(color = col), size = 3) +
  guides(shape = guide_legend(title = "Adaptive capacity\n(# of EEZs)"),
         alpha = F) +
  labs(y = "Sensitivity\n(Economic dependency %)", #Catch 
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


png(file = "figuras_definitivas/Figure_4bi.png", 
    width = 10, height = 6, units = 'in', res = 600)

ggplot(dep, aes(decadal_change, (landedvalueFEsp/landedvalueFE)*100, # catchdepFE*100, 
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
  geom_text_repel(data = subset(dep,(landedvalueFEsp/landedvalueFE)*100 > 15),
                  aes(color = col), size = 3) +
  guides(shape = guide_legend(title = "Adaptive capacity\n(# of EEZs)"),
         alpha = F) +
  labs(y = "Sensitivity\n(Economic dependency %)", #Catch 
       x = "Exposure\n(Depth shift)") 
dev.off()

#Map figure4
df2 <- data.frame(region = c("USA", "Canada", "Greenland", "Namibia", "Angola", "South Africa",
                             "Japan", "South Korea", "China", "Russia", "Poland",
                             "Estonia", "Portugal", "Azores", "Norway", "Spain", "Ireland",
                             "Germany", "Faroe Islands", "Iceland", "UK", "Sweden",
                             "Netherlands", "Denmark", "France", "Belgium", "Saint Pierre and Miquelon"), 
                  col = c(rep("black",3), rep("#CC79A7",3), rep("orange", 4), rep("#56B4E9", 17)),
                  stringsAsFactors = FALSE)

png(file = "figuras_definitivas/Figure_4_map.png", 
    width = 13, height = 7, units = 'in', res = 600)
ggplot() + 
  geom_map(data = map, 
           map = map,
           aes(x = long,
               y = lat,
               group = group, 
               map_id = region),
           fill = "white",
           colour = "#7f7f7f") + 
  geom_map(data = df2, 
           map = map,
           aes(fill = col, 
               map_id = region),
           colour = "#7f7f7f") +
  scale_fill_identity() +
  theme_void() + # remove the grey background
  theme(plot.title = element_text(size = 20),
        legend.position = "none") +
  xlab("") + ylab("") +
  coord_quickmap(xlim = c(-180, 180),  #-20, 20
                ylim = c(-50, 90))
dev.off()

#####6. FIGURE 5 ####
fig5 <- data %>%
  group_by(fishing_entity) %>%
  summarise(impactEEZ = length(unique(area_name)))
fig5 <- left_join(EEZ, fig5, by = "fishing_entity")
fig5$fishing_entity <- as.factor(fig5$fishing_entity)

fig5b <- fig5[,-c(2,3)]

fig5b <- gather(fig5b,"EEZ", "value", -fishing_entity)
fig5b$EEZ <- factor(fig5b$EEZ, levels = c("total", "impactEEZ"))


png(file = "figuras_definitivas/Figure_5.png",
    width = 10, height = 7, units = 'in', res = 600)

ggplot(fig5b, aes(reorder(fishing_entity, desc(value), sum, na.rm = T), value, fill = EEZ)) +
        geom_bar(stat="identity") +
        scale_fill_manual(values = c("grey", "#045a8d"),
                          labels = c("Unknown", "Impacted"),
                          name = "EEZ status:") +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16, colour = "black"),
              axis.text.y = element_text(size = 16),
              axis.title = element_text(size = 18),
              legend.text = element_text(size = 16),
              legend.title = element_text(size = 16)) +
        labs(x = "Fishing Entity", y = "# of EEZs", 
              col = "Total number \nof EEZs")
dev.off()