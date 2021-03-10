##################################################################
##### This script produces socioeconomic fugures for the manuscript.
##### 01/03/2019
##### INPUT FILES: biblio_database_full.csv
##### OUTPUT FILES: manuscipt figures XXXXXXXXXXXX
##################################################################

library(RColorBrewer)#brewer.pal
library(tidyverse)
library(ggrepel)#geom_text_repel
library(stringr)
library(readxl)
library(sf) #spatial functions

#Figure2
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

rm(remove, remove2, allsp, lostsp, rm_id, sp)

data2 <- data

d2 <- data2 %>%
  group_by(area_name, scientific_name) %>%
  summarise(n = n()) %>%
  group_by(area_name) %>%
  summarise(n = sum(n, na.rm = T))

# Load EEZ polygons, downloaded from
#https://mazu.nceas.ucsb.edu/data/#ohi_regions
#https://www.jamiecmontgomery.com/post/cropping-rasters-down-to-size/
regions <- st_read("data/regions/regions_gcs.shp", quiet = TRUE)
#regions$geometry
regions <- regions %>% 
  dplyr::filter(rgn_typ == "eez" & regions$ant_typ == "eez")
#plot(regions[3])
nam <- unique(regions$rgn_nam)
nam_biblio <- unique(data2$area_name)

nam_biblio[nam_biblio %in% nam == F]
nam_biblio[nam_biblio %in% nam == T]

nam[grep("Korea", nam)] 
d2$eez <- c("Angola", "Portugal", "Belgium", "Canada", "Denmark", "France", 
            "Germany", "Ireland", "Japan", "South Korea", "Namibia", "Netherlands",
            "Norway", "Portugal", "South Africa", "Spain", "Spain", "Sweden", 
            "United Kingdom", "United States", "United States", "United States", "United States")

unique(d2$eez) %in% nam
d3 <- d2 %>%
  group_by(eez) %>%
  summarise(n = sum(n))
# d4 <- d3
# colnames(d4)[1] <- "rgn_nam"
# angola <- regions %>% 
#             dplyr::filter(rgn_nam == "Angola")
# angola <- left_join(angola, d4, by = "rgn_nam")
# eez <- regions %>% 
#         dplyr::filter(rgn_nam %in% unique(d3$eez) & rgn_typ == "eez")#remove land observations
colnames(d3)[1] <- "rgn_nam"
# eez <- left_join(eez, d3, by = "rgn_nam") #add species number
eez <- left_join(regions, d3, by = "rgn_nam") #add species number

#world map
map <- map_data("world")

png(file = "paper_figures/figure_2a.png", 
    width = 13, height = 7, units = 'in', res = 600)

ggplot(eez[1:239,]) +
  geom_sf(aes(fill = n)) +
  scale_fill_gradientn("Number of\nshifting species", 
                       colours = c("darkgreen", "yellow", "purple"), na.value = "white") +
  geom_polygon(data = map, aes(long, lat, group = group), fill = "grey") +
  labs(x = "", y = "") +
  scale_x_continuous(limits = c(), expand = c(0, 0)) + #remove space between graph and axis
  scale_y_continuous(limits = c(), expand = c(0, 0)) + #remove space between graph and axis
  theme_classic() + # remove the grey background
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 20)) 

dev.off()

####Figure2 #####OLD FIGURE 3 ####
#Changing a few names for plotting
data$fishing_entity <- as.character(data$fishing_entity)
data$area_name <- as.character(data$area_name)
data$fishing_entity[data$fishing_entity == "Saint Pierre & Miquelon (France)"] <- "St Pierre & Miquelon (Fr)"

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

#Color palette
myP <- rev(brewer.pal(n = 8, name = 'RdBu'))

#LATITUDE
png(file = "paper_figures/figure_3a.png", 
    width = 10, height = 6, units = 'in', res = 600)

ggplot(lat, aes(catchdepFE*100, decadal_change, 
                colour = col, size = (landedvalueFEsp/landedvalueFE)*100,
                label = paste(scientific_name, "\n", fishing_entity, "in", area_name, sep =" "))) + #paste(scientific_name, ", ", area_name, sep ="" +
  scale_colour_identity() +
  geom_point(aes(alpha = 0.7)) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0.06, size = 13),
        axis.text.y = element_text(size = 13),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.key = element_rect(fill = NA),
        plot.title = element_text(size = 20)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_text_repel(data = subset(lat, catchdepFE*100 > 6),
                  aes(color = col), size = 3,
                  vjust = -2.5, hjust = "inward") +
  geom_text_repel(data = subset(lat, decadal_change > 500),
                  aes(color = col), size = 3,
                  vjust = -2.5, hjust = "inward") +
  guides(size = guide_legend(title = "Economic dependency\non species (%)"),
         alpha = F) +
  labs(x = "Catch dependency on species (%)",
       y = "Latitude shift (km/decade)") +
  ggtitle("a)") 
dev.off()


#DEPTH
png(file = "paper_figures/figure_3b.png", 
    width = 10, height = 6, units = 'in', res = 600)

ggplot(dep, aes(catchdepFE*100, decadal_change, 
                colour = col, size = (landedvalueFEsp/landedvalueFE)*100,
                label = paste(scientific_name, "\n", fishing_entity, "in", area_name, sep =" "))) + #paste(scientific_name, ", ", area_name, sep ="" +
  scale_colour_identity() +
  geom_point(aes(alpha = 0.7)) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0.06, size = 13),
        axis.text.y = element_text(size = 13),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.key = element_rect(fill = NA),
        plot.title = element_text(size = 20)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_text_repel(data = subset(dep, catchdepFE*100 > 6),
                  aes(color = col), size = 3,
                  vjust = -2.5, hjust = "inward") +
  geom_text_repel(data = subset(dep, decadal_change > 40),
                  aes(color = col), size = 3,
                  vjust = -2.5, hjust = "inward") +
  geom_text_repel(data = subset(dep, decadal_change < -40),
                  aes(color = col), size = 3,
                  vjust = -2.5, hjust = "inward") +
  guides(size = guide_legend(title = "Economic dependency\non species (%)"),
         alpha = F) +
  labs(x = "Catch dependency on species (%)",
       y = "Depth shift (m/decade)") +
  ggtitle("b)") 
dev.off()

#Map figure2
df2 <- data.frame(region = c("USA", "Canada", "Greenland", "Namibia", "Angola", "South Africa",
                             "Japan", "South Korea", "China", "Russia", "Poland",
                             "Estonia", "Portugal", "Azores", "Norway", "Spain", "Ireland",
                             "Germany", "Faroe Islands", "Iceland", "UK", "Sweden",
                             "Netherlands", "Denmark", "France", "Belgium", "Saint Pierre and Miquelon"), 
                  col = c(rep("black",3), rep("#CC79A7",3), rep("orange", 4), rep("#56B4E9", 17)),
                  stringsAsFactors = FALSE)

png(file = "paper_figures/new_map.png", 
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
  xlab("") + ylab("") 
dev.off()