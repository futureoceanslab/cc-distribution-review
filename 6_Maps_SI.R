#15/05/2020
#REF
# Flanders Marine Institute (2019). Maritime Boundaries Geodatabase: Maritime Boundaries and Exclusive Economic Zones (200NM), version 11. Available online at https://www.marineregions.org/. https://doi.org/10.14284/386

library(tidyverse)   
library(sf) #spatial functions
library(rgdal) #readOgr

#open dataset
data <- read.csv("data/biblio_database1.csv")

d <- data %>%
      group_by(lat, long) %>%
      summarise(n =n())

#world map
map <- map_data("world")

# create the breaks- and label vectors
ewbrks <- seq(-180,180,60)
nsbrks <- seq(-90,90,30)
ewlbls <- unlist(lapply(ewbrks, function(x) ifelse(x < 0, paste(x, "°E", sep = ""), ifelse(x > 0, paste(x, "°W", sep = ""),x))))
nslbls <- unlist(lapply(nsbrks, function(x) ifelse(x < 0, paste(x, "°S", sep = ""), ifelse(x > 0, paste(x, "°N", sep = ""),x))))

#location of the studies. colour of the points indicate the n of obs
Fig1 <- ggplot() +                
          geom_polygon(data = map, aes(long, lat, group = group), fill = "grey") +
          labs(x = "", y = "") +
          geom_point(aes(x =long , y=lat, color = n), data = d, size = 8) +
          scale_colour_gradientn("Number of\nobservations", 
                                 colours = c("darkgreen", "yellow", "purple")) + 
          scale_x_continuous(limits = c(), expand = c(0, 0),
                             breaks = ewbrks, labels = ewlbls) + #remove space between graph and axis
          scale_y_continuous(limits = c(), expand = c(0, 0),
                             breaks = nsbrks, labels = nslbls) + #remove space between graph and axis
          theme_classic() + # remove the grey background
          theme(axis.text.x = element_text(size = 16),
                axis.text.y = element_text(size = 16),
                legend.title = element_text(size = 18),
                legend.text = element_text(size = 16), 
                plot.title = element_text(size = 20)) +
          ggtitle("a)") 

# colfunc <- colorRampPalette(c("darkgreen", "yellow", "purple"))
# colfunc(max(d$n)) #56 colors

png(file = "paper_figures/figure_1a.png", 
    width = 13, height = 7, units = 'in', res = 600)
Fig1
dev.off()


#Figure2
data2 <- read.csv("data/biblio_database_full2.csv")
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

# plot(eez[1], key.pos = 1) #1 -> only eez contours

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
        plot.title = element_text(size = 20)) +
  ggtitle("a)") 

dev.off()

#https://globalfishingwatch.org/data-blog/working-with-our-downloadable-public-data-in-r/
#https://cran.r-project.org/web/packages/mregions/vignettes/mregions.html
library(mregions)
res1 <- mr_records_by_type(type = "EEZ")
res2 <- mr_shp(type = "EEZ")

#SI Table 1
data$cc_driver_detail <- as.character(data$cc_driver_detail)
data$cc_driver_detail[data$cc_driver_detail == "sst_anomaly"] <- "SST"
data$cc_driver_detail[data$cc_driver_detail == "sst"] <- "SST"
data$cc_driver_detail[data$cc_driver_detail == "sst, air_temperature"] <- "SST, Air T"
data$cc_driver_detail[data$cc_driver_detail == "sst, bottom_temperature"] <- "SST, BT"
data$cc_driver_detail[data$cc_driver_detail == "climate_velocity"] <- "CV"
data$cc_driver_detail[data$cc_driver_detail == "water_temperature"] <- "Water T"
data$cc_driver_detail[data$cc_driver_detail == "bottom_temperature"] <- "BT"

SI <- data %>%
        group_by(reference, year_publication, site, time_span) %>%
        summarise(N = n(),
                  Multiespecies = unique(multi_species),
                  Obs_type = paste(response, collapse = "-"),
                  Fishing_survey = NA,
                  Climate_driver = paste(cc_driver_detail, collapse = "-"))
