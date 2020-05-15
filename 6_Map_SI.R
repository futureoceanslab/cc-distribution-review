#15/05/2020

library(tidyverse)     

#open dataset
data <- read.csv("data/biblio_database1.csv")

d <- data %>%
      group_by(lat, long) %>%
      summarise(n =n())

#world map
map <- map_data("world")

#location of the studies. colour of the points indicate the n of obs
Fig1 <- ggplot() +                
          geom_polygon(data = map, aes(long, lat, group = group), fill = "grey") +
          labs(x = "", y = "") +
          geom_point(aes(x =long , y=lat, color = n), data = d, size = 8) +
          scale_colour_gradientn("Number of\nobservations", 
                                 colours = c("darkgreen", "yellow", "red")) + 
          scale_x_continuous(limits = c(), expand = c(0, 0)) + #remove space between graph and axis
          scale_y_continuous(limits = c(), expand = c(0, 0)) + #remove space between graph and axis
          theme_classic() + # remove the grey background
          theme(axis.text.x = element_text(size = 16),
                axis.text.y = element_text(size = 16),
                legend.title = element_text(size = 18),
                legend.text = element_text(size = 16))

png(file = "paper_figures/figure_1.png", 
    width = 13, height = 7, units = 'in', res = 600)
Fig1
dev.off()


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
