

##Review Paper
##May 1st
##Elena ojea
##work on socioeconomic figures
#https://www.r-bloggers.com/revisiting-world-bank-data-analysis-with-wdi-and-gvismotionchart/

##calling libraries
library(stats)
library(base)
library(RColorBrewer)
library(tidyverse)
library(gridExtra)
library(data.table)
library(grid)
library(ggrepel)
#library(xlsx)
library(readxl)
library(dplyr)
library(ggplot2)
source("function_multiplot.R")

#Open Biblio_data with SAU data on EEZ and FE:
data <- read_xlsx("data/biblio_database_full.xlsx")


data$area_name[data$area_name == "Spain (mainland, Med and Gulf of Cadiz)"] <- "Spain (Med)"
data$area_name[data$area_name == "South Africa (Atlantic and Cape)" ] <- "South Africa (Atl)"


###### DATA TYDING ##################################################################################################

# 1. REMOVE NAS IN FISHING ENTITY. 
##species with no catches in the last 5 years OR species not in SAU data at the species taxa level

data <- na.omit(data, cols="fishing_entity")
data<-data[!(data$fishing_entity=="Unknown Fishing Country"),]

# 2. REMOVE OUTLIERS in impact. remove decadal_change>300, one observation.
range(data$decadal_change)
plot(data$decadal_change, data$id_obs) #to check for outliers
data <- subset(data, data$decadal_change<300)



################Pruebas con relative indicators to compare counties (fishing entities)

library(WDI)
library(ggplot2)
library(googleVis)
library(plyr)
library(dplyr)


##correct the fishing_entity names
data$fishing_entity[data$fishing_entity == "Azores Isl"] <- "Portugal"
data$fishing_entity[data$fishing_entity == "Saint Pierre & Miquelon (France)"] <- "France"
data$fishing_entity[data$fishing_entity == "Faeroe Isl"] <-"Faeroe Islands" 
data$fishing_entity[data$fishing_entity == "USA"] <- "United States" 
data$fishing_entity[data$fishing_entity == "Korea (South)"] <- "Korea, Rep."   
countries_data <- unique(data$fishing_entity)

##### 1. World population datapercountry

#download population per country in2017
population = WDI(indicator='SP.POP.TOTL', country="all",start=2017, end=2017)

#filter population data for my countries (fishing_entity)
data$fishing_entity %in% population$country
countries_data <- unique(data$fishing_entity)
population_data <- subset(population, country %in% countries_data)
colnames(population_data) <- c("iso2c", "fishing_entity", "population", "year")
population_data <- population_data[, c(2,3)]

data <- merge(data,population_data, by="fishing_entity")



###### 2.  GDP per country (in US $)
gdp= WDI(indicator='NY.GDP.MKTP.CD', country="all",start=2017, end=2017)

gdp_greenland = WDI(indicator='NY.GDP.MKTP.CD', country="Greenland" ,start=2016, end=2016)*****

data$fishing_entity %in% gdp$country  #na for greenland

gdp_data <- subset(gdp, country %in% countries_data)

colnames(gdp_data) <- c("iso2c", "fishing_entity", "gdp", "year")
gdp_data <- gdp_data[, c(2,3)]

data <- merge(data,gdp_data, by="fishing_entity")


## 3.  Relative indicators calculation

#Per capita sp catches: catches by FE in EEZ of impacted species/population in country
data$catchpercapita <- (data$tonnesFEsp)/(data$population)
range(data$catchpercapita)

#Relative landed value per GDP
data$landedperGDP <- (data$landedvalueFEsp)/(data$gdp)
range(data$landedperGDP)




# 4. IMPACT TYPES. Subsets of the impacts:
data$response <- as.factor(data$response)
levels(data$response)

latitude <- subset (data, response == "latitude")
depth <- subset (data, response == "depth")


##prueba grafico percapita catch (relative catches)
P1 <- ggplot(latitude, aes(catchpercapita, decadal_change, label = scientific_name)) +
  #geom_point(aes(color = decadal_change, size = tonnesEEZsp), alpha = 0.6) +
  geom_point(aes(color = decadal_change, size = 1), alpha = 0.6) +
  scale_colour_gradient(low="blue", high="red",guide="legend", 
                        breaks = c(0.5e+8,5.0e+8, 1.0e+9, 1.3e+9))+
  #scale_size_continuous("Species catch (tones/year)", range=c(2,15),breaks=c(250000, 500000, 750000, 1000000))+
  theme(axis.text.x = element_text(angle = -45, hjust = 0.06, size = 8),
        axis.text.y = element_text(size = 8),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        legend.key=element_blank(),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_text_repel(data=subset(latitude, latitude$catchpercapita>0.005), aes(color=decadal_change), size=3, 
                  vjust=1) +
  guides(color = guide_legend(title = "km/decade"), 
         size = guide_legend(title = "Species catch (tonesyear)")) +
  labs(x = "Fishing countries percapita catch",
       y = "latitude shift (km)")
P1

P1 + facet_wrap(~fishing_entity)

 
latitude3 <- subset(latitude, landedperGDP>0.00005)

##prueba grafico landed/gdp (catch)relative landed value)
P2 <- ggplot(latitude, aes(landedperGDP, decadal_change, label = scientific_name)) +
  #geom_point(aes(color = decadal_change, size = tonnesEEZsp), alpha = 0.6) +
  geom_point(aes(color = decadal_change, size = 1), alpha = 0.6) +
  scale_colour_gradient(low="blue", high="red",guide="legend", 
                        breaks = c(0.5e+8,5.0e+8, 1.0e+9, 1.3e+9))+
  #scale_size_continuous("Species catch (tones/year)", range=c(2,15),breaks=c(250000, 500000, 750000, 1000000))+
  theme(axis.text.x = element_text(angle = -45, hjust = 0.06, size = 8),
        axis.text.y = element_text(size = 8),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        legend.key=element_blank(),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_text_repel(data=subset(latitude, latitude$landedperGDP>0.0002), aes(color=decadal_change), size=3, 
                  vjust=1) +
  guides(color = guide_legend(title = "km/decade"), 
         size = guide_legend(title = "Species catch (tonesyear)")) +
  labs(x = "relative landed value",
       y = "latitude shift (km)")
P2

P2 + facet_wrap(~fishing_entity)


##per capita impacts are higher for nordic countries, landed value importance at the country level more important for northern and african countries


