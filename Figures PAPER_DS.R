##R code for MAIN figures new for first paper draft
##Author: Diego Salgueiro
##Date: Decemeber 5th 2018


##calling libraries
library(dplyr)
library(stats)
library(base)
library(ggplot2)
library(RColorBrewer)
library(tidyr)

##read database
Biblio_database_full <- read.csv("data/Biblio_database_full.csv", stringsAsFactors=FALSE, header=T, sep = ",")

#show variables list of Biblio_database_full
names (Biblio_database_full)

##plot relation Countries-fishing grounds by catches (tonnes)
relation1 <- ggplot(Biblio_database_full, aes(x = area_name, y = fishing_entity, fill = tonnesFEsp)) +
  geom_tile(data = subset(Biblio_database_full, !is.na(fishing_entity))) +
  theme_bw() +
  theme( axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_gradientn("Catches (tonnes)", colours = rev(brewer.pal(9, "RdYlBu"))) +
  xlab("Fishing grounds") + ylab("Countries")
relation1

##plot relation Countries-fishing grounds by landed value (USD)
relation2 <- ggplot(Biblio_database_full, aes(x = area_name, y = fishing_entity, fill = landedvalueFEsp)) +
  geom_tile(data = subset(Biblio_database_full, !is.na(fishing_entity))) +
  theme_bw() +
  theme(  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_gradientn("Landed value (USD)", colours = rev(brewer.pal(9, "Spectral")), limits=c(0,420000000)) +
  xlab("Fishing grounds") + ylab("Countries")
relation2

##create variable of dependence
Biblio_database_full$catchdepFEsp<- Biblio_database_full$tonnesFEsp/Biblio_database_full$tonnesFEspT
Biblio_database_full$landdepFEsp<- Biblio_database_full$landedvalueFEsp/Biblio_database_full$tonnesFEspT

range(Biblio_database_full$catchdepFEsp, na.rm=TRUE)
range(Biblio_database_full$landdepFEsp, na.rm=TRUE) 

##plot ecnomic dependency
relation3 <- ggplot(Biblio_database_full, aes(x = area_name, y = fishing_entity, fill = catchdepFEsp)) +
  geom_tile(data = subset(Biblio_database_full, !is.na(fishing_entity))) +
  theme_bw() +
  theme(  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_gradientn("Dependency (tonnes)", colours = rev(brewer.pal(9, "Spectral"))) +
  xlab("Fishing grounds") + ylab("Countries")
relation3

##plot catches dependency
relation4 <- ggplot(Biblio_database_full, aes(x = area_name, y = fishing_entity, fill = landdepFEsp)) +
  geom_tile(data = subset(Biblio_database_full, !is.na(fishing_entity))) +
  theme_bw() +
  theme( axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_gradientn("Dependency (USD)", colours = rev(brewer.pal(9, "Spectral"))) +
  xlab("Fishing grounds") + ylab("Countries")
relation4

## If we want to modify scale, code here: ", limits=c(0,1250000), breaks=seq(0,1250000, by=1000000)"

