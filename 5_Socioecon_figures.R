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
library(reshape2)#melt function
library(readxl)
source("function_multiplot.R")

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
write.csv(data, "data/biblio_database_full2.csv", row.names = F)# FOR MAPS
rm(remove, remove2, allsp, lostsp, rm_id, sp)

#Changing a few names for plotting
data$fishing_entity <- as.character(data$fishing_entity)
data$area_name <- as.character(data$area_name)
data$fishing_entity[data$fishing_entity == "Saint Pierre & Miquelon (France)"] <- "St Pierre & Miquelon (Fr)"

data$response <- as.factor(data$response)
levels(data$response)


#####FIGURE 5 ####
sdg <- read_xlsx("data/SDR2020Database.xlsx", sheet = 4)[,c(1:3, 34:37, 40:43, 596,597,599,600)]
colnames(sdg) <- c("fishing_entity", "country_code", "sdg",
                        "sdg13_dash", "sdg13_trend", "sdg14_dash", "sdg14_trend",
                        "sdg16_dash", "sdg16_trend", "sdg17_dash", "sdg17_trend",
                        "sdg13_score", "sdg14_score", "sdg16_score", "sdg17_score")
unique(data$fishing_entity)[unique(data$fishing_entity) %in% unique(sdg$fishing_entity) == F]
sdg$fishing_entity[sdg$fishing_entity == "United States"] <- "USA"
sdg$fishing_entity[sdg$fishing_entity == "Korea, Rep."] <- "Korea (South)"

sdg2 <- gather(sdg, key = "attribute", value = "measurement",
               sdg13_dash, sdg13_trend, sdg14_dash, sdg14_trend, sdg16_dash, sdg16_trend, 
               sdg17_dash, sdg17_trend, sdg13_score, sdg14_score, sdg16_score, sdg17_score)

sdg3 <- sdg2[grep("score", sdg2$attribute),]
sdg3$measurement <- as.numeric(sdg3$measurement)

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
                    vulnerable_catch_prop = round((vulnerable_catch/tonnesFE)*100,2))

d_other_var <- left_join(d_FE, sdg3, by = "fishing_entity")

country_colours <- data.frame(fishing_entity = c("USA", "Canada", "Greenland", "Namibia", "Angola", "South Africa",
                                          "Japan", "Korea (South)", "China", "Russian Federation", "Poland",
                                          "Estonia", "Portugal", "Azores Isl", "Norway", "Spain", "Ireland",
                                          "Germany", "Faeroe Isl", "Iceland", "United Kingdom", "Sweden",
                                          "Netherlands", "Denmark", "France", "Belgium", "St Pierre & Miquelon (Fr)"), 
                              col = c(rep("black",3), rep("#CC79A7",3), rep("orange", 4), rep("#56B4E9", 17)),
                              stringsAsFactors = FALSE)

d_other_var <- left_join(d_other_var, country_colours, by = "fishing_entity")

#d_other_var$color_sdg14 <- factor(d_other_var$color_sdg14, levels = c("yellow", "orange", "red"))

png(file = "paper_figures/figure_5.png", 
    width = 13, height = 7, units = 'in', res = 600)
ggplot(d_other_var[complete.cases(d_other_var), ], 
       aes(vulnerable_catch_prop, measurement)) +
  geom_point(aes(color = col), #, shape = trend_sdg14
             size = 5) +
  scale_color_manual(values = c("#56B4E9", "#CC79A7", "black", "orange"),
                     labels = c("Europe", "Africa", "America", "Asia")) +
  geom_text_repel(aes(label = country_code),
                 size = 4) +
  xlab("Vulnerable catch (%)") +
  ylab("SDG total score") +
  # guides(color = guide_legend(title = "Continent", order = 1),
  #        shape = guide_legend(title = "SDG trend", order = 2)) +
  theme_bw() +
  geom_vline(xintercept = 25, linetype = "dashed", color = "black", size = 1) +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 18),
        # legend.title = element_text(size = 18),
        # legend.text = element_text(size = 16),
        legend.position = "none",
        strip.text = element_text(size = 16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(color="black")) +
  facet_wrap(~attribute, labeller = as_labeller(c(`sdg13_score` = "SDG 13", 
                                                  `sdg14_score` = "SDG 14",
                                                  `sdg16_score` = "SDG 16", 
                                                  `sdg17_score` = "SDG 17")))
dev.off()

rm(sdg, sdg2, sdg3, d_other_var, country_colours)

#####FIGURES 3 & 4 ####

d_FE$non_vulnerable_catch_prop <- NA
d_FE$non_vulnerable_catch_prop <- 100 - d_FE$vulnerable_catch_prop

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

d_FE$non_vulnerable_catch <- NA
d_FE$non_vulnerable_catch <- d_FE$tonnesFE - d_FE$vulnerable_catch
pl2 <- d_FE[c("fishing_entity", "vulnerable_catch", "non_vulnerable_catch")]
pl2 <- gather(pl2, key = "var", value = "measure",
             vulnerable_catch, non_vulnerable_catch)
pl2$fishing_entity <- as.factor(pl$fishing_entity)

pl$facet <- 1
pl2$facet <- 2

pl3 <- rbind(pl, pl2)
pl3$name <- pl3$var
pl3$name[pl3$name == "vulnerable_catch_prop"] <- "vulnerable_catch"   
pl3$name[pl3$name == "non_vulnerable_catch_prop"] <- "Unknown" 
pl3$name[pl3$name == "non_vulnerable_catch"] <- "Unknown" 


#Figure 3
png(file = "paper_figures/figure_2bb.png",
    width = 9, height = 7, units = 'in', res = 600)

pl3 %>%
  mutate(fishing_entity = fct_relevel(fishing_entity, 
                                      paste(names2))) %>%
  ggplot(aes(fishing_entity, measure)) +
  geom_bar(aes(fill = name), stat = "identity") +
  scale_fill_manual(values = c("grey","#045a8d"),
                    labels = c("Unknown", "Vulnerable"),
                    guide = guide_legend(reverse = T)) +
  labs(x = "Fishing country", y = "") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16),
    axis.text.y = element_text(size = 16),
    axis.title = element_text(size = 18),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    plot.title = element_text(size = 20),
    # strip.text.x = element_blank(),
    # strip.text.y = element_blank(),
    strip.text = element_text(size = 16),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()) +
  facet_wrap(~facet, scales = "free_y", nrow = 2, 
             labeller = as_labeller(c(`1` = "Catch proportion (%)", 
                                      `2` = "Catch (t)")))

dev.off()


#Read data for all fishing countries
path <- "data/data_FE_SAU/" #Final_SAU_FE, script integration SAU
l <- list.files(path, pattern = ".csv")
l2 <- str_sub(l, end = -5) 
names <- unique(data_original$fishing_entity)[-c(11,24)] 
names %in% l2
Final_SAU_FE <- read.csv("data/SAU_all.csv") #FINAL SAU FE l. 90
Final_SAU_FE$fishing_entity[Final_SAU_FE$fishing_entity == "Faeroe Isl.(Denmark)"] <- "Faeroe Isl"
Final_SAU_FE$fishing_entity[Final_SAU_FE$fishing_entity == "Azores Isl. (Portugal)"] <- "Azores Isl"
names %in% unique(Final_SAU_FE$fishing_entity)
data1 <- filter(Final_SAU_FE, fishing_entity %in% names)#all our fishing entities
dall_FE_area <- data1 %>% #all SAU data of our FEs by FE
                  group_by(fishing_entity, area_name, year, scientific_name) %>%
                  summarise(tonnesFEspyear = sum(tonnes, na.rm = T),
                            landedvalueFEspyear = sum(landed_value, na.rm = T)) %>%
                  group_by(fishing_entity, area_name, scientific_name) %>%
                  summarise(tonnesFEsp = mean(tonnesFEspyear, na.rm = T),
                            landedvalueFEsp = mean(landedvalueFEspyear, na.rm = T)) %>%
                  group_by(fishing_entity, area_name) %>%
                  summarise(tonnesFEEZ = sum(tonnesFEsp, na.rm = T),
                            landed_valueFEEZ = sum(landedvalueFEsp, na.rm = T))

rm(data1, path, l, l2, names, Final_SAU_FE)
#REVISIONS
# rev <- data %>%
#         group_by(fishing_entity, area_name) %>%
#         summarise(tonnesFEEZ = unique(tonnesFEEZ))
# rev[1,3]
# dall_FE_area[1,3]
# rev2 <- left_join(rev, dall_FE_area, by = c("fishing_entity", "area_name"))

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

d_plot$catch_na_prop <- round((d_plot$na_catch*100)/d_plot$tonnesFEEZ,2)
d_plot$value_na_prop <- round((d_plot$na_value*100)/d_plot$landed_valueFEEZ,2)
d_plot$catch_affect_prop <- round((d_plot$vulnerable_catch*100)/d_plot$tonnesFEEZ,2)
d_plot$value_affect_prop <- round((d_plot$vulnerable_value*100)/d_plot$landed_valueFEEZ,2)
d_plot$tot_catch <- d_plot$catch_na_prop + d_plot$catch_affect_prop
d_plot$tot_value <- d_plot$value_na_prop + d_plot$value_affect_prop  


d_plot2 <- d_plot %>%
  group_by(fishing_entity) %>%
  summarise(assessed_catch = (sum(vulnerable_catch)*100)/sum(tonnesFEEZ),
            unassessed_catch = ((sum(tonnesFEEZ)-sum(vulnerable_catch))*100)/sum(tonnesFEEZ))#,

#REVISIONS
# d_plot[1,3] + d_plot[2,3]
# filter(data, fishing_entity == "Angola")[1,20]

#prop_na_value = ((sum(landed_valueFEEZ)-sum(vulnerable_value))*100)/sum(landed_valueFEEZ))
pl <- melt(d_plot2) 

pl$fishing_entity[pl$fishing_entity == "Saint Pierre & Miquelon (France)"] <- "St P. & Miquelon (France)"
#FIGURE2b
png(file = "paper_figures/figure_2b.png", 
    width = 11, height = 7, units = 'in', res = 600)
ggplot(pl, aes(fishing_entity, value, fill = rev(variable))) +
  scale_fill_manual(values = c("grey","#045a8d"),
                    labels = c("Unknown","Vulnerable catch"), guide = guide_legend(reverse = T)) +
  #coord_polar() +
  geom_bar(stat = "identity") +
  # geom_hline(yintercept = seq(0, 500, by = 100), color = "grey80", size = 0.3) +
  # scale_x_continuous(breaks = 0:24, expand = c(.002,0)) +
  labs(x = "Fishing country", y = "Proportion of total catch") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 20))+
  ggtitle("a)") #+
  #geom_hline(yintercept = 25, linetype = "dashed", color = "black", size = 1) 
dev.off()



#Figure 2c 
data2 <- data %>% 
  group_by(fishing_entity) %>%
  summarise(response = paste(unique(response), collapse = "-"))

data2$response[data2$response == "depth-latitude"] <- "latitude-depth"

d_plot3 <- d_plot %>%
  group_by(fishing_entity) %>%
  summarise(assessed_catch = sum(vulnerable_catch),
            unassessed_catch = sum(na_catch))

d_plot3$fishing_entity[d_plot3$fishing_entity == "Saint Pierre & Miquelon (France)"] <- "St Pierre & Miquelon (Fr)"
int <- left_join(data2, d_plot3[,1:2], by = "fishing_entity")
d_plot3[,2] <- "Non-assessed"
colnames(d_plot3)[c(2,3)] <- c("response", "assessed_catch") #we change the name of column 2 fo binding in the next line

d_plot4 <- rbind(d_plot3, int)
rm(int)

png(file = "paper_figures/figure_2c.png", 
    width = 11, height = 7, units = 'in', res = 600)
ggplot(d_plot4, aes(fishing_entity, assessed_catch, fill = response)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  
  scale_fill_manual(values = c("darkturquoise", "#2b8cbe", "grey"), name = "Vulnerable catch?",
                    labels = c("Latitude", "Latitude & Depth","Non-assessed")) +
  labs(x = "Fishing country", y = "Total catch (t)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 20)) +
  ggtitle("b)") 

dev.off()


#FIGURE4
d_FE_subset <- d_FE[colnames(d_FE) %in% c ("fishing_entity", "tonnesFE", "landedvalueFE")] 
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
# d_plot5$fishing_entity[d_plot5$fishing_entity == "Azores Isl"] <- "Portugal"
d_plot5$area_name_simpl[grep("Russia", d_plot5$area_name_simpl)] <- "Russian Federation"
unique(d_plot5$fishing_entity) %in% unique(d_plot5$area_name_simpl)
unique(d_plot5$fishing_entity)[unique(d_plot5$fishing_entity) %in% unique(d_plot5$area_name_simpl) == F]


#add own/other EEZ var
d_plot5$in_out <- "Other EEZ"
d_plot5 <- as.data.frame(d_plot5)
for (i in 1:dim(d_plot5)[1]) {
  if(identical(d_plot5[i,colnames(d_plot5) == "fishing_entity"], d_plot5[i,colnames(d_plot5) == "area_name_simpl"])==T) {
    d_plot5$in_out[i] <- "Own EEZ"
  }
}

d_plot5b <- d_plot5 %>%
            group_by(fishing_entity, in_out) %>%
            summarise(eez_number = n(),
                      vulnerable_catch = sum(vulnerable_catch, na.rm = T),
                      na_catch = sum(na_catch, na.rm = T))

d_plot5b <- left_join(d_plot5b, d_FE_subset, by = "fishing_entity") 

d_plot5b <- d_plot5b %>%
            mutate(vulnerable_catch_prop = (vulnerable_catch*100)/tonnesFE,
                   na_catch_prop = (na_catch*100)/tonnesFE)

verification <- d_plot5b %>%
                  group_by(fishing_entity) %>%
                  summarise(total = sum(vulnerable_catch_prop + na_catch_prop))
#PORTUGAL!!!!!!!!!!!!!!!!!!
d_plot5c <- melt(d_plot5b[,colnames(d_plot5b) %in% c ("fishing_entity", "in_out", "eez_number",
                                                 "vulnerable_catch_prop", "na_catch_prop")], 
                id.vars = c("fishing_entity", "in_out", "eez_number"))

d_plot5c$in_out <- factor(d_plot5c$in_out, levels = c("Own EEZ", "Other EEZ"))

ggplot(d_plot5c, aes(fct_rev(fishing_entity), value, fill = variable)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#045a8d", "grey"), name = "vulnerable catch?",
                    labels = c("Yes", "Non-assessed")) +
  # geom_text(data = subset(d_plot5c, variable == "vulnerable_catch_prop"),
  #           aes(fishing_entity, value, label = eez_number), size = 4) +
  facet_grid(~ in_out, space = "free") + #scales="free", 
  coord_flip()+
  labs(x = "Fishing country", y = "% of catch") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 20),
        strip.text.x = element_text(size = 14)) 

write.csv(d_plot5c, "data.csv", row.names = F)      
data <- d_plot5c

data$eez_number[data$variable == "vulnerable_catch_prop"] <- NA

data[dim(data)[1]+1,] <- c("Azores Isl", "Other EEZ", 0, "na_catch_prop", 0)
data$eez_number <- as.numeric(data$eez_number)
data$value <- as.numeric(data$value)

nudge_fun <- function(df){
  ifelse(df$in_out == "Own EEZ", -102-df$value, 102-df$value)
}

png(file = "paper_figures/figure4.png", 
    width = 9, height = 7, units = 'in', res = 600)
ggplot(data, aes(x = fishing_entity, fill = variable)) + 
  scale_fill_manual(values = c("#045a8d", "grey"), 
                    labels = c("Vulnerable catch", "Non-assessed")) +
  geom_bar(data = subset(data, in_out == "Own EEZ"),
           aes(y = -value), 
           position = position_stack(reverse = T), 
           stat = "identity") +
  geom_bar(data = subset(data, in_out == "Other EEZ"), 
           aes(y = value),
           position = position_stack(reverse = T), 
           stat = "identity") +
  # geom_text(aes(y = value, label = eez_number),
  #           position = position_nudge(y = nudge_fun(data)),
  #           size = 4
  # ) +
  coord_flip() +
  xlab("Fishing country") +
  ylab("Catch (%)") +
  theme_bw() +
  theme(axis.text = element_text(size = 16, color = "black"),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = "bottom",
        plot.title = element_text(size = 18),
        strip.text.x = element_text(size = 14)) +
  geom_hline(yintercept= 0, color = "black") +
  scale_y_continuous(labels = abs) +
  ggtitle("a)               Own EEZ                         Other EEZ")
dev.off()

EEZ <- na.omit(data[c("fishing_entity","in_out","eez_number")])
EEZ <- spread(EEZ, "in_out", "eez_number")
colnames(EEZ) <- c("country", "own", "other")
EEZ$other[EEZ$other == 0] <-NA

png(file = "paper_figures/figure4b.png", 
    width = 6, height = 6, units = 'in', res = 600)
ggplot(EEZ) +
  geom_segment(aes(x=country, xend=country, y=-0.1, yend=0.1), color="black") +
  geom_point(aes(x=country, y=-0.1, size = own*3, color = "black")) +
  geom_point(aes(x=country, y=0.1, size = other*3, color = "blue")) +
  scale_size_continuous(range = c(3, 13)) +
  scale_color_discrete(name = "Number of EEZ", labels = c("Own", "Other")) +
  coord_flip()+
  theme_minimal() +
  geom_text(aes(x=country, y = 0.1, label = other),
            size = 5) +
  geom_text(aes(x=country, y = -0.1, label = own),
            size = 5) +
  theme(axis.text = element_text(size = 16, color = "black"),
        axis.text.x = element_blank(),
        plot.title = element_text(size = 16, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 15, color = "black"),
        legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  xlab("") +
  ylab("") +
  ggtitle("b)") +
  guides(size = F,
         color = guide_legend(override.aes = list(size = 5)))
dev.off()
##################################### SUPPLEMENTARY MATERIALS #############################################

##Making a specific subset to plot the specific varibales
mybiblio_database <- read.csv("data/biblio_database.csv", stringsAsFactors=FALSE, header=T, sep = ",")

##Subset of biblio_database without duplicated data
##mybiblio_database<- mybiblio_database%>%
##  filter(mybiblio_database$duplicate=="1")
mybiblio_database<-mybiblio_database[,c(2, 3,4, 6, 8,32,33,59, 63)]
str(mybiblio_database)
mybiblio_database<- as.data.frame(mybiblio_database)

##order in time 
mybiblio_database<-mybiblio_database[order(mybiblio_database$article_year),]

##creating new column with the number of observations
mybiblio_database$observations<-1 

###### characteristics
names(mybiblio_database)
glimpse( mybiblio_database)

##1st graph : obersvations-papers-duration of studies
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==10]<-"0-25"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==15]<-"0-25"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==17]<-"0-25"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==23]<-"0-25"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==24]<-"0-25"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==25]<-"0-25"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==27]<-"26-35"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==28]<-"26-35"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==30]<-"26-35"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==31]<-"26-35"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==35]<-"26-35"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==38]<-"35-45"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==39]<-"35-45"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==40]<-"35-45"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==41]<-"35-45"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==42]<-"35-45"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==75]<-"75-80"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==77]<-"75-80"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==78]<-"75-80"
mybiblio_database$categoriesbyyears[mybiblio_database$years_data==80]<-"75-80"

##Re-order the axis from alphabetical to dataframe's order
mybiblio_database$reference <- reorder( mybiblio_database$reference, mybiblio_database$article_year)

##Tailoring data for next plotting
mybiblio_database$years_data_pattern<- factor( mybiblio_database$years_data_pattern, levels=c("1","2"),
                                            labels=c("Continuous","Discontinuous")) 

mybiblio_database$data_type<- factor( mybiblio_database$data_type, levels=c("1","4"),
                                      labels=c("Data collected","Biological surveys"))

mybiblio_database$statistical_methodology<- factor( mybiblio_database$statistical_methodology, levels=c("1","2","5","2,3"),
                                                    labels=c("Correlation","Regression","Indirect relation", "Regression and Spatial analysis"))

##FIGURES
##1st Figure: Stalked-Bar-Chart that shows "nº observations-duration of studies-statistical methodology"
ggplot(data = mybiblio_database, aes(x = categoriesbyyears, y = observations, fill = statistical_methodology)) + 
  geom_bar(stat="identity", alpha=0.8) + scale_fill_manual(values=c("darkgreen", "yellow", "orange", "orangered")) +
  labs(title="Quality of data-source", y = "Number of observations", x = "Duration of the studies (Years)", fill = "Statistical methodology")+ 
  theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'), legend.position= "left")

##2d figure: Table with general information.
## Plotting a table including following variables: reference, article_year, years_data, years_data_patern, data_type and statistical_methodology 
#creating a new subset
mytable<-mybiblio_database[,c(3,4,6,7,8,9,10)]
mytable
##Re-order the columns in the dataframe
mytable<-mytable[, c(1,2,7,3,4,5,6)]
##Save the table in csv to further manipulation
write.csv(mytable, file = "data/Methodsdata.csv")

## Creation of a database in excell (called "Methods_table.csv") based on the information of mytable dataframe, but manually simplified.## Need to be codified.
##Open "Methods_table.csv"
Mytable<- read.csv("data/Methods_table.csv", stringsAsFactors=FALSE, header=T, sep = ";")
##Transforming the database in a dataframe
Mytable<- as.data.frame(Mytable)

##Exporting and save the second figure (table) as a pdf
pdf(file = "data/Description of database.pdf", height = 11, width = 13)
grid.table(Mytable, rows=NULL)
dev.off() 
