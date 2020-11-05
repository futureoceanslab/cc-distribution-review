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
source("function_multiplot.R")
library(reshape2)#melt function
library(readxl)

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

# Subsets of the impacts depending on the response type lat or depth
data$response <- as.factor(data$response)
levels(data$response)
latitude <- subset (data, response == "latitude")
depth <- subset (data, response == "depth")

#Color palette
myP <- rev(brewer.pal(n = 8, name = 'RdBu'))


#####2. FIGURE 3 ####
#Changing a few names for plotting
lat <- latitude
dep <- depth
lat$area_name[lat$area_name == "Canada (East Coast)"] <- "Can-East"
dep$area_name[dep$area_name == "Canada (East Coast)"] <- "Can-East"
lat$area_name[lat$area_name == "Korea (South)"] <- "S-Korea"
dep$area_name[dep$area_name == "Korea (South)"] <- "S-Korea"
lat$area_name[lat$area_name == "United Kingdom (UK)"] <- "UK"
dep$area_name[dep$area_name == "United Kingdom (UK)"] <- "UK"
lat$area_name[lat$area_name == "USA (Alaska, Subarctic)"] <- "Alaska"
dep$area_name[dep$area_name == "USA (Alaska, Subarctic)"] <- "Alaska"
lat$area_name[lat$area_name == "Japan (main islands)"] <- "Japan"
dep$area_name[dep$area_name == "Japan (main islands)"] <- "Japan"

#LAT
max((lat$landedvalueFEsp/lat$landedvalueFE)*100)
lat$brea <- cut((lat$landedvalueFEsp/lat$landedvalueFE)*100, 
                breaks = unique(c(seq(0, 15, by = 2.5), seq(15, 40, by = 5))))

P3 <- ggplot(lat, aes(catchdepFE*100, decadal_change, 
                           label = paste(scientific_name, ", ", area_name, sep =""))) + #paste(scientific_name, ", ", area_name, sep ="" +
        geom_point(aes(color = brea, alpha = 0.7), size = 3) +
        scale_colour_manual("Value dependency\non species (%)", 
                            values = rev(c("#a50026", "#d73027", "#f46d43", "#fdae61", 
                              "#fee090", "#abd9e9", "#74add1", "#4575b4", "#313695"))) +
        theme(axis.text.x = element_text(angle = -45, hjust = 0.06, size = 10),
              axis.text.y = element_text(size = 10),
              panel.background = element_rect(fill = "white"),
              axis.line.x = element_line(colour = c("black")),
              axis.line.y = element_line(colour = c("black")),
              legend.key=element_blank(),
              axis.title.y = element_text(size = 14),
              axis.title.x = element_text(size = 14),
              plot.title = element_text(size = 20)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
        geom_text_repel(data = subset(lat, catchdepFE*100 > 6), 
                        aes(color = brea), size = 2.5,
                        vjust = -2.5, hjust = "inward") +
        guides(size = F,
               alpha = F) +
        labs(x = "Catch dependency on species (%)",
             y = "Latitude shift (km/decade)") +
        facet_wrap(~ fishing_entity, ncol = 3)+
        ggtitle("a)") 

#DEPTH
max((dep$landedvalueFEsp/dep$landedvalueFE)*100)
dep$brea <- cut((dep$landedvalueFEsp/dep$landedvalueFE)*100, 
                breaks = unique(c(seq(0, 15, by = 2.5), seq(15, 35, by = 5), seq(35, 46, by = 11))))

P4 <-  ggplot(dep, aes(catchdepFE*100, decadal_change, 
                      label = paste(scientific_name, ", ", area_name, sep =""))) + #paste(scientific_name, ", ", area_name, sep ="" +
        geom_point(aes(color = brea, alpha = 0.7), size = 3) +
        scale_colour_manual("Value dependency\non species (%)", 
                            values = rev(c("#a50026", "#d73027", "#f46d43", "#fdae61", 
                                           "#fee090", "#abd9e9", "#74add1", "#4575b4", "#313695"))) +
        theme(axis.text.x = element_text(angle = -45, hjust = 0.06, size = 10),
              axis.text.y = element_text(size = 10),
              panel.background = element_rect(fill = "white"),
              axis.line.x = element_line(colour = c("black")),
              axis.line.y = element_line(colour = c("black")),
              legend.key=element_blank(),
              axis.title.y = element_text(size = 14),
              axis.title.x = element_text(size = 14),
              plot.title = element_text(size = 20)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
        geom_text_repel(data = subset(dep, catchdepFE*100 > 6), 
                        aes(color = brea), size = 2.5,
                        vjust = -2.5, hjust = "inward") +
        guides(size = F,
               alpha = F) +
        labs(x = "Catch dependency on species (%)",
             y = "Depth shift (m/decade)") +
        facet_wrap(~ fishing_entity, ncol = 3) +
        ggtitle("b)") 

##Paper Fig 3 IMpact and Catch dependency
png(file = "paper_figures/figure_3a.png", 
 width = 7, height = 10, units = 'in', res = 600)
 P3
dev.off()

png(file = "paper_figures/figure_3b.png", 
 width = 7, height = 10, units = 'in', res = 600)
 P4
dev.off()

rm(P3, P4, lat, dep, depth, latitude)

#####3. FIGURE 4 ####
sdg <- read_xlsx("data/SDR2020Database.xlsx", sheet = 4)[,c(1:3,36,37)]
colnames(sdg) <- c("fishing_entity", "country_code", "sdg", "color_sdg14", "trend_sdg14")
unique(data$fishing_entity)[unique(data$fishing_entity) %in% unique(sdg$fishing_entity) == F]
sdg$fishing_entity[sdg$fishing_entity == "United States"] <- "USA"
sdg$fishing_entity[sdg$fishing_entity == "Korea, Rep."] <- "Korea (South)"

#VERIFICATIONS OK!
# a <- data %>%
#       group_by(fishing_entity, area_name) %>%
#       summarise(catchdepFEEZ = unique(catchdepFEEZ))
# 
# a$prop_catchdepFEEZ <- round(a$catchdepFEEZ*100,0)
# 
# a2 <- a %>%
#         group_by(fishing_entity) %>%
#         summarise(catchdepFEEZ = sum(catchdepFEEZ))
# a2$prop_catchdepFEEZ <- round(a2$catchdepFEEZ*100,0)

d_FE <- data %>% ###our data by FE
          group_by(fishing_entity, area_name, scientific_name) %>%
          summarise(tonnesFEsp = unique(tonnesFEsp),
                    landedvalueFEsp = unique(landedvalueFEsp),
                    tonnesFE = unique(tonnesFE),
                    landedvalueFE = unique(landedvalueFE)) %>%
          group_by(fishing_entity) %>%
          summarise(affected_catch = sum(tonnesFEsp),
                    affected_value = sum(landedvalueFEsp),
                    tonnesFE = unique(tonnesFE),
                    landedvalueFE = unique(landedvalueFE),
                    affected_catch_prop = round((affected_catch/tonnesFE)*100,2))

d_other_var <- left_join(d_FE, sdg, by = "fishing_entity")

#FIGURE5!!!!!!!!!!!!!!
d_other_var$color_sdg14 <- factor(d_other_var$color_sdg14, levels = c("yellow", "orange", "red"))

png(file = "paper_figures/figure_5.png", 
    width = 11, height = 7, units = 'in', res = 600)
ggplot(d_other_var[complete.cases(d_other_var), ], 
       aes(sdg, affected_catch_prop)) +
  geom_point(aes(color = color_sdg14, shape = trend_sdg14),
             size = 8) +
  scale_color_manual(values = c("yellow", "orange", "red"), 
                     labels = c("Challenges remain", "Significant challenges", "Major challenges")) +
  geom_text_repel(aes(label = country_code),
                  hjust=0, vjust=0, size = 5) +
  ylab("Affected catch prop.") +
  xlab("SDG total score") +
  guides(color = guide_legend(title = "Status SDG 14", order = 1),
         shape = guide_legend(title = "Trend SDG 14", order = 2)) +
  theme_classic() +
  geom_hline(yintercept = 25, linetype = "dashed", color = "black", size = 1) +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16)) 
dev.off()


#####4. OTHER FIGURES ####
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
            summarise(affected_catch = sum(tonnesFEsp, na.rm = T),
                      affected_value = sum(landedvalueFEsp, na.rm = T))
d_plot <- left_join(dall_FE_area, d_area, by = c("fishing_entity", "area_name"))
d_plot[is.na(d_plot)] <- 0
d_plot$na_catch <- d_plot$tonnesFEEZ - d_plot$affected_catch
d_plot$na_value <- d_plot$landed_valueFEEZ - d_plot$affected_value

d_plot$catch_na_prop <- round((d_plot$na_catch*100)/d_plot$tonnesFEEZ,2)
d_plot$value_na_prop <- round((d_plot$na_value*100)/d_plot$landed_valueFEEZ,2)
d_plot$catch_affect_prop <- round((d_plot$affected_catch*100)/d_plot$tonnesFEEZ,2)
d_plot$value_affect_prop <- round((d_plot$affected_value*100)/d_plot$landed_valueFEEZ,2)
d_plot$tot_catch <- d_plot$catch_na_prop + d_plot$catch_affect_prop
d_plot$tot_value <- d_plot$value_na_prop + d_plot$value_affect_prop  


d_plot2 <- d_plot %>%
  group_by(fishing_entity) %>%
  summarise(assessed_catch = (sum(affected_catch)*100)/sum(tonnesFEEZ),
            unassessed_catch = ((sum(tonnesFEEZ)-sum(affected_catch))*100)/sum(tonnesFEEZ))#,

#REVISIONS
# d_plot[1,3] + d_plot[2,3]
# filter(data, fishing_entity == "Angola")[1,20]

#prop_na_value = ((sum(landed_valueFEEZ)-sum(affected_value))*100)/sum(landed_valueFEEZ))
pl <- melt(d_plot2) 

pl$fishing_entity[pl$fishing_entity == "Saint Pierre & Miquelon (France)"] <- "St P. & Miquelon (France)"
#FIGURE2b
png(file = "paper_figures/figure_2b.png", 
    width = 11, height = 7, units = 'in', res = 600)
ggplot(pl, aes(fishing_entity, value, fill = rev(variable))) +
  scale_fill_manual(values = c("grey","#045a8d"), name = "Affected catch?",
                    labels = c("Non-assessed","Yes"), guide = guide_legend(reverse = T)) +
  #coord_polar() +
  geom_bar(stat = "identity") +
  # geom_hline(yintercept = seq(0, 500, by = 100), color = "grey80", size = 0.3) +
  # scale_x_continuous(breaks = 0:24, expand = c(.002,0)) +
  labs(x = "Fishing entity", y = "Proportion of total catch") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 20))+
  ggtitle("b)") #+
  #geom_hline(yintercept = 25, linetype = "dashed", color = "black", size = 1) 
dev.off()



#Figure 2c 
data2 <- data %>% 
  group_by(fishing_entity) %>%
  summarise(response = paste(unique(response), collapse = "-"))

data2$response[data2$response == "depth-latitude"] <- "latitude-depth"

d_plot3 <- d_plot %>%
  group_by(fishing_entity) %>%
  summarise(assessed_catch = sum(affected_catch),
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
  
  scale_fill_manual(values = c("#bdc9e1", "#2b8cbe", "grey"), name = "Affected catch?",
                    labels = c("Latitude", "Latitude & Depth","Non-assessed")) +
  labs(x = "Fishing entity", y = "Total catch (t)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 20)) +
  ggtitle("c)") 

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
                      affected_catch = sum(affected_catch, na.rm = T),
                      na_catch = sum(na_catch, na.rm = T))

d_plot5b <- left_join(d_plot5b, d_FE_subset, by = "fishing_entity") 

d_plot5b <- d_plot5b %>%
            mutate(affected_catch_prop = (affected_catch*100)/tonnesFE,
                   na_catch_prop = (na_catch*100)/tonnesFE)

verification <- d_plot5b %>%
                  group_by(fishing_entity) %>%
                  summarise(total = sum(affected_catch_prop + na_catch_prop))
#PORTUGAL!!!!!!!!!!!!!!!!!!
d_plot5c <- melt(d_plot5b[,colnames(d_plot5b) %in% c ("fishing_entity", "in_out", "eez_number",
                                                 "affected_catch_prop", "na_catch_prop")], 
                id.vars = c("fishing_entity", "in_out", "eez_number"))

d_plot5c$in_out <- factor(d_plot5c$in_out, levels = c("Own EEZ", "Other EEZ"))

ggplot(d_plot5c, aes(fct_rev(fishing_entity), value, fill = variable)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#045a8d", "grey"), name = "Affected catch?",
                    labels = c("Yes", "Non-assessed")) +
  # geom_text(data = subset(d_plot5c, variable == "affected_catch_prop"),
  #           aes(fishing_entity, value, label = eez_number), size = 4) +
  facet_grid(~ in_out, space = "free") + #scales="free", 
  coord_flip()+
  labs(x = "Fishing entity", y = "% of catch") +
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

data$eez_number[data$variable == "affected_catch_prop"] <- NA

data[dim(data)[1]+1,] <- c("Azores Isl", "Other EEZ", 0, "na_catch_prop", 0)
data$eez_number <- as.numeric(data$eez_number)
data$value <- as.numeric(data$value)

nudge_fun <- function(df){
  ifelse(df$in_out == "Own EEZ", -102-df$value, 102-df$value)
}

png(file = "paper_figures/figure4.png", 
    width = 11, height = 7, units = 'in', res = 600)
ggplot(data, aes(x = fishing_entity, fill = variable)) + 
  scale_fill_manual(values = c("#045a8d", "grey"), name = "Affected catch?",
                    labels = c("Yes", "Non-assessed")) +
  geom_bar(data = subset(data, in_out == "Own EEZ"),
           aes(y = -value), 
           position = position_stack(reverse = T), 
           stat = "identity") +
  geom_bar(data = subset(data, in_out == "Other EEZ"), 
           aes(y = value),
           position = position_stack(reverse = T), 
           stat = "identity") +
  geom_text(aes(y = value, label = eez_number),
            position = position_nudge(y = nudge_fun(data)),
            size = 4
  ) +
  coord_flip() +
  xlab("Fishing entity") +
  ylab("Catch %") +
  theme_bw() +
  theme(axis.text = element_text(size = 16, color = "black"),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 18),
        strip.text.x = element_text(size = 14)) +
  geom_hline(yintercept= 0, color = "black") +
  scale_y_continuous(labels = abs) +
  ggtitle("             Own EEZ                         Other EEZ")
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
