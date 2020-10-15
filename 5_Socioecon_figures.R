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
P3 <- ggplot(lat, aes(catchdepFE*100, decadal_change, 
                           label = paste(scientific_name, ", ", area_name, sep =""))) + #paste(scientific_name, ", ", area_name, sep ="" +
        geom_point(aes(color = (landedvalueFEsp/landedvalueFE)*100, alpha = 0.6), size = 5) +
        scale_colour_gradientn(colours = myP, name = "Value dependency\non species (%)") +
        scale_shape_manual(values = c(0:4)) +
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
                        aes(color = (landedvalueFEsp/landedvalueFE)*100), size = 3,
                        vjust = -2.5, hjust = "inward") +
        guides(size = F,
               alpha = F) +
        labs(x = "Catch dependency on species (%)",
             y = "Latitude shift (km/decade)") +
        facet_wrap(~ fishing_entity)+
        ggtitle("a)") 

#DEPTH
P4 <- ggplot(dep, aes(catchdepFE*100, decadal_change, label = paste(scientific_name, ", ", area_name, sep =""))) +
        geom_point(aes(color = (landedvalueFEsp/landedvalueFE)*100, alpha = 0.6), size = 5) +
        scale_colour_gradientn(colours = myP, name = "Value dependency\non species (%)") +
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
                        aes(color = (landedvalueFEsp/landedvalueFE)*100), size = 2.8,
                        vjust = -2.5, hjust = "inward", force = 3) +
        guides(size = F,
               alpha = F) +
        labs(x = "Catch dependency on species (%)",
             y = "Depth shift (m/decade)") +
        facet_wrap(~ fishing_entity)+
        ggtitle("b)") 

##Paper Fig 3 IMpact and Catch dependency
png(file = "paper_figures/figure_3a.png", 
 width = 14, height = 7, units = 'in', res = 600)
 P3
dev.off()

png(file = "paper_figures/figure_3b.png", 
 width = 14, height = 7, units = 'in', res = 600)
 P4
dev.off()

rm(P3, P4, lat, dep)

#####3. FIGURE 4 ####
gdp <- read.csv("data/gdp_country.csv")
HDI <- read.csv("data/HDI.csv")
dd <- full_join(gdp, HDI, by = "fishing_entity")
dd <- dd[complete.cases(dd), ]

country_code <- read.csv("data/country_code.csv")
d_other_var <- left_join(dd, country_code, by = "fishing_entity")
rm(country_code, HDI, gdp, dd)

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

d_FE <- data %>% ###LATITUDE OR DATA!!!!!!!!!!!!!!!!!
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

d_other_var <- left_join(d_FE, d_other_var, by = "fishing_entity")
d_other_var$affected_value_gdp <- (d_other_var$affected_value/d_other_var$gdp)*100

#FIGURE4!!!!!!!!!!!!!!
png(file = "paper_figures/figure_4.png", 
    width = 11, height = 7, units = 'in', res = 600)
ggplot(d_other_var, aes(HDI, affected_catch_prop)) +
  geom_point(aes(size =affected_value_gdp)) +
  geom_text_repel(aes(label = Code),
                  hjust=0, vjust=0, size = 5) +
  ylab("Maximum affected catch prop.") +
  guides(size = guide_legend(title = "Maximum affected \ngdp prop.")) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16)) 
dev.off()

#####4. OTHER FIGURES ####
#Read data for all fishing countries
path <- "data/data_FE_SAU/" #Final_SAU_FE, script integration SAU
l <- list.files(path, pattern = ".csv")
l2 <- str_sub(l, end = -5) 
names <- unique(data_original$fishing_entity)[-c(11,24)] #data from script 5
names %in% l2
Final_SAU_FE <- read.csv("data/SAU_all.csv") #FINAL SAU FE l. 90
Final_SAU_FE$fishing_entity[Final_SAU_FE$fishing_entity == "Faeroe Isl.(Denmark)"] <- "Faeroe Isl"
Final_SAU_FE$fishing_entity[Final_SAU_FE$fishing_entity == "Azores Isl. (Portugal)"] <- "Azores Isl"
names %in% unique(Final_SAU_FE$fishing_entity)
data1 <- filter(Final_SAU_FE, fishing_entity %in% names)#all our fishing entities
dall_FE_area <- data1 %>%
              group_by(fishing_entity, area_name, year) %>%
              summarise(tonnes = sum(tonnes, na.rm = T),
                        landed_value = sum(landed_value, na.rm = T)) %>%
              group_by(fishing_entity, area_name) %>%
              summarise(tonnes = mean(tonnes, na.rm = T),
                        landed_value = mean(landed_value, na.rm = T))
rm(data1, path, l, l2, names)
#REVISIONS
# rev <- data %>%
#         group_by(fishing_entity, area_name) %>%
#         summarise(tonnes = unique(tonnesFEEZ))
# rev[1,3]
# dall_FE_area[1,3]
# rev2 <- left_join(rev, dall_FE_area, by = c("fishing_entity", "area_name"))

d_area <- data %>% 
            group_by(fishing_entity, area_name, scientific_name) %>%
            summarise(tonnesFEsp = unique(tonnesFEsp),
                      landedvalueFEsp = unique(landedvalueFEsp)) %>%
            group_by(fishing_entity, area_name) %>% 
            summarise(affected_catch = sum(tonnesFEsp, na.rm = T),
                      affected_value = sum(landedvalueFEsp, na.rm = T))
d_plot <- left_join(dall_FE_area, d_area, by = c("fishing_entity", "area_name"))
d_plot[is.na(d_plot)] <- 0
d_plot$na_catch <- d_plot$tonnes - d_plot$affected_catch
d_plot$na_value <- d_plot$landed_value - d_plot$affected_value

d_plot$catch_na_prop <- round((d_plot$na_catch*100)/d_plot$tonnes,2)
d_plot$value_na_prop <- round((d_plot$na_value*100)/d_plot$landed_value,2)
d_plot$catch_affect_prop <- round((d_plot$affected_catch*100)/d_plot$tonnes,2)
d_plot$value_affect_prop <- round((d_plot$affected_value*100)/d_plot$landed_value,2)
d_plot$tot_catch <- d_plot$catch_na_prop + d_plot$catch_affect_prop
d_plot$tot_value <- d_plot$value_na_prop + d_plot$value_affect_prop  


d_plot2 <- d_plot %>%
  group_by(fishing_entity) %>%
  summarise(#total_catch = sum(tonnes),
    #total_value = sum(landed_value),
    assessed_catch = (sum(affected_catch)*100)/sum(tonnes),
    #prop_affected_value = (sum(affected_value)*100)/sum(landed_value),
    unassessed_catch = ((sum(tonnes)-sum(affected_catch))*100)/sum(tonnes))#,
#prop_na_value = ((sum(landed_value)-sum(affected_value))*100)/sum(landed_value))
pl <- melt(d_plot2) 

pl$fishing_entity[pl$fishing_entity == "Saint Pierre & Miquelon (France)"] <- "St P. & Miquelon (France)"
#FIGURE2
png(file = "paper_figures/figure_2b.png", 
    width = 11, height = 7, units = 'in', res = 600)
ggplot(pl, aes(fishing_entity, value, fill = variable)) +
  scale_fill_manual(values = c("black", "grey"), name = "Affected catch?",
                    labels = c("Yes", "No")) +
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
  ggtitle("b)") 
dev.off()


d_plot3 <- d_plot %>%
            group_by(fishing_entity) %>%
            summarise(total_catch = sum(tonnes),
                      affected_catch = sum(affected_catch))
d_plot3 <- melt(d_plot3) 
#ALTERNATIVE 2b pr SI
png(file = "paper_figures/figure_2b_alternative1.png", 
    width = 11, height = 7, units = 'in', res = 600)
ggplot(d_plot3, aes(fishing_entity, value, fill = fct_rev(variable))) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(values = c("black", "grey"), name = "Affected catch?",
                    labels = c("Yes", "Non-assessed")) +
  labs(x = "Fishing entity", y = "Total catch (t)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 20))
dev.off()

all <- d_plot %>%
  group_by(fishing_entity) %>%
  summarise(toneesFE = sum(tonnes),
            landedvalueFE = sum(landed_value))
all2 <- left_join(d_plot, all, by = "fishing_entity")
all2 <- all2 %>%
  mutate(catchdepFEEZ = round((tonnes/toneesFE)*100,4))

#Figure 2b alternative
dataa2 <- data %>% 
  group_by(fishing_entity) %>%
  summarise(response = paste(unique(response), collapse = "-"),
            affected_catch = unique(tonnesFE)) #it's total catch but name changed for plotting purposes

dataa2$response[dataa2$response == "depth-latitude"] <- "latitude-depth"

d_other_var$response <- "non-assessed"
d42 <- d_other_var[,colnames(d_other_var) %in% c("fishing_entity", 
                                                 "response", 
                                                 "affected_catch")]
# colnames(d42)[3] <- "affected_catch"

n1 <- cbind(d42[,c("fishing_entity", "response")], dataa2[,"affected_catch"])
n2 <- cbind(dataa2[,c("fishing_entity", "response")], d42[,"affected_catch"])
d_plot <- rbind(n1, n2)

png(file = "paper_figures/figure_2b_alternative2.png", 
    width = 11, height = 7, units = 'in', res = 600)
ggplot(d_plot, aes(fishing_entity, affected_catch, fill = response)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  
  scale_fill_manual(values = c("hotpink", "limegreen", "grey"), name = "Affected catch?",
                    labels = c("Latitude", "Latitude & Depth","Non-assessed")) +
  labs(x = "Fishing entity", y = "Total catch (t)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 20))
dev.off()

#FIGURE5 (OLD)
png(file = "paper_figures/figure_5.png", 
    width = 20, height = 7, units = 'in', res = 600)
ggplot(all2, aes(x = area_name, y = fishing_entity, fill = catchdepFEEZ)) +
  geom_tile() + #data = subset(data, !is.na(fishing_entity))
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 11)) +
  scale_fill_gradientn("Catch dependency\non EEZ (%)", 
                       colours = myP) + #c("blue","white","red")
  xlab("Economic Exclusive Zones") + ylab("Fishing entities") 
dev.off()

#FIGURE5 (NEW)
df2 <- left_join(all2, d_area, by = c("fishing_entity", "area_name"))

#Same names in "fishing_entity" and "area_name"
df2$area_name_simpl <- df2$area_name
df2$area_name_simpl[grep("USA", df2$area_name_simpl)] <- "USA"
df2$area_name_simpl[grep("Denmark", df2$area_name_simpl)] <- "Denmark"
df2$area_name_simpl[grep("Germany", df2$area_name_simpl)] <- "Germany"
df2$area_name_simpl[grep("Sweden", df2$area_name_simpl)] <- "Sweden"
df2$area_name_simpl[grep("UK", df2$area_name_simpl)] <- "United Kingdom"
df2$area_name_simpl[grep("Japan", df2$area_name_simpl)] <- "Japan"
df2$area_name_simpl[grep("Canada", df2$area_name_simpl)] <- "Canada"
df2$area_name_simpl[grep("Spain", df2$area_name_simpl)] <- "Spain"
df2$area_name_simpl[grep("France", df2$area_name_simpl)] <- "France"
df2$area_name_simpl[grep("Africa", df2$area_name_simpl)] <- "South Africa"
df2$area_name_simpl[df2$area_name_simpl == "Azores Isl. (Portugal)"] <- "Portugal"
df2$area_name_simpl[grep("Portugal", df2$area_name_simpl)] <- "Portugal"
df2$fishing_entity[df2$fishing_entity == "Azores Isl"] <- "Portugal"
df2$fishing_entity[df2$fishing_entity == "Saint Pierre & Miquelon (France)"] <- "France"
unique(df2$area_name_simpl) %in% unique(df2$fishing_entity)

#add own/other EEZ var
df2$in_out <- "Other EEZ"
df2 <- as.data.frame(df2)
for (i in 1:dim(df2)[1]) {
  if(identical(df2[i,colnames(df2) == "fishing_entity"], df2[i,colnames(df2) == "area_name_simpl"])==T) {
    df2$in_out[i] <- "Own EEZ"
  }
}

dataa3 <- df2 %>%
            group_by(fishing_entity, in_out) %>%
            summarise(eez_number = n(),
                      affected_catch = sum(affected_catch.x, na.rm = T),
                      eez_total_catch = sum(tonnes, na.rm = T))

dataa3 <- melt(dataa3, id.vars = c("fishing_entity", "in_out", "eez_number"))
dataa3 <- filter(dataa3, !fishing_entity == "St Pierre & Miquelon (Fr)")

ggplot(dataa3, aes(fct_rev(fishing_entity), value, fill = variable)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("black", "grey"), name = "Affected catch?",
                    labels = c("Yes", "Non-assessed")) +
  # geom_text(data = subset(dataa3, variable == "affected_catch"),
  #           aes(fishing_entity, value, label = eez_number), size = 4, hjust = -0.2) +
  facet_grid(~ in_out, space = "free") + #scales="free", 
  coord_flip()+
  labs(x = "Fishing entity", y = "Total catch (t)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 20),
        strip.text.x = element_text(size = 14)) 

                 
####FINAL TABLE of most vulnerable countries

##Latitude
LatitudeTable <- subset(latitude, latitude$catchdepFE>0.05)
colnames(LatitudeTable)
LatitudeTable <- LatitudeTable[ , c(3, 2, 8, 1, 31)]

write.csv(LatitudeTable, "paper_tables/latitudetable.csv")

##Depth
DepthTable <- subset(depth, depth$catchdepFE>0.04)
colnames(DepthTable)
DepthTable <- DepthTable[ , c(3, 2, 8, 1, 31)]

write.csv(DepthTable, "paper_tables/depthtable.csv")





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
