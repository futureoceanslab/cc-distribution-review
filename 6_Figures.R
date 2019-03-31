##R code for figures
##Author: FOL
##Date: March, 1st 2019

##calling libraries
library(dplyr)
library(stats)
library(base)
library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(gridExtra)
library(data.table)
library(grid)
library(ggrepel)

#Open Biblio_data with SAU data on EEZ and FE:
data <- read.csv("data/biblio_database_full.csv", stringsAsFactors = FALSE)

##PREPARETION OF VARIABLES: Subsets of the impacts
data$b_impact_combine <- as.factor(data$b_impact_combine)
levels(data$b_impact_combine)

latitude <- subset (data, b_impact_combine =="lat shift")
depth <- subset (data, b_impact_combine=="depth shift")
long <- subset (data, b_impact_combine=="long shift")
lat_long <- subset (data, b_impact_combine=="lat and long shift")

area <- subset (data, b_impact_combine == "shift in area occupied")

lat_range    <- subset (data, b_impact_combine== "latitude range shift")
depth_range    <- subset (data, b_impact_combine== "depth range shift")

lat_boundary    <- subset (data, b_impact_combine== "boundary lat shift")


########### FIGURES 3 and 4: CATCH and PRICE CATEGORY IMPACTS ###########
####LATITUDE
latitude$area_name <- as.factor(latitude$area_name)

#Impacts by EEZ and Landings (Catch t)
seq(min(latitude$tonnesEEZsp, na.rm=T),max(latitude$tonnesEEZsp, na.rm=T), by = 250000)

P1<- ggplot(latitude, aes(area_name, b_value)) +
  geom_point(aes(color = tonnesEEZsp, size = tonnesEEZsp), alpha = 0.6) +
  scale_colour_gradient(low="blue", high="red",guide="legend", 
                        breaks = c(0.5e+5,5.0e+5, 7.5e+5, 1.0e+6, 1.3e+6))+
  scale_size(range=c(3,15), breaks = c(0.5e+5,5.0e+5, 7.5e+5, 1.0e+6, 1.3e+6))+
  theme(axis.text.x = element_text(angle = -45, hjust = 0.06, size = 12),
        axis.text.y = element_text(size = 12),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        legend.key=element_blank(),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  guides(color = guide_legend(title = "Catch (t)"),
         size = guide_legend(title = "Catch (t)")) +
  labs(x = "Economic Exclusive Zones",
       y = "Latitudinal shift (km/decade)")
P1

#Impacts by EEZ and Landings (Landings $)
seq(min(latitude$landedvalueEEZsp, na.rm=T),max(latitude$landedvalueEEZsp, na.rm=T), by = 250000000)

P2<- ggplot(latitude, aes(area_name, b_value)) +
  geom_point(aes(color = landedvalueEEZsp, size = landedvalueEEZsp), alpha = 0.6) +
  scale_colour_gradient(low="blue", high="red",guide="legend", 
                        breaks = c(0.5e+8,5.0e+8, 1.0e+9, 1.3e+9))+
  scale_size(range=c(3,15), breaks = c(0.5e+8,5.0e+8, 1.0e+9, 1.3e+9))+
  theme(axis.text.x = element_text(angle = -45, hjust = 0.06, size = 12),
        axis.text.y = element_text(size = 12),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        legend.key=element_blank(),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  guides(color = guide_legend(title = "Landings ($)"), 
         size = guide_legend(title = "Landings ($)")) +
  labs(x = "Economic Exclusive Zones",
       y = "Latitudinal shift (km/decade)")
P2

#Impacts by Price category and Catch (Catch t)
P3<- ggplot(latitude, aes(PriceCateg, b_value)) +
  geom_point(aes(color = tonnesEEZsp, size = tonnesEEZsp), alpha = 0.6) +
  scale_colour_gradient(low="blue", high="red",guide="legend", 
                        breaks = c(0.5e+5,5.0e+5, 7.5e+5, 1.0e+6, 1.3e+6))+
  scale_size(range=c(3,15), breaks = c(0.5e+5,5.0e+5, 7.5e+5, 1.0e+6, 1.3e+6))+
  theme(axis.text.x = element_text(angle = -45, hjust = 0.06, size = 12),
        axis.text.y = element_text(size = 12),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        legend.key=element_blank(),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  guides(color = guide_legend(title = "Catch (t)"),
         size = guide_legend(title = "Catch (t)")) +
  labs(x = "Price Category",
       y = "Latitudinal shift (km/decade)") +
  scale_x_discrete(limits = c("low","medium", "high","very high"))
P3

#Impacts by Price Category and Landings (Landings $)
P4<- ggplot(latitude, aes(PriceCateg, b_value)) +
  geom_point(aes(color = landedvalueEEZsp, size = landedvalueEEZsp), alpha = 0.6) +
  scale_colour_gradient(low="blue", high="red",guide="legend", 
                        breaks = c(1.3e+6,7.5e+8, 1.0e+9, 1.25e+9, 1.5e+9))+
  scale_size(range=c(3,15), breaks = c(1.3e+6,7.5e+8, 1.0e+9, 1.25e+9, 1.5e+9))+
  theme(axis.text.x = element_text(angle = -45, hjust = 0.06, size = 12),
        axis.text.y = element_text(size = 12),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        legend.key=element_blank(),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  guides(color = guide_legend(title = "Landings ($)"), 
         size = guide_legend(title = "Landings ($)")) +
  labs(x = "Price Category",
       y = "Latitudinal shift (km/decade)") +
  scale_x_discrete(limits = c("low","medium", "high","very high"))
P4

####DEPTH
depth$area_name <- as.factor(depth$area_name)

#Impacts by EEZ and Catch (Catch t)
seq(min(depth$tonnesEEZsp, na.rm=T),max(depth$tonnesEEZsp, na.rm=T), by = 250000)

P5<- ggplot(depth, aes(area_name, b_value)) +
  geom_point(aes(color = tonnesEEZsp, size = tonnesEEZsp), alpha = 0.6) +
  scale_colour_gradient(low="blue", high="red",guide="legend", 
                        breaks = c(0.5e+5,5.0e+5, 7.5e+5, 1.0e+6, 1.3e+6))+
  scale_size(range=c(3,15), breaks = c(0.5e+5,5.0e+5, 7.5e+5, 1.0e+6, 1.3e+6))+
  theme(axis.text.x = element_text(angle = -45, hjust = 0.06, size = 12),
        axis.text.y = element_text(size = 12),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        legend.key=element_blank(),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  guides(color = guide_legend(title = "Catch (t)"),
         size = guide_legend(title = "Catch (t)")) +
  labs(x = "Economic Exclusive Zones",
       y = "Depth shift (m/decade)")
P5

#Impacts by EEZ and Landings (Landings $)
seq(min(depth$landedvalueEEZsp, na.rm=T),max(depth$landedvalueEEZsp, na.rm=T), by = 250000000)

P6<- ggplot(depth, aes(area_name, b_value)) +
  geom_point(aes(color = landedvalueEEZsp, size = landedvalueEEZsp), alpha = 0.6) +
  scale_colour_gradient(low="blue", high="red",guide="legend", 
                        breaks = c(0.5e+8,5.0e+8, 1.0e+9, 1.3e+9))+
  scale_size(range=c(3,15), breaks = c(0.5e+8,5.0e+8, 1.0e+9, 1.3e+9))+
  theme(axis.text.x = element_text(angle = -45, hjust = 0.06, size = 12),
        axis.text.y = element_text(size = 12),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        legend.key=element_blank(),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  guides(color = guide_legend(title = "Landings ($)"), 
         size = guide_legend(title = "Landings ($)")) +
  labs(x = "Economic Exclusive Zones",
       y = "Depth shift (m/decade)")
P6

#Impacts by Price Category and Catch (Catch t)
P7<- ggplot(depth, aes(PriceCateg, b_value)) +
  geom_point(aes(color = tonnesEEZsp, size = tonnesEEZsp), alpha = 0.6) +
  scale_colour_gradient(low="blue", high="red",guide="legend", 
                        breaks = c(0.5e+5,5.0e+5, 7.5e+5, 1.0e+6, 1.3e+6))+
  scale_size(range=c(3,15), breaks = c(0.5e+5,5.0e+5, 7.5e+5, 1.0e+6, 1.3e+6))+
  theme(axis.text.x = element_text(angle = -45, hjust = 0.06, size = 12),
        axis.text.y = element_text(size = 12),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        legend.key=element_blank(),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  guides(color = guide_legend(title = "Catch (t)"),
         size = guide_legend(title = "Catch (t)")) +
  labs(x = "Price Category",
       y = "Depth shift (m/decade)") +
  scale_x_discrete(limits = c("low","medium", "high","very high"))
P7

#Impacts by Price Category and Landings (Landings $)
P8<- ggplot(depth, aes(PriceCateg, b_value)) +
  geom_point(aes(color = landedvalueEEZsp, size = landedvalueEEZsp), alpha = 0.6) +
  scale_colour_gradient(low="blue", high="red",guide="legend", 
                        breaks = c(1.3e+6,7.5e+8, 1.0e+9, 1.25e+9, 1.5e+9))+
  scale_size(range=c(3,15), breaks = c(1.3e+6,7.5e+8, 1.0e+9, 1.25e+9, 1.5e+9))+
  theme(axis.text.x = element_text(angle = -45, hjust = 0.06, size = 12),
        axis.text.y = element_text(size = 12),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        legend.key=element_blank(),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  guides(color = guide_legend(title = "Landings ($)"), 
         size = guide_legend(title = "Landings ($)")) +
  labs(x = "Price Category",
       y = "Depth shift (m/decade)") +
  scale_x_discrete(limits = c("low","medium", "high","very high"))
P8

##PAPER Fig.3
grid.arrange(P1, P2, P5, P6, nrow = 2, ncol=2)
##PAPER Fig.4
grid.arrange(P3,P4,P7,P8, nrow = 2, ncol=2)



######### FIGURE 5: IMPACTS AND FE DEPENDENCY ON SHIFTING STOCKS #################

##PAPER Fig.5
grid.arrange(P9, P10, nrow = 1, ncol=2)



######### FIGURE 6: RELATIONAL DEPENDENCY between FISHING ENTITIES AND EEZ (T AND $) #####################

##CREATION OF VARIABLES for DEPEDENCE
# A) CATCH DEPENDENCY OF FISHING ENTITIES
# B) VALUE OF SPECIES FOR FISHING ENTITIES
# C) CATCH PRESSURE IN EEZ

# A) CATCH DEPENDENCY OF FISHING ENTITIES
#1. Species catch dependency on the area
data$catchdepFEsp <- data$tonnesFEsp/data$tonnesFEspT #the dependence of the country species catches on the EEZ species catches
data$landdepFEsp <-  data$landedvalueFEsp/data$landedvalueFEspT #the dependence of the country total SP Value on the EEZ SP catch value 
range(data$catchdepFEsp, na.rm=TRUE)
range(data$landdepFEsp, na.rm=TRUE) #is the same relation
#2. Country dependency on the species in the area
data$catchdepFE <- data$tonnesFEsp/data$tonnesFE #the dependence of the country species catches on the EEZ species catches
range(data$catchdepFE, na.rm=TRUE)
#3. Country dependency on the area
data$catchdepFEEZ <- data$tonnesFEEZ/data$tonnesEEZ 
range(data$catchdepFEEZ, na.rm=TRUE)
a<-filter(data,catchdepFEEZ>1)

# B) VALUE OF SPECIES FOR FISHING ENTITIES
###Species VAlue in FE: landed value/tonnes
data$spvalueFE  <- data$landedvalueFEsp/data$tonnesFEsp
range(data$spvalueFE, na.rm=TRUE)
quantile(data$spvalueFE, na.rm=TRUE)

# C) CATCH PRESSURE IN EEZ
data$catchpresEEZsp <-data$tonnesEEZsp/data$tonnesEEZ
data$landpresEEZsp <- data$landedvalueEEZsp/data$landedvalueEEZ
range(data$catchpresEEZsp, na.rm=TRUE)

##plot catch dependency
P11<- ggplot(data, aes(x = area_name, y = fishing_entity, fill = catchdepFEEZ)) +
  geom_tile(data = subset(data, !is.na(fishing_entity))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_gradientn("Dependency (t)", colours = rev(brewer.pal(9, "Spectral"))) +
  xlab("Economic Exclusive Zones") + ylab("Fishing entities")
P11

##plot $ dependency
P12<- ggplot(data, aes(x = area_name, y = fishing_entity, fill = landedvalueFEEZ/landedvalueFE)) +
  geom_tile(data = subset(data, !is.na(fishing_entity))) +
  theme_bw() +
  theme( axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_gradientn("Dependency ($)", colours = rev(brewer.pal(9, "Spectral"))) +
  xlab("Economic Exclusive Zones") + ylab("Fishing entities")
P12
## If we want to modify scale, code here: ", limits=c(0,1250000), breaks=seq(0,1250000, by=1000000)"

##PAPER Fig.6 
grid.arrange(P11,P12, nrow = 1, ncol=2)


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
mybiblio_database$categoriesbyyears[mybiblio_database$b_years==10]<-"0-25"
mybiblio_database$categoriesbyyears[mybiblio_database$b_years==15]<-"0-25"
mybiblio_database$categoriesbyyears[mybiblio_database$b_years==17]<-"0-25"
mybiblio_database$categoriesbyyears[mybiblio_database$b_years==23]<-"0-25"
mybiblio_database$categoriesbyyears[mybiblio_database$b_years==24]<-"0-25"
mybiblio_database$categoriesbyyears[mybiblio_database$b_years==25]<-"0-25"
mybiblio_database$categoriesbyyears[mybiblio_database$b_years==27]<-"26-35"
mybiblio_database$categoriesbyyears[mybiblio_database$b_years==28]<-"26-35"
mybiblio_database$categoriesbyyears[mybiblio_database$b_years==30]<-"26-35"
mybiblio_database$categoriesbyyears[mybiblio_database$b_years==31]<-"26-35"
mybiblio_database$categoriesbyyears[mybiblio_database$b_years==35]<-"26-35"
mybiblio_database$categoriesbyyears[mybiblio_database$b_years==38]<-"35-45"
mybiblio_database$categoriesbyyears[mybiblio_database$b_years==39]<-"35-45"
mybiblio_database$categoriesbyyears[mybiblio_database$b_years==40]<-"35-45"
mybiblio_database$categoriesbyyears[mybiblio_database$b_years==41]<-"35-45"
mybiblio_database$categoriesbyyears[mybiblio_database$b_years==42]<-"35-45"
mybiblio_database$categoriesbyyears[mybiblio_database$b_years==75]<-"75-80"
mybiblio_database$categoriesbyyears[mybiblio_database$b_years==77]<-"75-80"
mybiblio_database$categoriesbyyears[mybiblio_database$b_years==78]<-"75-80"
mybiblio_database$categoriesbyyears[mybiblio_database$b_years==80]<-"75-80"

##Re-order the axis from alphabetical to dataframe's order
mybiblio_database$author <- reorder( mybiblio_database$author, mybiblio_database$article_year)

##Tailoring data for next plotting
mybiblio_database$b_years_pattern<- factor( mybiblio_database$b_years_pattern, levels=c("1","2"),
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
## Plotting a table including following variables: author, article_year, b_years, b_years_patern, data_type and statistical_methodology 
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

