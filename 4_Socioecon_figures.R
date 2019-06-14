##R code for figures
##reference: FOL
##Date: March, 1st 2019

##calling libraries
library(stats)
library(base)
library(RColorBrewer)
library(tidyverse)
library(gridExtra)
library(data.table)
library(grid)
library(ggrepel)
source("function_multiplot.R")

#Open Biblio_data with SAU data on EEZ and FE:
data <- read.csv("data/biblio_database_full.csv")

##PREPARETION OF VARIABLES: Subsets of the impacts
data$response <- as.factor(data$response)
levels(data$response)

latitude <- subset (data, response == "latitude")
depth <- subset (data, response == "depth")

######### FIGURE 3: RELATIONAL DEPENDENCY between FISHING ENTITIES AND EEZ (T AND $) #####################
##plot catch dependency
P1<- ggplot(data, aes(x = area_name, y = fishing_entity, fill = catchdepFEEZ)) +
  geom_tile(data = subset(data, !is.na(fishing_entity))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_gradientn("Dependency (t)", colours = rev(brewer.pal(9, "Spectral"))) +
  xlab("Economic Exclusive Zones") + ylab("Fishing entities") +
  ggtitle("a)")
P1

##plot $ dependency
P2<- ggplot(data, aes(x = area_name, y = fishing_entity, fill = landedvalueFEEZ/landedvalueFE)) +
  geom_tile(data = subset(data, !is.na(fishing_entity))) +
  theme_bw() +
  theme( axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_gradientn("Dependency ($)", colours = rev(brewer.pal(9, "Spectral"))) +
  xlab("Economic Exclusive Zones") + ylab("Fishing entities") +
  ggtitle("b)")
P2
## If we want to modify scale, code here: ", limits=c(0,1250000), breaks=seq(0,1250000, by=1000000)"

##PAPER Fig3 
png(file = "paper_figures/figure_3ab.png", 
    width = 16, height = 7, units = 'in', res = 600)
multiplot(P1, P2, cols = 2)
dev.off()


###FIGURE FISHING ENTITIES CATCH DEPENDENCY


##LATITUDE

P13 <- ggplot(latitude, aes(catchdepFE, decadal_change, colour=decadal_change, label = scientific_name)) +
  geom_point(aes(colour = decadal_change, size = tonnesEEZsp), alpha = 0.4) +
  geom_text_repel(data=subset(latitude, latitude$catchdepFE>0.07), aes(color=decadal_change), size=3, 
                  vjust=1) +
  scale_color_gradient("km/decade", low = "blue", high = "red")+
  ylim(-100, 450)+
  scale_size_continuous("Species landings in EEZ (t/y)", range=c(3,20),breaks=c(250000, 500000, 750000, 1000000))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        #legend.key=element_blank(),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  labs(x = "FE species catch dependency on EEZ",
       y = "latitude shift")
P13

##DEPTH


P14 <- ggplot(depth, aes(catchdepFE, decadal_change, colour=decadal_change, label = scientific_name)) +
  geom_point(aes(colour = decadal_change, size = tonnesEEZsp), alpha = 0.4) +
  geom_text_repel(data=subset(depth, depth$catchdepFE>0.10), aes(color=decadal_change), size=3, 
                  vjust=1) +
  scale_color_gradient("km/decade", low = "blue", high = "red")+
  ylim(-50, 100)+
  scale_size_continuous("Species landings in EEZ (t/y)", range=c(3,20),breaks=c(250000, 500000, 750000, 1000000))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        #legend.key=element_blank(),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  labs(x = "FE species catch dependency on EEZ",
       y = "depth shift")
P14


##Paper Fig 5. Catch dependency fishing entities

png(file = "paper_figures/figure_3ab.png", 
    width = 16, height = 7, units = 'in', res = 600)
a <- multiplot(P13, P14, cols = 2)
dev.off()

##text con fishing entities
##graphs con values economic
###ZOOM ON FIGURE FISHING ENTITIES CATCH DEPENDENCY

##LATITUDE ZOOM

P15 <- ggplot(latitude, aes(catchdepFE, decadal_change, colour=decadal_change, label = scientific_name)) +
  geom_point(aes(colour = decadal_change, size = tonnesEEZsp), alpha = 0.4) +
  geom_text_repel(data=subset(latitude, latitude$catchdepFE>0.04), aes(color=decadal_change), size=3, 
                  vjust=1) +
  scale_color_gradient("km/decade", low = "blue", high = "red")+
  ylim(-100, 450)+
  xlim(0,0.07)+
  scale_size_continuous("Species landings in EEZ (t/y)", range=c(3,20),breaks=c(250000, 500000, 750000, 1000000))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        #legend.key=element_blank(),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  labs(x = "FE species catch dependency on EEZ",
       y = "latitude shift")
P15

##DEPTH


P16 <- ggplot(depth, aes(catchdepFE, decadal_change, colour=decadal_change, label = scientific_name)) +
  geom_point(aes(colour = decadal_change, size = tonnesEEZsp), alpha = 0.4) +
  geom_text_repel(data=subset(depth, depth$catchdepFE>0.04), aes(color=decadal_change), size=3, 
                  vjust=1) +
  scale_color_gradient("km/decade", low = "blue", high = "red")+
  ylim(-50, 100)+
  xlim(0, 0.10)+
  scale_size_continuous("Species landings in EEZ (t/y)", range=c(3,20),breaks=c(250000, 500000, 750000, 1000000))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = c("black")),
        axis.line.y = element_line(colour = c("black")),
        #legend.key=element_blank(),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  labs(x = "FE species catch dependency on EEZ",
       y = "depth shift")
P16







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
