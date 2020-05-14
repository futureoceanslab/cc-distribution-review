##################################################################
##### This script produces socioeconomic fugures for the manuscript.
##### 01/03/2019
##### INPUT FILES: biblio_database_full.csv
##### OUTPUT FILES: manuscipt figures XXXXXXXXXXXX
##################################################################

library(RColorBrewer)#brewer.pal
library(tidyverse)
library(ggrepel)#geom_text_repel
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

remove2 <- filter(data_original, is.na(fishing_entity) == T)
rm_id <- remove2$ID
data <- filter(data, !ID %in% rm_id)
length(unique(data$scientific_name)) #
#we loose a lot of species again!!

# Subsets of the impacts depending on the response type lat or depth
data$response <- as.factor(data$response)
levels(data$response)
latitude <- subset (data, response == "latitude")
depth <- subset (data, response == "depth")

#Color palette
myP <- rev(brewer.pal(n = 8, name = 'RdBu'))

#####2. FIGURE 3: RELATIONAL DEPENDENCY between FISHING ENTITIES AND EEZ (T AND $) ####
##plot catch dependency
P1<- ggplot(data, aes(x = area_name, y = fishing_entity, fill = catchdepFEEZ)) +
  geom_tile(data = subset(data, !is.na(fishing_entity))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 11),
        axis.text.y = element_text(size = 11),
        title = element_text(size = 13),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 11)) +
  scale_fill_gradientn("Catch dependency\non EEZ (%)", 
                       colours = myP) + #c("blue","white","red")
  xlab("Economic Exclusive Zones") + ylab("Fishing entities") +
  ggtitle("a)")
P1

##plot $ dependency
P2<- ggplot(data, aes(x = area_name, y = fishing_entity, fill = landedvalueFEEZ/landedvalueFE)) +
  geom_tile(data = subset(data, !is.na(fishing_entity))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 11),
        axis.text.y = element_text(size = 11),
        title = element_text(size = 13),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 11)) +
  scale_fill_gradientn("Value dependency\non EEZ (%)", 
                       colours = myP) + #c("blue","white","red")
  xlab("Economic Exclusive Zones") + ylab("Fishing entities") +
  ggtitle("b)")
P2
## If we want to modify scale, code here: ", limits=c(0,1250000), breaks=seq(0,1250000, by=1000000)"

##PAPER Fig3 
png(file = "paper_figures/figure_3ab.png", 
    width = 16, height = 7, units = 'in', res = 600)
F3 <- multiplot(P1, P2, cols = 2)
dev.off()


# REMOVE OUTLIERS in impact. remove decadal_change>300, one observation
# range(data$decadal_change)
# plot(data$decadal_change, data$id_obs) #to check for outliers
# data <- subset(data, data$decadal_change < 300)

#####3. FIGURE 4:
#LAT
P3 <- ggplot(latitude, aes(catchdepFE*100, decadal_change, label = scientific_name)) +
        geom_point(aes(color = (landedvalueFEsp/landedvalueFE)*100, alpha = 0.6), size = 5) +
        scale_colour_gradientn(colours = myP, name = "Value dependency\non species (%)") +
        theme(axis.text.x = element_text(angle = -45, hjust = 0.06, size = 10),
              axis.text.y = element_text(size = 10),
              panel.background = element_rect(fill = "white"),
              axis.line.x = element_line(colour = c("black")),
              axis.line.y = element_line(colour = c("black")),
              legend.key=element_blank(),
              axis.title.y = element_text(size = 14),
              axis.title.x = element_text(size = 14)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
        geom_text_repel(data = subset(latitude, catchdepFE*100 > 6), 
                        aes(color = (landedvalueFEsp/landedvalueFE)*100), size = 3,
                        vjust = -2.5, hjust = "inward", force = 3) +
        guides(size = F,
               alpha = F) +
        labs(x = "Catch dependency on species (%)",
             y = "Latitude shift (km)") +
        facet_wrap(~ fishing_entity)

#DEPTH
P4 <- ggplot(depth, aes(catchdepFE*100, decadal_change, label = scientific_name)) +
        geom_point(aes(color = (landedvalueFEsp/landedvalueFE)*100, alpha = 0.6), size = 5) +
        scale_colour_gradientn(colours = myP, name = "Value dependency\non species (%)") +
        theme(axis.text.x = element_text(angle = -45, hjust = 0.06, size = 10),
              axis.text.y = element_text(size = 10),
              panel.background = element_rect(fill = "white"),
              axis.line.x = element_line(colour = c("black")),
              axis.line.y = element_line(colour = c("black")),
              legend.key=element_blank(),
              axis.title.y = element_text(size = 14),
              axis.title.x = element_text(size = 14)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
        geom_text_repel(data = subset(depth, catchdepFE*100 > 6), 
                        aes(color = (landedvalueFEsp/landedvalueFE)*100), size = 3,
                        vjust = -2.5, hjust = "inward", force = 3) +
        guides(size = F,
               alpha = F) +
        labs(x = "Catch dependency on species (%)",
             y = "Depth shift (m)") +
        facet_wrap(~ fishing_entity)

##Paper Fig 4. IMpact and Catch dependency
png(file = "paper_figures/figure_4.png", 
 width = 12, height = 7, units = 'in', res = 600)
 P3
dev.off()

png(file = "paper_figures/figure_5.png", 
 width = 12, height = 7, units = 'in', res = 600)
 P4
dev.off()



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
