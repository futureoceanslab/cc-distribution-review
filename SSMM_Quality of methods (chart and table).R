
##Author: Diego Salgueiro Otero
##Data: 11/05/2018
##Title: Script for methods quality for the review paper

#Installing packages
install.packages("gridExtra")
install.packages("data.table")

#calling libraries
library(ggplot2)
library(plotly)
library(networkD3)
library(gridExtra)
library(data.table)
library(grid)
library(shiny)

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
dim( mybiblio_database)
head( mybiblio_database)
tail ( mybiblio_database)
str( mybiblio_database)
summary( mybiblio_database)

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
