
##Author: Diego Salgueiro Otero
##Data: 15/01/2017
##Title: The quality of methods for the review-paper

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

##Makeing a specific subset to plot the specific varibales
mybiblio_database<-biblio_database[,c(2, 3,4, 6, 8,32,33,59, 63)]
str(mybiblio_database)
mybiblio_database<- as.data.frame(mybiblio_database)

## remove Nicholas (because is not only marine)
mybiblio_database<- mybiblio_database[-c(166:175), ]
mybiblio_database

##order in time 
mybiblio_database<-mybiblio_database[order(mybiblio_database$article_year),]

##creating new column
mybiblio_database$observations<-1 

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

mybiblio_database$statistical_methodology<- factor( mybiblio_database$statistical_methodology, levels=c("1","2","5","23"),
                                                    labels=c("Lineal correlation","Lineal regresion","Indirect relation", "Lineal regresion and Spatial analysis"))

##1st graph "observations-duration of studies-statistical methodology"
ggplot(data = mybiblio_database, aes(x = categoriesbyyears, y = observations, fill = statistical_methodology)) + 
  geom_bar(stat="identity", alpha=0.8) + scale_fill_manual(values=c("darkgreen", "yellow", "orange", "orangered")) +
  labs(title="Quality of data-source", subtitle="496 obs.", y = "Number of observations", x = "Duration of the studies (Years)", fill = "Statistical methodology", caption="Review Future Oceans Lab")+ 
  theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'), legend.position= "left")

## Plotting a table including following variables: author, article_year, b_years, b_years_patern, data_type and statistical_methodology 
#creating a new subset
mytable<-mybiblio_database[,c(3,4,6,7,8,9)]
mytable

data<-mytable
maxrow <-35
npages <- ceiling(nrow(data)/maxrow)
pdf("DT1.pdf", height = 11, width = 8.5)
idx <- seq(1,maxrow)
grid.table(data[idx,], rows = NULL)
for(i in 2:npages){
  grid.newpage()
  if(i*maxrow <= nrow(data)){
    idx <- seq(1+((i-1)*maxrow), i*maxrow)
  }else{
    idx <- seq(1+((i-1)*maxrow), nrow(data))
  }
  grid.table(data[idx,], rows = NULL)
}
dev.off()


##-----------------------

###### characteristics
names(mybiblio_database)
dim( mybiblio_database)
head( mybiblio_database)
tail ( mybiblio_database)
str( mybiblio_database)
summary( mybiblio_database)



