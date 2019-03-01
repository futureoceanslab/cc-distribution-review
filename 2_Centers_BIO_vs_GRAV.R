#################################
##### FOL
##### 28/02/2019
#################################

library(rockchalk)
library(tidyverse)

##############
# Statistics #
##############
#depth and area are all observations from center of biomass

#to date boundary latitude only 1 obs for biomass so no suidata for a statistical test

#lat+long, long not done

data <- read.table("data/biblio_databse1.csv", header = T, sep = ",")

#FOR LATITUDE: 
data$b_impact_i <- as.factor(data$b_impact_i)
data$b_years <- as.numeric(as.character(data$b_years))

latitude1 <- subset(data, b_impact_i == "1", select = c(b_value, b_impact_i)) #center of grav
latitude2 <- subset(data, b_impact_i == "2", select = c(b_value, b_impact_i)) #center of bio
latitude <- rbind(latitude1,latitude2)
latitude$b_impact_i <- factor(latitude$b_impact_i)

tapply(latitude$b_value, latitude$b_impact_i, mean)
tapply(latitude$b_value, latitude$b_impact_i, var)

boxplot(latitude$b_value ~ latitude$b_impact_i)

#Normality and Homocedasticity
hist(latitude$b_value) #Kinda of N distribution
hist(log(latitude$b_value)) #More N distribution

tapply(latitude$b_value, latitude$b_impact_i, shapiro.test) # NO NORMAL DISTRI
bartlett.test(latitude$b_value, latitude$b_impact_i) #HOMOCEDASTICITY 

boxplot(latitude$b_value ~ latitude$b_impact_i)
boxplot(log(latitude$b_value + 1) ~ latitude$b_impact_i)

#NO PARAMETRIC TEST - not very strong test
kruskal.test(b_value ~ b_impact_i, data = latitude) # < 0.05

#RUNNING ANOVAS NO TRANS AND LOG TRANS
m1 <- aov(latitude$b_value ~ latitude$b_impact_i)
b_value_transformed <- log(102 + latitude$b_value) #get rid of negative values we add a constant

m2<-aov(b_value_transformed ~ latitude$b_impact_i)

summary(m1)
summary(m2) #No significant differences. OK to combine them


#############
#COMBINATION
#############
#new colums with center of grav combined with center of biomass
data["b_impact_combine"] <- data$b_impact
data$b_impact_combine <- combineLevels(data$b_impact_combine, levs = c("lat shift center of grav", "lat shift center of bio"), newLabel = c("lat shift"))
data$b_impact_combine <- combineLevels(data$b_impact_combine, levs = c("boundary lat shift center of grav", "boundary lat shift center of bio"), newLabel = c("boundary lat shift"))
data$b_impact_combine <- combineLevels(data$b_impact_combine, levs = c("long shift center of grav", "long shift center of bio"), newLabel = c("long shift"))
levels(data$b_impact_combine)[levels(data$b_impact_combine) == "lat and long shift center of grav"] <- "lat and long shift"

data["b_direction_combine"] <- data$b_direction
data$b_direction_combine <- combineLevels(data$b_direction_combine, levs = c("lat shift north center of grav", "lat shift north center of bio"), newLabel = c("lat shift north"))
data$b_direction_combine <- combineLevels(data$b_direction_combine, levs = c("lat shift south center of grav", "lat shift south center of bio"), newLabel = c("lat shift south"))
data$b_direction_combine <- combineLevels(data$b_direction_combine, levs = c("boundary lat shift north center of grav", "boundary lat shift north center of bio"), newLabel = c("boundary lat shift north"))
levels(data$b_direction_combine)[levels(data$b_direction_combine) == "max lat shift north center of grav"] <- "max lat shift north"
levels(data$b_direction_combine)[levels(data$b_direction_combine) == "max lat shift south center of grav"] <- "max lat shift south"
levels(data$b_direction_combine)[levels(data$b_direction_combine) == "min lat shift north center of grav"] <- "min lat shift north"
levels(data$b_direction_combine)[levels(data$b_direction_combine) == "shift southweastwards center of grav"] <- "shift southweastwards"
levels(data$b_direction_combine)[levels(data$b_direction_combine) == "shift northeastwards center of grav"] <- "shift northeastwards"
levels(data$b_direction_combine)[levels(data$b_direction_combine) == "shift westwards center of grav"] <- "shift westwards"
levels(data$b_direction_combine)[levels(data$b_direction_combine) == "boundary lat shift east center of bio"] <- "boundary lat shift east"





#####NEW DATABASE
write.csv(data, "data/biblio_database2.csv")
