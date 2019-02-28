##############
# Statistics #
##############
#depth and area are all observations from center of biomass

#to date boundary latitude only 1 obs for biomass so no suitable for a statistical test

#lat+long, long not done

table<-read.table("data/biblio_database.csv", header= T, sep= ",")

#FOR LATITUDE: 
table$b_impact<-as.factor(table$b_impact)
table$b_years<-as.numeric(as.character(table$b_years))

latitude1<-subset(table, b_impact=="1", select=c(b_value,b_impact)) #center of grav
latitude2<-subset(table, b_impact=="2", select=c(b_value,b_impact)) #center of bio
latitude<-rbind(latitude1,latitude2)
latitude$b_impact<-factor(latitude$b_impact)

tapply(latitude$b_value,latitude$b_impact,mean)
tapply(latitude$b_value,latitude$b_impact,var)

boxplot(latitude$b_value~latitude$b_impact)

#Normality and Homocedasticity
hist(latitude$b_value) #Kinda of N distribution
hist(log(latitude$b_value)) #More N distribution

tapply(latitude$b_value,latitude$b_impact, shapiro.test) # NO NORMAL DISTRI
bartlett.test(latitude$b_value,latitude$b_impact) #HOMOCEDASTICITY 

boxplot(latitude$b_value~latitude$b_impact)
boxplot(log(latitude$b_value+1)~latitude$b_impact)

#NO PARAMETRIC TEST - not very strong test
kruskal.test(b_value~b_impact, data=latitude) # < 0.05

#RUNNING ANOVAS NO TRANS AND LOG TRANS
m1<-aov(latitude$b_value~latitude$b_impact)
b_value_transformed <- log(102 + latitude$b_value) #get rid of negative values we add a constant

m2<-aov(b_value_transformed~latitude$b_impact)

summary(m1)
summary(m2) #No significant differences. OK to combine them


#############
#COMBINATION
#############

#new colums with center of grav combined with center of biomass
table["b_impact_combine"]<-table$b_impact
table$b_impact_combine<-combineLevels(table$b_impact_combine, levs = c("lat shift center of grav", "lat shift center of bio"), newLabel = c("lat shift"))
table$b_impact_combine<-combineLevels(table$b_impact_combine, levs = c("boundary lat shift center of grav", "boundary lat shift center of bio"), newLabel = c("boundary lat shift"))
table$b_impact_combine<-combineLevels(table$b_impact_combine, levs = c("long shift center of grav", "long shift center of bio"), newLabel = c("long shift"))
levels(table$b_impact_combine)[levels(table$b_impact_combine)=="lat and long shift center of grav"] <- "lat and long shift"

table["b_direction_combine"]<-table$b_direction
table$b_direction_combine<-combineLevels(table$b_direction_combine, levs = c("lat shift north center of grav", "lat shift north center of bio"), newLabel = c("lat shift north"))
table$b_direction_combine<-combineLevels(table$b_direction_combine, levs = c("lat shift south center of grav", "lat shift south center of bio"), newLabel = c("lat shift south"))
table$b_direction_combine<-combineLevels(table$b_direction_combine, levs = c("boundary lat shift north center of grav", "boundary lat shift north center of bio"), newLabel= c("boundary lat shift north"))
levels(table$b_direction_combine)[levels(table$b_direction_combine)=="max lat shift north center of grav"] <- "max lat shift north"
levels(table$b_direction_combine)[levels(table$b_direction_combine)=="max lat shift south center of grav"] <- "max lat shift south"
levels(table$b_direction_combine)[levels(table$b_direction_combine)=="min lat shift north center of grav"] <- "min lat shift north"
levels(table$b_direction_combine)[levels(table$b_direction_combine)=="shift southweastwards center of grav"] <- "shift southweastwards"
levels(table$b_direction_combine)[levels(table$b_direction_combine)=="shift northeastwards center of grav"] <- "shift northeastwards"
levels(table$b_direction_combine)[levels(table$b_direction_combine)=="shift westwards center of grav"] <- "shift westwards"
levels(table$b_direction_combine)[levels(table$b_direction_combine)=="boundary lat shift east center of bio"] <- "boundary lat shift east"

#####NEW DATABASE
write.csv(table, "biblio...csv")