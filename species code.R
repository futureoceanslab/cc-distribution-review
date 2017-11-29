##R code for the Price Category, Vulnerability and Commercial Importance at SPECIES level after the integration of Database Review and FISH base
##Author: Diego & Alba
##Date: April 20th 2017

#set working directory
#setwd("/Users/diegosalgueirootero/OneDrive/CLOCK_TEAM/03_FUTURE OCEANS/FO_BIBLIO/FO_DATA_ANALYSIS/R for fishbase review")
setwd("C:/Users/alba.aguion/OneDrive/CLOCK_TEAM/03_FUTURE OCEANS/FO_BIBLIO/FO_DATA_ANALYSIS/R for fishbase review")
#install.packages("rockchalk")
#install.packages("Rmisc")

#libraries
library(rockchalk)
library(ggplot2)
library(Rmisc)

ReviewSp <- read.csv("ReviewDatsp.csv", header=T, sep = ",")
str(ReviewSp, list.len=ncol(ReviewSp))

#Factors
ReviewSp$researcher<-as.factor(ReviewSp$researcher)
ReviewSp$id_study<-as.factor(ReviewSp$id_study)
ReviewSp$cc<-as.factor(ReviewSp$cc)
ReviewSp$b_impact<-as.factor(ReviewSp$b_impact)
ReviewSp$b_direction<-as.factor(ReviewSp$b_direction)
ReviewSp$cc<-as.factor(ReviewSp$cc)

#Numeric
ReviewSp$b_value<-as.numeric(as.character(ReviewSp$b_value), na.omit=TRUE)
ReviewSp$b_years<-as.numeric(as.character(ReviewSp$b_years))

#change codes to a short description; in our review center of grav separated from center of biomass
levels(ReviewSp$b_impact)<-c("lat shift center of grav", #1
                          "lat shift center of bio",  #2
                          "depth shift", #3
                          "boundary lat shift center of grav", #4
                          "boundary lat shift center of bio", #5
                          "long shift center of grav", #7
                          "long shift center of bio", #8
                          "lat and long shift center of grav", #9
                          "shift in area occupied") #11

levels(ReviewSp$b_direction)<-c("lat shift north center of grav", #1
                             "lat shift south center of grav", #2
                             "boundary lat shift north center of grav", #3
                             "max lat shift north center of grav", #4
                             "max lat shift south center of grav",#5
                             "min lat shift north center of grav", #6
                             "shifting deeper", #7
                             "shifting shallower", #8
                             "area expansion", #9
                             "area contraction", #10
                             "lat shift north center of bio", #11
                             "lat shift south center of bio", #12
                             "shift southweastwards center of grav", #13
                             "shift northeastwards center of grav", #14
                             "shift westwards center of grav", #19
                             "boundary lat shift east center of bio", #20
                             "boundary lat shift north center of bio") #21

levels(ReviewSp$cc)<-c("AMO", #10
                    "climate velocity", #11
                    "sst", #2
                    "sst,bt,AMO", #2,3,10
                    "sst,bt", #2,3
                    "bt") #3

#new colums with center of grav combined with center of biomass

ReviewSp["b_impact_combine"]<-ReviewSp$b_impact
ReviewSp$b_impact_combine<-combineLevels(ReviewSp$b_impact_combine, levs = c("lat shift center of grav", "lat shift center of bio"), newLabel = c("lat shift"))
ReviewSp$b_impact_combine<-combineLevels(ReviewSp$b_impact_combine, levs = c("boundary lat shift center of grav", "boundary lat shift center of bio"), newLabel = c("boundary lat shift"))
ReviewSp$b_impact_combine<-combineLevels(ReviewSp$b_impact_combine, levs = c("long shift center of grav", "long shift center of bio"), newLabel = c("long shift"))
levels(ReviewSp$b_impact_combine)[levels(ReviewSp$b_impact_combine)=="lat and long shift center of grav"] <- "lat and long shift"

ReviewSp["b_direction_combine"]<-ReviewSp$b_direction
ReviewSp$b_direction_combine<-combineLevels(ReviewSp$b_direction_combine, levs = c("lat shift north center of grav", "lat shift north center of bio"), newLabel = c("lat shift north"))
ReviewSp$b_direction_combine<-combineLevels(ReviewSp$b_direction_combine, levs = c("lat shift south center of grav", "lat shift south center of bio"), newLabel = c("lat shift south"))
ReviewSp$b_direction_combine<-combineLevels(ReviewSp$b_direction_combine, levs = c("boundary lat shift north center of grav", "boundary lat shift north center of bio"), newLabel= c("boundary lat shift north"))
levels(ReviewSp$b_direction_combine)[levels(ReviewSp$b_direction_combine)=="max lat shift north center of grav"] <- "max lat shift north"
levels(ReviewSp$b_direction_combine)[levels(ReviewSp$b_direction_combine)=="max lat shift south center of grav"] <- "max lat shift south"
levels(ReviewSp$b_direction_combine)[levels(ReviewSp$b_direction_combine)=="min lat shift north center of grav"] <- "min lat shift north"
levels(ReviewSp$b_direction_combine)[levels(ReviewSpe$b_direction_combine)=="shift southweastwards center of grav"] <- "shift southweastwards"
levels(ReviewSp$b_direction_combine)[levels(ReviewSpe$b_direction_combine)=="shift northeastwards center of grav"] <- "shift northeastwards"
levels(ReviewSp$b_direction_combine)[levels(ReviewSp$b_direction_combine)=="shift westwards center of grav"] <- "shift westwards"
levels(ReviewSp$b_direction_combine)[levels(ReviewSp$b_direction_combine)=="boundary lat shift east center of bio"] <- "boundary lat shift east"

#SUBSETS COMBINE
lat<-subset (ReviewSp, b_impact_combine=="lat shift")
depth<-subset (ReviewSp, b_impact_combine=="depth shift")
boundary<-subset (ReviewSp, b_impact_combine== "boundary lat shift")
area<-subset (ReviewSp, b_impact_combine== "shift in area occupied")
lat_long<-subset (ReviewSp, b_impact_combine== "lat and long shift")
long<-subset (ReviewSp, b_impact_combine== "long shift")







##################################################################################
#Exploratory analysis of Price Category, Vulnerability and Commercial Importance #
##################################################################################

# JUST VULNERABILITY SIG RESULTS WHEN ANALYSED WITH "AREA"


################
#Price Category#     Nothing important here - no sig, no interesting results
################

levels(ReviewSp$PriceCateg ) # dont understand why NULL ??
freq <- as.data.frame(table(ReviewSp$PriceCateg)); freq

#Do the shifting stocks have a different price category?
mod1<-aov(b_value~PriceCateg, data=ReviewSp)
summary(mod1) #no sig


#LATITUDE
stripchart(lat$b_value~lat$PriceCateg, vertical=T,pch=20,
           col=c("blue","green", "red", "black"),method="jitter",jitter=0.2,
           ylab="Latitude shift (km/dec)")
mod3<-aov(b_value~PriceCateg, data=lat)
summary(mod3) #no sig difference 


#Do the ones that shift north have a different price category that the ones that shift south?
mod6<- aov(b_value~PriceCateg*b_direction, data=lat)
summary(mod6) #YES for b direction

#Do the ones that shift north/south have a different price category depending on the site?
mod7<- aov(b_value~PriceCateg*b_direction*site, data=lat)
summary(mod7) 

#DEPTH
stripchart(depth$b_value~depth$PriceCateg, vertical=T,pch=20,
           col=c("blue","green", "red", "black"),method="jitter",jitter=0.2,
           ylab="Depth shift (km/dec)")

mod8<-aov(b_value~PriceCateg, data=depth)
summary(mod8) #no sig dif

#Do the ones that go deeper have a different price category that the ones that go shallower?
mod11<- aov(b_value~PriceCateg*b_direction, data=depth)
summary(mod11) #no


mod12<-aov(b_value~PriceCateg*site, data=depth)
summary(mod12) #value of impact is explained by price category and site
#lets try to make a plot
figdat<-summarySE(data=mod12$model,measurevar="b_value",groupvars=c("PriceCateg","site"))
ggplot(data=figdat, aes(x=PriceCateg, y=b_value, fill=site)) +
  geom_bar(stat="identity", position=position_dodge(),colour="black")

#Area occupied
stripchart(area$b_value~area$PriceCateg, vertical=T,pch=20,
           col=c("blue","green", "red", "black"),method="jitter",jitter=0.2,
           ylab="Area occupied (km2/dec)")

mod13<-aov(b_value~PriceCateg, data=area)
summary(mod13) #no sig difference 

#Do the ones that contract have a different price category that the ones that expand?
mod16<- aov(b_value~PriceCateg*b_direction, data=area)
summary(mod16) #no

#Do the ones that contract/expand have a different price category depending on site?
mod17<- aov(b_value~PriceCateg*b_direction*site, data=area) #??
summary(mod17) 
#Are the assumptions met? (no need to since no sig results)
#Normality
shapiro.test(mod$res)
plot(mod,which=2)
#Residuals independent and identically distributed
plot(mod,which=1)
plot(mod,which=3)



###############
#Vulnerability#     Area vs. Vulnerability sig results (mod30)
###############

cor.test(lat$b_value,lat$Vulnerability, method="pearson") #no sig
cor.test(depth$b_value, depth$Vulnerability, method="pearson") #no sig
cor.test(boundary$b_value,boundary$Vulnerability, method="pearson") #no sig
cor.test(area$b_value, area$Vulnerability, method="pearson") #SIGNIFICANT!

#Do the shifting stocks have different vulnerability?
mod18<-aov(b_value~Vulnerability, data=ReviewSp)
summary(mod18) #no sign

#Do the shitfing stocks have different vulnerability depending on the site? (TWO way ANOVA: two categorical)
mod19<-aov(b_value~Vulnerability*site, data=ReviewSp)
summary(mod19) #no sig

#Latitude
plot(b_value~Vulnerability, data=lat)
mod20<- lm(b_value~Vulnerability, data=lat)
summary(mod20) #no sig
abline(mod20,col="red")


#Area Occupied
plot(b_value~Vulnerability, data=area)
mod30<- lm(b_value~Vulnerability, data=area)
summary(mod30) #SIGNIFICANT #the more area occupied, the less vulnerable species are 
abline(mod30,col="red")



#######################
#Commercial Importance#       No sig results, nothing important
#######################

## Impact on spp
pietable<- pie(table(ReviewSp$b_impact))
qplot(ReviewSp$b_impact, ReviewSp$sciname)

## Price
subset<- Reviewsp[ReviewSp$sciname,"Pricecateg"]
plot(subset)
pietable<- pie(table(ReviewSp$PriceCateg))

## Commercial Importance
subset<- ReviewSp[ReviewSp$sciname,"Importance"]
plot(subset)
pietable<- pie(table(ReviewSp$Importance))

ggplot(ReviewSp, aes(x=Importance, y=sciname)) + geom_violin(alpha=0.75) +geom_jitter(height = 0)

## Aquaculture
subset<- ReviewSp[ReviewSp$sciname,"UsedforAquaculture"]
plot(subset)
pietable<- pie(table(ReviewSp$UsedforAquaculture))

## Habitat
subset<- ReviewSp[ReviewSp$sciname,"DemersPelag"]
plot(subset)
pietable<- pie(table(ReviewSp$DemersPelag))

## AnaCat
subset<- reviewsp[ReviewSp$sciname,"AnaCat"]
plot(subset)
pietable<- pie(table(ReviewSp$AnaCat))

##Gears
subset<- reviewsp[ReviewSp$sciname,"MainCatchingMethod"]
plot(subset)
pietable<- pie(table(ReviewSp$MainCatchingMethod))

##Gears II
subset<- ReviewSp[sppdata$sciname,"II"]
plot(subset)
pietable<- pie(table(ReviewSp$II))

##Years of study
ggplot(ReviewSp, aes(x=b_years, y=Importance ))+ 
  geom_point (aes(size =0.01))+
  ggtitle("From which groups of spp we have longer temporal data?")+
  theme(axis.title.x = element_text(face="bold"),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=7))

ggplot(ReviewSp, aes(x=b_years, y=sciname))+ 
  geom_point (aes(size =0.01))+
  ggtitle("SS and its time-data collection")+
  theme(axis.title.x = element_text(face="bold"),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=7))

ggplot(ReviewSp, aes(x=Importance, y=b_years, fill=b_impact)) + geom_violin(alpha=0.75) +geom_jitter(height = 0)

ggplot(ReviewSp, aes(x=Importance, y=b_impact)) + geom_violin(alpha=0.75) +geom_jitter(height = 0)


## Linking impacts-spp-commercial interest
ggplot(ReviewSp, aes(x=PriceCateg, y=sciname, colour=Importance))+ 
  geom_point (aes(size = 0.01))+
  ggtitle("Spp and their commercial values")+
  theme(axis.title.x = element_text(face="bold"),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=7))

ggplot(ReviewSp, aes(x=DemersPelag, y=Importance, colour=b_impact))+ 
  geom_point (aes(size=0.01))+
  ggtitle("Groups of spp and their commercial importance")+
  theme(axis.title.x = element_text(face="bold"),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=7))

ggplot(ReviewSp, aes(x=sciname, y=Importance, colour=DemersPelag))+ 
  geom_point (aes(size = 0.01))+
  ggtitle("Spp impacts and commercial importance")+
  theme(axis.title.x = element_text(face="bold"),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=7))

ggplot(ReviewSp, aes(x=Importance, y=b_impact, colour=PriceCateg))+ 
  geom_point (aes(size = b_years))+
  ggtitle("Spp impacts and commercial importance")+
  theme(axis.title.x = element_text(face="bold"),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=7))

ggplot(ReviewSp, aes(x=Importance, y=b_value, colour=b_impact))+ 
  geom_point (aes(size = 0.01))+
  ggtitle("Spp impacts and commercial importance")+
  theme(axis.title.x = element_text(face="bold"),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=7))

ggplot(ReviewSp, aes(x=b_impact, y=b_value))+ 
  geom_point (aes(size = 0.01))+
  ggtitle("Which impatc has higher vaues?")+
  theme(axis.title.x = element_text(face="bold"),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=7))

ggplot(ReviewSp, aes(x=Importance, y=b_direction))+ 
  geom_point (aes(size =0.01))+
  ggtitle("Do we consider CC?")+
  theme(axis.title.x = element_text(face="bold"),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=7))

ggplot(ReviewSp, aes(x=Importance, y=b_direction, colour=DemersPelag))+ 
  geom_point (aes(size =b_years))+
  ggtitle("direction of impact- Importance?")+
  theme(axis.title.x = element_text(face="bold"),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=7))

qplot(ReviewSp$Importance, ReviewSp$sciname)
qplot(ReviewSp$Vulnerability, ReviewSp$Importance)
pietable<- pie(table(ReviewSp$b_impact))
qplot(ReviewSp$b_impact, ReviewSp$sciname)
qplot(ReviewSp$Vulnerability, ReviewSp$sciname)
qplot(ReviewSp$Vulnerability, ReviewSp$DemersPelag)


#Importance related to different types of impacts
#########
levels(ReviewSp$Importance)
levels(ReviewSp$b_impact)
levels(ReviewSp$b_impact_combine)

#Area
mod1<-aov(b_value~Importance, data=area)
summary(mod1) #no sig difference

#Range
mod2<-aov(b_value~Importance, data=boundary)
summary(mod2) #no sig difference!

#Latitude
mod3<-aov(b_value~Importance, data=lat)
summary(mod3) #no sig difference!

#Longitude
mod4<-aov(b_value~Importance, data=long)
summary(mod4)# no sig difference!

#Depth
mod6<-aov(b_value~Importance, data=depth)
summary(mod6)# no sig dif !

#Lat_long
mod6<-aov(b_value~Importance, data=lat_long)
summary(mod6)# no sig difference!








#######################################################
# MORE ANALYSIS FOR ICES POSTER. ALBA. SEPTEMBER 2017 #
#######################################################

freq <- as.data.frame(table(ReviewSp$PriceCateg)); freq
freq <- as.data.frame(table(ReviewSp$Importance)); freq
freq <- as.data.frame(table(ReviewSp$DemersPelag)); freq



#Analysis on Pelagic vs Demersal

ReviewSp$DemersPelag<- as.factor(ReviewSp$DemersPelag)

impacts <- c ("Latitude", "Depth", "Area", "Boundary Latitude")
bathydemersal <- c ("11", "12", "2", "1") 
bathypelagic <- c ("2", "3", "0", "1")
benthopelagic <- c ("38", "34", "0", "4")
demersal <- c ("111", "108","14","23")
pelagic_neritic <- c ("19", "13","3", "2")
pelagic_oceanic <- c ("6", "4", "0", "0")
reef_associated <- c ("7", "6", "0", "1") 

data<-data.frame(impacts, bathydemersal, bathypelagic, benthopelagic, demersal, pelagic_neritic, pelagic_oceanic, reef_associated)

data1<-read.table("Stalked_Bar_Chart_demers_pelag.csv", header= T, sep= "," )

data1$fish <- factor(data1$fish, levels = c("bathydemersal","bathypelagic","benthopelagic", "demersal",
                                        "pelagic_neritic","pelagic_oceanic", "reef_associated"), labels=c("bathydemersal","bathypelagic","benthopelagic", "demersal",
                                                                                                          "pelagic neritic","pelagic oceanic", "reef associated"))

data1$Impact <- factor (data1$Impact, levels= c ("Area", 
                                                 "Boundary","Depth","Latitude"), labels=c("Area","Boundary Latitude", "Depth", "Mean Latitude"))                                       

ggplot(data = data1, aes(x = Impact, y = Sample.Size, fill = factor(fish))) + 
  geom_bar(stat="identity", alpha=0.8) + scale_fill_manual(values=c("darkgreen", "chartreuse3", "violet", "blue", "orangered", "red3", "black")) +
  labs( y = "Number of Observations", x = "Impact", fill = "Fish")+ theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'))

#  + coord_flip() from colums to rows in ggplot

#maybe too much detail in this graph?

data2<-read.table("Stalked_Bar_Chart_demers_pelag1.csv", header= T, sep= "," )

data2$fish <- factor(data2$fish, levels = c("demersal","pelagic", "reef_associated"), labels=c("Demersal",
                                                                                              "Pelagic", "Reef associated"))

data2$Impact <- factor (data2$Impact, levels= c ("Area", 
                                                 "Boundary","Depth","Latitude"), labels=c("Area","Boundary Latitude", "Depth", "Mean Latitude"))                                       

ggplot(data = data2, aes(x = Impact, y = Sample.Size, fill = factor(fish))) + 
  geom_bar(stat="identity", alpha=0.8) + scale_fill_manual(values=c("darkgreen", "red3", "chartreuse3")) +
  labs( y = "Number of Observations", x = "Impact", fill = "Fish")+ theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'), legend.position="left")


#Analysis of Commercial Importance

freq <- as.data.frame(table(ReviewSp$Importance)); freq

data2<-read.table("Stalked_Bar_Chart_impor.csv", header= T, sep= "," )

data2$importance <- factor(data2$importance, levels = c("highly_commercial", "commercial","minor_commercial", 
                                            "subsistence_fisheries", "no_interest"), labels=c("highly commercial", "commercial","minor commercial", 
                                                                                              "subsistence fisheries", "no interest"))

data2$Impact <- factor (data2$Impact, levels= c ("Area", 
                                                 "Boundary","Depth","Latitude"), labels=c("Area","Boundary Latitude", "Depth", "Mean Latitude"))                                       

ggplot(data = data2, aes(x = Impact, y = Sample.Size, fill = factor(importance))) + 
  geom_bar(stat="identity", alpha=0.8) + scale_fill_manual(values=c("black", "grey", "yellow", "blue", "brown")) +
  labs( y = "Number of Observations", x = "Impact", fill = "Commercial Importance")+ theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'))


#Analysis of Price Category

data3<-read.table("Stalked_Bar_Chart_price.csv", header= T, sep= "," )

data3$price <- factor(data3$price, levels = c("very_high","high","medium", "low"),
                                                  labels=c("Very high", "High", "Medium", "Low"))

data3$Impact <- factor (data3$Impact, levels= c ("Area", 
                                                 "Boundary","Depth","Latitude"), labels=c("Area","Boundary Latitude", "Depth", "Mean Latitude"))                                       

ggplot(data = data3, aes(x = Impact, y = Sample.Siza, fill = factor(price))) + 
  geom_bar(stat="identity", alpha=0.8) + scale_fill_manual(values=c("red3", "orangered", "yellow3", "gold")) +
  labs( y = "Number of Observations", x = "Impact", fill = "Price Category")+ theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'), legend.position="left")




#############

#Do species that have a greater range shift more? absolute values, not considering the direction of the shift

data4<-read.table("Depth_range.csv", header= T, sep= "," )
data4$Depth.Range<-as.numeric(as.character(data4$Depth.Range))
sum(is.na(data4$Depth.Range))

cor.test(data4$b_value,data4$Depth.Range, method="pearson") 

shapiro.test(data4$b_value)
shapiro.test(data4$Depth.Range)

log_b_value<- log(data4$b_value) #log transform
log_Depth.Range<-log (data4$Depth.Range) #log transform
mod<- aov (log_b_value~log_Depth.Range, data=data4)
summary(mod) 


plot(data4$b_value, data4$Depth.Range,             
     xlab="Shift Rate (m/dec)",        
     ylab="Depth Range")

ggplot(data4, aes(x=data4$b_value, y=data4$Depth.Range)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)+
  labs( y = "Depth range (m)", x = "Depth Shift Rate (m/decade)")+ theme(panel.background = element_rect(fill = 'whitesmoke', colour = 'black'))

#############

#Do species that live longer shift more? absolute values, not considering the direction of the shift

data5<-read.table("Longevity_Lat.csv", header= T, sep= "," )
str(data5)
sum(is.na(data5$longevity))

cor.test(data5$b_value,data5$longevity, method="pearson") 

shapiro.test(data5$b_value)
shapiro.test(data5$longevity)

log_b_value<- log(data5$b_value) #log transform
log_longevity<-log (data5$longevity) #log transform
mod<- aov (log_b_value~log_longevity, data=data5)
summary(mod) 
