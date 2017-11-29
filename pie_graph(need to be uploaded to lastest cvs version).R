##R code for the ecological analysis of our review - SCRIPT FOR PIES AND SOME OTHER GRAPHS
## THIS SCRIPT NEEDS TO BE UPLOADED WITH THE LATEST DATABASE VERSION !!!!!!!!!!!!
## NOT DONE SO FAR BECAUSE THESE GRAPHS DONT SEEM USEFUL FOR NOW

##Author: Alba and Iratxe
##Date: June 8th 2017

#open dataset
table1<-read.table("biblio_database.csv", header= T, sep= ",")
table<-table1[,1:68]    #delete columns 
str(table)

#Factors
table$researcher<-as.factor(table$researcher)
table$id_study<-as.factor(table$id_study)
table$cc<-as.factor(table$cc)
table$b_impact<-as.factor(table$b_impact)
table$b_direction<-as.factor(table$b_direction)

#Numeric
table$b_value<-as.numeric(as.character(table$b_value), na.omit=TRUE)
table$b_years<-as.numeric(as.character(table$b_years))

#change codes to a short description; in our review center of grav separated from center of biomass
levels(table$b_impact)<-c("lat shift center of grav", #1
                          "lat shift center of bio",  #2
                          "depth shift", #3
                          "boundary lat shift center of grav", #4
                          "boundary lat shift center of bio", #5
                          "long shift center of grav", #7
                          "long shift center of bio", #8
                          "lat and long shift center of grav", #9
                          "shift in area occupied") #11

levels(table$b_direction)<-c("lat shift north center of grav", #1
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

levels(table$cc)<-c("AMO", #10
                    "climate velocity", #11
                    "sst", #2
                    "sst,bt,AMO", #2,3,10
                    "sst,bt", #2,3
                    "bt") #3

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

#Subsets for later exploration
lat_cg<-subset (table, b_impact=="lat shift center of grav")
boundary_cg<-subset (table, b_impact== "boundary lat shift center of grav")
long_cg<-subset (table, b_impact== "long shift center of grav")
#depth_cg = depth (all observations for depth are center of gravity, none is center of biomass)
#area_cg = area (no differentiation between center of grav and center of bio)
#lat_long_cg = lat_long (all observations for lat and long are center of gravity, none is center of biomass)
lat_bio<-subset (table, b_impact=="lat shift center of bio")
boundary_bio<-subset (table, b_impact== "boundary lat shift center of bio")
long_bio<-subset (table, b_impact== "long shift center of bio")



#General overview
pie(table(table$author))
pie(table(table$site))

# PIE: Observations per site 
pie(table(table$site_group))
site<-c("Arctic Ocean","Bering Sea","Korean peninsula","Mediterranean Sea",
        "Northeast Atlantic Ocean","Northwest Atlantic Ocean") #describe(table$site_group)
n1<-c(2,5,6,1,82,93) #sum(n1)
percentlabels<- round(100*n1/sum(n1), 1)
cols <- c("grey","mediumorchid1","tomato","green2","hotpink1","royalblue1")
pielabels<- paste(percentlabels, "%", sep ="","(","n=",n1,")")
pie(n1, main="Observations per site", col=cols, labels= pielabels,cex=1,
    radius = 1)
legend("topleft", c("Arctic Ocean","Bering Sea","Korean peninsula","Mediterranean Sea",
                    "Northeast Atlantic Ocean","Northwest Atlantic Ocean"), 
       cex=0.9, fill=cols, bty = "n")

# PIE: Variables causing shifts in fish distribution
pie(table(table$cc)) #problem with the empty level (?)
a<-as.data.frame(table(table$cc))
colnames(a)<-c("cc","n2")
labels<- round(100*a$n2/sum(a$n2), 1)
cols <- c("grey","mediumorchid1","tomato","green2","hotpink1","royalblue1")
pielabels<- paste(labels, "%", sep ="","(","n=",a$n2,")")
pie(a$n2, main="Variables causing shifts in fish distribution", col=cols, labels= pielabels,cex=1,
    radius = 1)
legend("topleft", c("EMPTY","AMO","SST","SST,SBT,AMO","SST,SBT",
                    "SBT"), 
       cex=0.9, fill=cols, bty = "n")

# PIE: Climate Change impacts in fish distribution
pie(table(table$b_impact_combine))
b<-as.data.frame(table(table$b_impact_combine))
colnames(b)<-c("Impact","n3")
labels<- round(100*b$n3/sum(b$n3), 1)
cols <- c("grey","mediumorchid1","tomato","green2","hotpink1","royalblue1")
pielabels<- paste(labels, "%", sep ="","(","n=",b$n3,")")
pie(b$n3, main="Climate change impacts in fish distribution", col=cols, labels= pielabels,cex=1,
    radius = 1)
legend("topleft", c("depth shift","lat and long shift","shift in area occupied","lat shift",
                    "boundary lat shift","long shift"), 
       cex=0.9, fill=cols, bty = "n")

# PIE: Shift direction
pie(table(table$b_direction_combine))
c<-as.data.frame(table(table$b_direction_combine))
colnames(c)<-c("Shift_direction","n4")
labels<- round(100*c$n4/sum(c$n4), 1)
cols <- c("grey","mediumorchid1","tomato","green2","hotpink1","royalblue1","dodgerblue4","gold","darkseagreen1","orangered","black","tan","pink","yellow")
pielabels<- paste(labels, "%", sep ="","(","n=",c$n4,")")
pie(c$n4, main="Shift direction", col=cols, labels= pielabels,cex=0.9,
    radius = 0.7)
legend("topleft", c("max lat shift north","max lat shift south","min lat shift north","shifting deeper",
                    "shifting shallower","area expansion","area contraction","shift southweastwards","shift northeastwards",
                    "shift westwards","boundary lat shift east","lat shift north","lat shift south","boundary lat shift north"),
       cex=0.9, fill=cols, bty = "n")







#GRAPH: get the multiplot per site
table$b_impact <- factor(table$b_impact,labels = c("lat shift center of grav", 
                                                   "lat shift center of bio", 
                                                   "depth shift", 
                                                   "boundary lat shift center of grav", 
                                                   "boundary lat shift center of bio", 
                                                   "long shift center of grav", 
                                                   "long shift center of bio", 
                                                   "lat and long shift center of grav", 
                                                   "shift in area occupied"))

###b_impact for latitudes and longitudes
lat_lon<-table[table$b_impact %in% c("lat shift center of grav",
                                     "lat shift center of bio",
                                     "boundary lat shift center of grav",
                                     "boundary lat shift center of bio",
                                     "long shift center of grav", 
                                     "long shift center of bio", 
                                     "lat and long shift center of grav"),]

l<-as.data.frame(table(lat_lon$b_impact))
l<-bind_rows(list(l[1:2,],l[4:8,]))
label_site<-as.data.frame(table(lat_lon$site_group))
label_site<-label_site[2:7,]

ggplot(lat_lon, aes(x= lat_lon$b_impact , y = lat_lon$b_value)) +
  scale_x_discrete(name = NULL, 
                   labels=paste(c("lat shift center \n of grav",   
                                  "lat shift center \n of bio",
                                  "boundary lat \n shift center \n of grav",
                                  "boundary lat \n shift center \n of bio",
                                  "long shift \n center of grav", 
                                  "long shift \n center of bio", 
                                  "lat and long shift \n center of grav"),"\n(n=",l$Freq,")",sep="")) +
  scale_y_continuous(name = "Latitude/longitude shift (Km/decade)")  +
  theme(axis.text.x=element_text(colour="black", size = 8)) +
  geom_boxplot(aes(fill=factor(lat_lon$site_group)))+
  scale_fill_discrete(name="Site grouped",
                      labels=paste(c("Arctic Ocean", "Bering Sea", "Korean peninsula","Mediterranean Sea","Northeast Atlantic Ocean","Northwest Atlantic Ocean"),"(n=",label_site$Freq,")",sep=""))



