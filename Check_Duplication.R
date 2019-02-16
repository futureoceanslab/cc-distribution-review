##Author: Diego Salgueiro Otero
##Date: 16/02/2019
##Title: Duplication of databses source from literature review on distribution impact CC

##call libraries
library(tidyr)
library(dplyr)

##Understand db
glimpse(biblio_database)

##check author variable and coerce it
class(biblio_database$author)
levels (as.factor(biblio_database$author))

##Northwest Atlantic- Canada east coast
bell<- biblio_database%>%
  filter(biblio_database$author=="Bell et al")
glimpse(bell)
levels(as.factor(bell$author))
levels(as.factor(bell$b_scientific_name))

nye<- biblio_database%>%
  filter(biblio_database$author=="Nye et al")
glimpse(nye)
levels(as.factor(nye$author))
levels(as.factor(nye$b_scientific_name))

nye<- count(as.numeric("id_obs"))

Overholtz<- biblio_database%>%
  filter(biblio_database$author=="Overholtz et al")
glimpse(Overholtz)
levels(as.factor(Overholtz$author))
levels(as.factor(Overholtz$b_scientific_name))

Murawski<- biblio_database%>%
  filter(biblio_database$author=="Murawski")
glimpse(Murawski)
levels(as.factor(Murawski$author))
levels(as.factor(Murawski$b_scientific_name))

##Bering sea
Mueter<- biblio_database%>%
  filter(biblio_database$author=="Mueter & Litzow")
glimpse(Mueter)
levels(as.factor(Mueter$author))
levels(as.factor(Mueter$b_scientific_name))

##North sea
Perry<- biblio_database%>%
  filter(biblio_database$author=="Perry et al")
glimpse(Perry)
levels(as.factor(Perry$author))
levels(as.factor(Perry$b_scientific_name))

Dulvy<- biblio_database%>%
  filter(biblio_database$author=="Dulvy et al.")
glimpse(Dulvy)
levels(as.factor(Dulvy$author))
levels(as.factor(Dulvy$b_scientific_name))

Montero<- biblio_database%>%
  filter(biblio_database$author=="Montero-Serra et al.")
glimpse(Montero)
levels(as.factor(Montero$author))
levels(as.factor(Montero$b_scientific_name))

Kerby<- biblio_database%>%
  filter(biblio_database$author=="Kerby et al")
glimpse(Kerby)
levels(as.factor(Kerby$author))
levels(as.factor(Kerby$b_scientific_name))

Engelhard<- biblio_database%>%
  filter(biblio_database$author=="Engelhard et al")

Engelhard2011<-Engelhard%>%
  filter(Engelhard$article_year=="2011")
glimpse(Engelhard2011)
levels(as.factor(Engelhard2011$author))
levels(as.factor(Engelhard2011$b_scientific_name))

Engelhard2014<- Engelhard%>%
  filter(Engelhard$article_year=="2014")
glimpse(Engelhard2014)
levels(as.factor(Engelhard2014$author))
levels(as.factor(Engelhard2014$b_scientific_name))


