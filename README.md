# cc-distribution-review

Climate change distributional impacts in marine commercial species Review Database Repository.

biblio_database.csv

This repository contains:
 
<strong>-1. ECOLOGICAL ANALYSIS</strong>

<strong>Biomass vs distribution:</strong>
r code for the exploration of center of biomass vs center of gravity observations. Is it possible to combine them? It contains:

1. Center of biomass combined with center of gravity (We concluded this is the best option)

2. Just center of gravity observations 

3. Just center of biomass observations

<strong>Distributional impacts:</strong> 
r code for the analysis of climate variables related to the distributional impacts and their magnitude. It contains:

1. Analysis of the climate change variables that cause each of the distributional impacts (Stalked chart Figure 2D in our poster) To produce this figure the CVS "Stalked_Bar_Chart_cc_variables" is needed 

2. Magnitude of the latitude shift: boxplot, geom_bar, and histograms of different types (type 3.2 in the script Figure 2A in our poster)

3. Magnitude of the depth shift: boxplot, geom_bar, and histograms of different types (type 3 in the script Figure 2B in our poster)

4. Magnitude of the boundary shift: boxplot, geom_bar and histograms of different types (type 3 in the script Figure 2C in our poster)

5. Magnitude of the area shift: boxplot, geom_bar and histograms of different types

6. Histograms Multiplot for latitude, depth, boundary and area magnitude shifts


<strong>Pie graph: NOT UPLOADED SCRIPT TO THE LASTEST biblio_database.csv!</strong>

r code for pie graphs and other graphs that don´t seem useful for now

<strong>-2. FISH BASE </strong>

<strong>Integration fishbase:</strong>
r code for the integration of our review database and Fish Base information

<strong>Species code:</strong>
r code for the price category, vulnerability and commercial importance at species level. It contains:

1. Exploratory analysis of price category per distributional impact

2. Exploratory analysis of vulnerability per distributional impact

3. Exploratory analysis of commercial importance per distributional impact

4. Pie charts and ggplots on aquaculture, habitat, gears, anad/catad, years of study and commercial importance.

5. Analysis for ICES poster:

     5.1. Number of Pelagic vs. demersal (Figure 2E in our poster). Input: biblio_database.csv and Stalked_Bar_Chart_demers_pelag1.csv

     5.2. Number of commercial important species. Input: biblio_database.csv and Stalked_Bar_Chart_impor.csv

     5.3. Number of different price category species (Figure 2F in our poster). Input: biblio_database.csv and Stalked_Bar_Chart_price.csv

6. Exploratory analysis on "Do species that have a greater range shift more or less?" and "Do species that live longer shift more?"

<strong>Stock code:</strong>
r code for the analysis of the resilience and environment/habitat at stock level. It contains:

1. Exploratory analysis of resilience per distributional impact
2. Exploratory analysis of environment/habitat per distributional impact

<strong>-3. SEA AROUND US</strong>
Graphs on fish dependency (EEZ, fishing country and per species). Figure 3 in our poster.
It contents three R code files (.R) for creating the data on catches (total), per EEZ and per EEZ sp from the SAU data original

<strong>1. Catches SAU fishing entity:</strong>
r code for creating .csv file with data on fishing country catches from SAU (CATCHDat.csv)
input: Final SAU EEZ.csv

<strong>2. CCatches SAU area_name:</strong>
r code for creating a .csv file with data on EEZ catches (CatchDatEEZ.csv)
input: Final SAU EEZ.csv

<strong>3. Catches SAU area_name_sp:</strong>
r code for creating a .csv file with data on EEZ catches for the specific species (CatchDatEEZsp.csv)
input: Final SAU EEZ.csv

<strong>4. Graphs_SAU_Review_elena_wpinksy:</strong>
R markdown code for the figures, used the 3  .csv generated files as input, together with the Biblio_database.csv file
input: biblio_database.csv


