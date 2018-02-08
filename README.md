# cc-distribution-review

Climate change distributional impacts in marine commercial species Review Database Repository.

biblio_database.csv

This repository contains:
 
<strong>-1. ECOLOGICAL ANALYSIS</strong>

<strong>Biomass vs distribution:</strong>
r code for the exploration of center of biomass vs center of gravity observations. Is it possible to combine them? It contains:

1. Center of biomass combined with center of gravity (We concluded this is the best option). Input: biblio_database.csv

2. Just center of gravity observations. Input: biblio_database.csv

3. Just center of biomass observations. Input: biblio_database.csv

<strong>Distributional impacts:</strong> 
r code for the analysis of climate variables related to the distributional impacts and their magnitude. It contains:

1. Analysis of the climate change variables that cause each of the distributional impacts (Stalked chart Figure 2D in our poster) Input: biblio_database.csv and Stalked_Bar_Chart_cc_variables.csv

2. Magnitude of the latitude shift: boxplot, geom_bar, and histograms of different types (type 3.2 in the script Figure 2A in our poster). Input: biblio_database.csv

3. Magnitude of the depth shift: boxplot, geom_bar, and histograms of different types (type 3 in the script Figure 2B in our poster). Input: biblio_database.csv

4. Magnitude of the boundary shift: boxplot, geom_bar and histograms of different types (type 3 in the script Figure 2C in our poster). Input: biblio_database.csv

5. Magnitude of the area shift: boxplot, geom_bar and histograms of different types. Input:biblio_databse.csv

6. Histograms Multiplot for latitude, depth, boundary and area magnitude shifts. Input: biblio_database. csv


<strong>Pie graph: NOT UPLOADED SCRIPT TO THE LASTEST biblio_database.csv!</strong>

r code for pie graphs and other graphs not used yet. Input: biblio_database.csv

<strong>-2. FISH BASE </strong>

<strong>Integration fishbase:</strong>
r code for the integration of our review database and Fish Base information 
Input: biblio_database.csv
Output: ReviewDatsp.csv and ReviewDatst.csv

<strong>Species code:</strong>
r code for the price category, vulnerability and commercial importance at species level. It contains:

1. Exploratory analysis of price category per distributional impact. Input: ReviewDatsp.csv

2. Exploratory analysis of vulnerability per distributional impact. Input: ReviewDatsp.csv

3. Exploratory analysis of commercial importance per distributional impact. Input: ReviewDatsp.csv

4. Pie charts and ggplots on aquaculture, habitat, gears, anad/catad, years of study and commercial importance. Input: ReviewDatsp.csv

5. Analysis for ICES poster:

     5.1. Number of Pelagic vs. demersal (Figure 2E in our poster). Input: ReviewDatsp.csv and Stalked_Bar_Chart_demers_pelag1.csv

     5.2. Number of commercial important species. Input: ReviewDatsp.csv and Stalked_Bar_Chart_impor.csv

     5.3. Number of different price category species (Figure 2F in our poster). Input: ReviewDatsp.csv and Stalked_Bar_Chart_price.csv

6. Exploratory analysis on "Do species that have a greater range shift more or less?" and "Do species that live longer shift more?" Input: ReviewDatsp.csv

<strong>Stock code:</strong>
r code for the analysis of the resilience and environment/habitat at stock level. It contains:

1. Exploratory analysis of resilience per distributional impact. Input: ReviewDatst.csv
2. Exploratory analysis of environment/habitat per distributional impact. Input: ReviewDatst.csv

<strong>-3. SEA AROUND US</strong>

Graphs on fish dependency  and pressure per species.
It contents one R code file (Data_SAU.R) for merging original SAU data of countries into one file (Final_SAU_FE), R code (Data_Merge_SAU.R) to merge the database from the review (biblio_database) and the SAU EEZ data, and  R code file (Data_Merge_SAU(2).R) to add data on fishing entities, and a final code (Graphs_Lat_SAU.R) for the graphs on latitude, and one .Rmd file for exporting these graphs to PDF/htlm (TO BE UPDATED). 


<strong>1. Data_SAU.R</strong>
r code for creating a .csv file with data on fishing entity total per species (FE_EEZ.csv.csv)
Use: only needed if adding new observations to the biblio_database.
input: .csv per country on SAU data (in /Data SAU)
output: FE_EEZ.csv

<strong>2. Data_Merge_SAU.R</strong>
r code for merging the biblio_database and the SAU data for adding columns on EEZ catches. 
Use: when adding new observations/modifying biblio_database. For selecting species and equivalences between SAU and biblio_database.
input: biblio_database.csv and FE_EEZ.csv
output: ReviewDat_Merge_SAU.csv

<strong>2. Data_Merge_SAU(2).R</strong>
r code for adding information on the fishing entities from SAU. 
Use: after updating Data_Merge_SAU.R
input: ReviewDat_Merge_SAU.csv
output: Biblio_database_full.csv

<strong>3. Graphs_Lat_SAU.R</strong>
r code for creating the graphs on latitude and countries dependency,  and latitude and eez pressure.
Use: directly from last version of Biblio_database_full.csv.
input: Biblio_database_full.csv
output:graphs


