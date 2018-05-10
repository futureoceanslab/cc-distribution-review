# cc-distribution-review

Climate change distributional impacts in marine commercial species Review Database Repository.

biblio_database.csv


This repository contains:
 

<strong>-1. ECOLOGICAL ANALYSIS</strong>

<strong>distributional_impacts_CENTERS_BIO_vs_GRAV</strong>
r code for the exploration of center of biomass vs center of gravity observations. Is it possible to combine them? input: biblio_database.csv

<strong>distributional_impacts_PAPER</strong> 
r code for the analysis of climate variables and distributional impacts. Figures for the paper and SSMM. Input: biblio_database. csv

<strong>distributional_impacts_OTHER</strong> 
r code for the analysis of climate variables and distributional impacts not included in the paper. Input: biblio_database. csv

<strong>distributional_impacts_FISHBASE</strong> 
r code for the analysis of the database combined with fishbase information data per species and per stock. Inputs: ReviewDatst. csv, ReviewDatsp.csv


<strong>-2. SEA AROUND US</strong>

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

<strong>-4. DATABASE DESCRIPTION </strong>

R code which describes the database used for this study.

 <strong>1.General description:</strong> Authors, year or article publication, type of data, duration of sampling, etc.
 Outcome: Figure

<strong>2. Quality of data:</strong> Statistical methodology, number of observations and length of databases. 
 Outcome: Figure
