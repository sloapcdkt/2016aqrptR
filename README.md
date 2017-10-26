# 2016aqrptR
Data and R code for graphs and analysis in 2016 Annual Air Quality Report for San Luis Obispo County.

## Details
The 206 AQ Report is a product of the [San Luis Obispo County Air Pollution Control District](http://www.slocleanair.org/) and will be available on the District website [here](http://www.slocleanair.org/library/air-quality-reports.php) once finalized.

## Data Sources
Almost all data used in the report were downloaded from EPA's [Air Quality System (AQS)](https://www.epa.gov/aqs). These raw data downloads are provided as `.txt` files. Data from California Department of Parks and Recreation's "S1" meteorology tower were obtained from their data management system, and is provided in the `.csv` files begining `vdv_`. The data in Tables 3 and 4 are from AQS AMP440 and AMP450 reports; these are provided as `.pdf` files.

## Analyses and Figures
Scripts for reproducing the analyses and figures in the report are provided as `.R` files. `AQSloader.R` contains a single function for loading certain types files spit out by AQS. Most of the other scripts make use of this function. Otherwise, the different scripts are independent, i.e. `ozone.R` depends on having sourced `AQSloader.R` but not on having run any of the other scripts. 

## Dependencies
The following packages will be needed: `openair`, `dplyr`, `reshape2`, `tree`, and `RColorBrewer`. All are available on [CRAN](https://cran.r-project.org/).

### Session Info:
```
R version 3.4.0 (2017-04-21)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 7 x64 (build 7601) Service Pack 1

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252   
[3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
[5] LC_TIME=English_United States.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] tree_1.0-37   dplyr_0.5.0    openair_2.1-0  reshape2_1.4.2

loaded via a namespace (and not attached):
 [1] Rcpp_0.12.10        cluster_2.0.6       magrittr_1.5        maps_3.1.1          MASS_7.3-47        
 [6] lattice_0.20-35     R6_2.2.0            stringr_1.2.0       plyr_1.8.4          tools_3.4.0        
[11] grid_3.4.0          nlme_3.1-131        mgcv_1.8-17         latticeExtra_0.6-28 DBI_0.6-1          
[16] lazyeval_0.2.0      assertthat_0.2.0    tibble_1.3.0        Matrix_1.2-9        RColorBrewer_1.1-2 
[21] mapproj_1.2-4       stringi_1.1.5       compiler_3.4.0      lubridate_1.6.0     hexbin_1.27.1   
```