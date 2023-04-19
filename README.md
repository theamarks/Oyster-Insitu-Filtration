# *In situ* Clearance Rates of Olympia Oyster (*Ostrea lurida*) Habitat and Japanese Oyster (*Magallana gigas*) Aquaculture in California

This repository holds the wrangling, analysis, and document building code for two versions of this research: 1) `Publication_Analysis` and 2) `Thesis_Manuscript`

## Instructions to Reproduce Publication Analysis:

1) Run all `01_Data_wrangling.Rmd`
2) Run all `Publication_Analysis/Pub_Results.Rmd`

 ## File Details:

- Raw data is in `Data/` 
- `manual_Corrections/` contains a .csv with field notes and observations used to cut "disturbed" data from the analysis (e.g. major boat wake)
- `Thesis_Manuscript/` contains scripts used to construct CSUF thesis
- `Publication_Analysis/` contains scripts used to analyze data for publication
- `output/` will be populated by 10 sub-directories when `01_Data_wrangling.Rmd` is run
- `functions.R` contains wrangling and analysis functions. sourced by other `.Rmd` scripts
- `01_Data_wrangling.Rmd` cleans up raw data, pairs upstream and downstream measurements based on water velocity, corrects instrument measurements based on side-by-side trials and compiles summary tables used in further analysis. When knit, this file produces a html dashboard `Insitu_Filtration_Data_wrangling.html` displaying time series plots of filtration and side-by-side before and after trials for each field experiment. Tabs show raw data, data corrected by `manual_Corrections`, and Chlorophyll drawdown over time of paired instrument measurements. 
- `02_Water_Quality.Rmd` when knit produces a html dashboard `Insitu_Filtration_Water_Quality.html` that displays all water quality data collected during filtration and side-by-side trials from both upstream and downstream instruments (Chl a, turbidity, temperature, and salinity). These are exploratory time series not directly use for anlysis or needed to reproduce thesis or publication results.


## Thesis Details

M.S. completed 2021 with this version --> [![DOI](https://zenodo.org/badge/211166776.svg)](https://zenodo.org/badge/latestdoi/211166776)

Althea N. Marks  
M.S. Biology | Marine Ecology

Graduate Thesis for  
Department of Biological Sciences  
California State University Fullerton

Advisor: Dr. Danielle Zacherl (CSUF)  
Committee: Dr. Doug Eernisse (CSUF), Dr. Paul Stapp (CSUF), Dr. Ted Grosholz (UC Davis)  
Statistical Consulting: Dr. Kevin Nichols (CSUF)  
Programming Consulting: Sarthak Saini 

### Instructions to Reproduce Thesis 

Not sure this is working at the moment. I've learned a lot about reproducible practices since first putting this together. Shortcut: open `THESIS.pdf`

1) Run all `01_Data_wrangling.Rmd`
2) Knit `Thesis_Manuscript/THESIS.Rmd` which will knit and compile children documents into single final document `THESIS.pdf` (`Thesis_Acknowledgments.Rmd`, `Thesis_Abstract.Rmd`, `Thesis_Introduction.Rmd`, `Thesis_Methods.Rmd`, `Thesis_Results.Rmd`, and `Thesis_Discussion.Rmd`)

## Publication Status

In prep for publication 2023

Co-authors: Dr. Matthew Gray (UMCES), Dr. Kevin Nichols (CSUF), Dr. Danielle Zacherl (CSUF)


## Session Info & package versions needed to run:

R version 3.6.3 (2020-02-29)  
Platform: x86_64-apple-darwin15.6.0 (64-bit)  
Running under: macOS Sierra 10.12.6  

Matrix products: default  
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib  
LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib 

locale:  
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:  
[1] grid      stats     graphics  grDevices datasets  utils     methods   base       

other attached packages:  
 [1] pwr_1.3-0          quantreg_5.85      SparseM_1.81       multcompView_0.1-8  
 [5] agricolae_1.3-3    broom_0.7.6        tinytex_0.38       ISLR_1.2            
 [9] MASS_7.3-54        xtable_1.8-4       olsrr_0.5.3        wesanderson_0.3.6   
[13] cowplot_1.1.1      scales_1.1.1       viridis_0.6.2      ggpmisc_0.3.9       
[17] gridExtra_2.3      forcats_0.5.1      viridisLite_0.4.0  hms_1.1.1           
[21] stringr_1.4.0      ggthemes_4.2.4     ggplot2_3.3.3      lubridate_1.8.0     
[25] data.table_1.13.6  magrittr_2.0.3     readr_2.1.1        tidyr_1.2.0       
[29] dplyr_1.0.7        knitr_1.38        



