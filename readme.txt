Replication Package

The this repository includes code to replicate all figures in the paper "Berkeley Unified Numident Mortality Database: Public administrative records for individual-level mortality research." 


1. Download the BUNMD file from the CenSoc Project Download Page (https://censoc-download.demog.berkeley.edu/), unzip the data file, and move it to the top level of the 'data' subfolder.
2. Run the 00_run_all.Rmd script, which will run all code (or run scripts 01 and 02 individually)
3. Look in the 'data' subfolder 


Additional details: 

/data directory  

* cuyahoga_county_shapefile.rda — shape-file for cuyahoga county, home of Cleveland 
* hmd_deaths_1x1_usa.csv — HMD deaths 1x1 for USA. Downloaded from https://www.mortality.org/
* oasdi_zip05_combined_new.csv — ZIP Code level data average social security benefits 


—————————————


/code directory 


* 00_run_all.Rmd - this script runs all scripts.

Alternatively, researchers can run the following files individually in order:

* 01_descriptive_figures.Rmd - this R notebook contains code to replicate the following figures in the paper:

— Figure 1
— Figure 2
— Figure 3
— Figure A-2
— Figure A-3

* 02_case_studies.Rmd - this R notebook runs code to replicate the two case studies and the following figures in the paper:

— Figure 4
— Figure 5

Note: Figure A-1 is a manually constructed flow-chart. 

* helper_functions.R - this script contains custom helper functions.  


* weights/

Contains a script to replicate the weights in the BUNMD. The R function allows users to recalculate weights for custom user-generated sub-samples.

—————————————

\figures directory 

Contains all Figures in the directory. 

—————————————


Authors

* Casey Breen (caseybreen@berkeley.edu)
* Joshua Goldstein


