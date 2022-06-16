## Berkeley Unified Numident Mortality Database: Public Administrative Records for Individual-Level Mortality Research

The Berkeley Unified Numident Mortality Database (BUNMD) is available for download:

https://censoc.berkeley.edu/ 

[BUNMD Codebook](https://github.com/caseybreen/numident_paper/raw/master/codebook/berkeley_unified_numident_documentation.pdf
)

### Abstract

The Numerical Identification System (Numident) forms the backbone of the U.S. Social Security Administration’s record keeping system. For every person with a Social Security number, the Numident tracks claims status, date of birth (and, if applicable, death), as well as other background information including birthplace, race, sex, and names of parents. In 2013, the Social Security Administration transferred a large portion of their Numident records to the National Archives and Records Administration (NARA). The public release of these records in 2019, which we call "NARA Numident," offers nearly complete coverage of those who died from 1988 to 2005. In this paper, we describe the contents of the publicly available records, introduce a cleaned and harmonized version of the data, and show how the records can be used for the study of mortality in the United States. 

## Replication

This repository contains code and materials to replicate "Berkeley Unified Numident Mortality Database: Public Administrative Records for Individual-Level Mortality Research."

<!---
% A pre-print of the paper is available on SocArXiv: [https://osf.io/preprints/socarxiv/87e32/](https://osf.io/preprints/socarxiv/87e32/) 
-->


### Replication Package

The this repository includes code to replicate all figures and tables in the paper. There are three steps to running the replication code: 

1. Clone this repository
2. Download the BUNMD file from the [CenSoc Project Download Page](https://censoc-download.demog.berkeley.edu/), unzip the data file, and move it to the top level of the 'data' subfolder. 
3. Run the `00_run_all.Rmd` script, which will run all code (or run scripts `01` and `02` individually)


#### Data 

Please download all data for replication from the [CenSoc Project Download Page](https://censoc-download.demog.berkeley.edu/)

#### Code 

After downloading the required data and moving into the data folder of the replication repository, researchers can run the following script to replicate all figures and tables: 

- `00_run_all.Rmd` - this file runs all scripts. 

Alternatively, researchers can run the following files individually in order: 

- `01_descriptive_figures.Rmd` - this file contains code to replicate the following figures in the paper: 

    * Figure 1
    * Figure 2 
    * Figure 3
    * Figure A-2
    * Figure A-3 

- `02_case_studies.Rmd` - this file runs code to replicate the two case studies and the following figures in the paper: 

    * Figure 4
    * Figure 5 

### Authors

- [Joshua Goldstein](https://jrgoldstein.com/)
- [Casey Breen](caseybreen.com)

