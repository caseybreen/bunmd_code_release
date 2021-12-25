########################################################
# Create BUNMD Lexis Plot
########################################################

## Library Packages
library(gridExtra)
library(ggpubr)
library(data.table)
library(tidyverse)
library(HMDHFDplus)
``
## source helper functions
source("/censoc/code/workspace/numident_paper/code/helper_functions.R")

## Read in BUNMD file
bunmd <- fread("/censoc/data/numident/4_berkeley_unified_mortality_database/bunmd.csv")

## Get HMD deaths
hmd_deaths <-  readHMDweb(CNTRY = "USA", item = "Deaths_1x1", username ="caseybreen@berkeley.edu", password = "censoc") %>% mutate(linking_key = paste(Year, Age, sep = "_" ))

## Tabulate BUNMD 
bunmd.tabulation <- bunmd %>% 
  filter(death_age %in% 0:100) %>% 
  group_by(Year = dyear, Age = death_age) %>% 
  filter(Year %in% c(1940:2010)) %>% 
  summarize(deaths = n()) 

## HMD tabulation
hmd.tabulation <- hmd_deaths %>% 
  filter(Age %in% 0:100) %>% 
  group_by(Year, Age) %>% 
  filter(Year %in% c(1940:2010)) %>% 
  summarize(deaths = sum(Total)) %>% 
  mutate(source = "HMD")

## Prep data for Lexis Function
lexis.data <- bunmd.tabulation %>% 
  inner_join(hmd.tabulation, by = c("Year", "Age" )) %>%
  mutate(coverage = deaths.x/deaths.y)

## Call Lexis Function
lexis_coverage <- lexis(data = lexis.data, year_begin = 1940, age_begin = 0, fill_column = coverage, breaks = c(0, 0.25, .5, .75, .9, .95, 5),
                        labels = c( "0 to 25%",
                                    "25% to 50%",
                                    "50% to 75%",
                                    "75% to 90%",
                                    "90% to 95%",
                                    ">95% Death Coverage")) +
  labs(
    x = "",
    y = "Average Rating (0-10)",
    title = "Coverage",
    subtitle = ""
  )

## Prep data for Lexis Function
lexis.data.filter <- lexis.data %>% 
  filter(Year > 1985 & Age > 65)

## Call Lexis Function
lexis_coverage_all <- lexis(data = lexis.data.filter, year_begin = 1985, age_begin = 65, fill_column = coverage, breaks = c(0, 0.25, .5, .75, .9, .95, 5),
                            labels = c( "0%to 25%",
                                        "25% to 50%",
                                        "50% to 75%",
                                        "75% to 90%",
                                        "90% to 95%",
                                        ">95% Death Coverage")) +
  ## add on specific title etc. 
  labs(
    x = "",
    y = "Average Rating (0-10)",
    title = "High Coverage Subsample",
    subtitle = ""
  )

bunmd.lexis <- ggarrange(lexis_coverage, lexis_coverage_all, common.legend = T, legend="bottom")

bunmd.lexis <- annotate_figure(bunmd.lexis,
                               top = text_grob("BUNMD Death Record Coverage \n",
                                               face = "bold", size = 30))
