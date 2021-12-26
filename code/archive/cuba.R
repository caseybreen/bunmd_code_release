#############################################
## Racial differences among Cuban immigrants
#############################################

## Library Packages
library(memisc)
library(data.table)
library(tidyverse)

## source helper functions
source("/censoc/code/workspace/numident_paper/code/helper_functions.R")

## read in BUNMD file
dt <- fread("/censoc/data/numident/4_berkeley_unified_mortality_database/bunmd.csv")

## restrict to rows with complete birthplace and race
dt <- dt %>% 
  filter(!is.na(cweight)) %>% 
  as.data.table

## 2) create analysis variables
## code countries with a lot of immigrants
dt[, country := vector(mode = "character", length = nrow(dt))]
dt[bpl ==  15000, country := "Canada"]
dt[bpl ==  20000, country := "Mexico"]
dt[bpl ==  25000, country := "Cuba"]
dt[bpl ==  26030, country := "Jamaica"]
dt[bpl ==  41000, country := "England"]
dt[bpl ==  41400, country := "Ireland"]
dt[bpl ==  43300, country := "Greece"]
dt[bpl ==  43400, country := "Italy"]
dt[bpl ==  43600, country := "Portugal"]
dt[bpl ==  45010, country := "Austria"]
dt[bpl ==  45200, country := "Czechoslovakia"]
dt[bpl ==  45310, country := "Germany"]
dt[bpl ==  45500, country := "Poland"]
dt[bpl ==  45700, country := "Yugoslavia"]
dt[bpl ==  46500, country := "Russia"]
dt[bpl ==  50000, country := "China"]
dt[bpl ==  50220, country := "Korea"]
dt[bpl ==  50100, country := "Japan"]
dt[bpl ==  51500, country := "Philippines"]
dt[bpl ==  54100, country := "Syria"]
dt[bpl < 1300,    country := "USA"]

## prepare variables for regression
dt[, Byear := as.factor(byear)]
dt[, Byear := relevel(Byear, ref = "1910")]

## recode race
dt[race_first == 1, Race := "White"]
dt[race_first == 2, Race := "Black"]
dt[race_first == 3, Race := "Other"]
dt[race_first == 4, Race := "Asian"]
dt[race_first == 5, Race := "Hispanic"]
dt[race_first == 6, Race := "Native"]
dt[, Race := relevel(as.factor(Race), ref = "White")] ## capital "W"?

## restrict to years with high coverage 
## restrict to Cuba
my.dt.3 <- dt[country %in% c("Cuba") &
              byear %in% 1900:1920 &
              dyear %in% 1988:2005 &
              age_first_application < 65]

## For this analysis, we focus on the original answer that people gave to the race question. Most of these responses were prior to 1980, 
## when there were only 3 categories ("White", "Black", and "Other"). (A possible extension of this initial analysis would be to consider 
## responses to post-1980 race classification, which included "Hispanic", "Asian", and "Native American".
## However, analysis of the post-1980 classification is complicated by the fact that most of these respondents were filling out the form for a 2nd time

## you can only be White, Black, or Other before 1980
my.dt.3[race_first == 1 & race_first_cyear < 1980, Race_pre_1980 := "White"]
my.dt.3[race_first == 2 & race_first_cyear < 1980, Race_pre_1980 := "Black"]
my.dt.3[race_first == 3 & race_first_cyear < 1980, Race_pre_1980 := "Other"]

## Set reference groups
my.dt.3[, Race_pre_1980 := relevel(factor(Race_pre_1980), ref = "White")]

## Run regression seperately for men and women
m.cuba.m.pre_1980 <-  lm(death_age ~ Race_pre_1980 +  Byear,
                      data = my.dt.3,
                      subset = sex == 1,
                      weight = cweight)

m.cuba.f.pre_1980 <- update(m.cuba.m.pre_1980,
                   subset = sex == 2)

mtable(m.cuba.m.pre_1980, m.cuba.f.pre_1980)

## crosstabs
my.dt.3[, table(Race_pre_1980, sex)]

## both sexes combined -- table for paper
m.cuba.mf.pre_1980 <-  lm(death_age ~ Race_pre_1980 +  Byear + as.factor(sex),
                      data = my.dt.3,
                      weight = cweight)

## crosstabs
mtable(m.cuba.mf.pre_1980)

## Create table for paper
## library(stargazer)
## stargazer(m.cuba.mf.pre_1980, single.row = TRUE)

