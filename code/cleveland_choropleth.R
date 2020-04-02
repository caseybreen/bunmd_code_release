## library packages
library(data.table)
library(tidyverse)
library(choroplethrZip)
library(choroplethr)
library(sf)
library(broom)

## source helper functions
source("/censoc/code/workspace/numident_paper/code/helper_functions.R")

## Read in data
bunmd <- fread("/censoc/data/numident/4_berkeley_unified_mortality_database/bunmd.csv")

## create 5-digit ZIP codes
bunmd[,"zip5" := as.numeric(substr(zip_residence, 1, 5))]

## Find cuyahoga county ZIP Codes
data("zip.regions")

## Filter to only include ZIP Codes in Cuyahoga
cuyahoga.zip <- zip.regions %>%
  filter(county.name == "cuyahoga")

## filter to only include cuyahoga ZIP Codes
cuyahoga.df <- bunmd %>% 
  filter(zip5 %in% cuyahoga.zip$region) %>% 
  as.data.table()

## restrict to high coverage years
cuyahoga.df.filter <- cuyahoga.df[byear %in% 1910:1919 &
                                    dyear %in% 1988:2005 &
                                    age_first_application < 65]

## prepare variables for regression
cuyahoga.df.filter[, Byear := as.factor(byear)]
cuyahoga.df.filter[, Byear := relevel(Byear, ref = "1910")]

## prepare variables for regression
cuyahoga.df.filter[, Zip5 := as.factor(zip5)]
cuyahoga.df.filter[, Zip5 := relevel(Zip5, ref = "44101")]


## linear model predicting age at death from ZIP and byear 
cuyahoga.model <- lm(death_age ~ Zip5 +  Byear,
                     data = cuyahoga.df.filter,
                     weight = ccweight)

## tidy lm 
cuyahoga.model.df <- tidy(cuyahoga.model)

## select coefficients and ZIP Codes
cuyahoga.model.df <- cuyahoga.model.df %>%
  select(term, estimate) %>% 
  filter(str_detect(term, "Zip")) %>% 
  mutate(zip = substr(term, 5, 9)) %>% 
  select(region = zip, value = estimate)

## Create choro plot

ohio_fips <-  39035
choro = ZipChoropleth$new(rev(cuyahoga.model.df))
choro$title = "Cuyahoga County Life Expectancy at Age 65"
choro$set_zoom_zip(state_zoom = NULL, county_zoom = ohio_fips, msa_zoom = NULL, zip_zoom = NULL)

## Add on a boundary for cleveland. We will pull this from the Tigris database. 

## Shapefile must be downloaded from 
shapefile <- st_read("/90days/casey/Municipalities_WGS84__Tiled.shp") 
cleveland_shapefile <- shapefile %>%
  filter(MUNI_NAME == "Cleveland")

## Plot Cleveland
cleveland_plot <- choro$render() +
  theme(text=element_text(size=25)) + 
  scale_fill_brewer(name="E65", palette = "Greens", direction = 1, na.value="grey", drop=FALSE) +
  geom_sf(data = cleveland_shapefile,
          inherit.aes = FALSE, fill = NA, color = "black", size = 1) +
  annotate("text", x=-81.67908, y=41.48074, label="Cleveland", color = "black", size = 8) + 
  theme(text=element_text(size=25))


