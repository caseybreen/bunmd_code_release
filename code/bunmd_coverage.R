###################################################
# Plot BUNMD Coverage
###################################################

## Library Packages
library(data.table)
library(tidyverse)
library(HMDHFDplus)
library(gridExtra)
library(ggpubr)

## Install bbplot package from github
# install.packages('devtools')
# devtools::install_github('bbc/bbplot')
library(bbplot)

## Read in BUNMD file
bunmd <- fread("/censoc/data/numident/4_berkeley_unified_mortality_database/bunmd.csv")

## Get HMD deaths from website
hmd_deaths <-  readHMDweb(CNTRY = "USA", item = "Deaths_1x1", username ="caseybreen@berkeley.edu", password = "censoc") %>% mutate(linking_key = paste(Year, Age, sep = "_" ))

## Tabulate deaths in BUNMD for 65+
bunmd.deaths.tabulated <- bunmd %>% 
  filter(death_age >= 65) %>% 
  group_by(Year = dyear) %>% 
  filter(Year > 1960) %>% 
  summarize(deaths = n()) %>% 
  mutate(source = "BUNMD")

## Tabulate deaths in HMD for 65+ 
hmd.deaths.tabulated <- hmd_deaths %>%
  filter(Age >= 65) %>% 
  group_by(Year) %>% 
  filter(Year > 1960) %>% 
  summarize(deaths = sum(Total)) %>% 
  mutate(source = "Human Mortality Database")

## Combine into one data frame 
data.for.plot <- numident_deaths_age %>% 
  bind_rows(hmd.deaths.tabulated) %>% 
  filter(Year > 1970 & Year < 2006)

## Create death coverage plot
ggplot(data.for.plot) + 
  geom_line((aes(x = Year, y = deaths, linetype=source))) + 
  bbc_style() + #replace with a different theme (theme_bw()) if the bbplot package isn't downloaded 
  ggtitle("BUNMD Death Coverage 65+") + 
  theme(legend.position="bottom") +
  xlab("title") + 
  theme(axis.title = element_text(size = 17), 
        axis.text.y = element_text(size=17),
        axis.text.x = element_text(size=17)) +
  labs(x = "Year", 
       y = "Count") + 
  scale_y_continuous(labels = scales::comma) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n=5)) 