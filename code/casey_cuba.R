## Cuba Analysis

## (1) read in data and code variables
library(memisc)
library(data.table)
path <- "~/Documents/data/matched_data/"
dt <- fread(paste0(path, "berkeley_unified_numident_mortality_file_v01_complete.csv"))
nrow(dt)
## [1] 18982919

## 2) create analysis variables

## code countries with a lot of immigrants
dt[, country := NULL]
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
## dt[bpl ==  50220, country := "Korea"]
## dt[bpl ==  50100, country := "Japan"]
dt[bpl ==  51500, country := "Philippines"]
## dt[bpl ==  54100, country := "Syria"]
dt[bpl < 1300,    country := "USA"]

## prepare variables for regression
dt[, Byear := as.factor(byear)]
dt[, Byear := relevel(Byear, ref = "1910")]

dt[race_last == 1, Race := "White"]
dt[race_last == 2, Race := "Black"]
dt[race_last == 3, Race := "Other"]
dt[race_last == 4, Race := "Asian"]
dt[race_last == 5, Race := "Hispanic"]
dt[race_last == 6, Race := "Native"]
dt[, Race := relevel(as.factor(Race), ref = "White")] ## capital "W"?

## Set reference group
dt[, country := relevel(factor(country), ref = "USA")]

## restrict to years with high coverage
my.dt.3 <- dt[country %in% c("Cuba") &
              byear %in% 1900:1920 &
              dyear %in% 1988:2005 &
              age_first_application < 65]

## (2) look at the counts of Hispanic etc
## by date of last app which race came from
## to see if we can get more Hispanics than
## we did by conditioning on 1 app.

my.dt.3[, table(race_last_cyear, Race)]

## we see a bunch of Hispanic from 80 to 88
## but also a bunch after 88, particularly in 1997.
## Cuban immigraiton in 97?

## (2) do the regression
m.cuba.m.late <-  lm(death_age ~ Race +  Byear,
                data = my.dt.3,
                subset = sex == 1 & race_last_cyear %in% 1980:1987,
                weight = cweight)
m.cuba.f.late <- update(m.cuba.m.late,
                   subset = sex == 2 & race_last_cyear %in% 1980:1987)

m.cuba.m.early <-  lm(death_age ~ Race +  Byear,
                      data = my.dt.3,
                      subset = sex == 1 & race_last_cyear < 1980,
                      weight = cweight)
m.cuba.f.early <- update(m.cuba.m.early,
                   subset = sex == 2 & race_last_cyear < 1980)

mtable(m.cuba.f.late, m.cuba.m.late, m.cuba.f.early, m.cuba.m.early)

## check counts
my.dt.3[race_last_cyear < 1980, table(Race, sex)]
##           sex
## Race           1     2
##   White    15224 15612
##   Asian        0     0
##   Black      364   427
##   Hispanic     0     0
##   Native       0     0
##   Other      344   410

my.dt.3[race_last_cyear %in% 1980:1987, table(Race, sex)]
##           sex
## Race          1    2
##   White     605  523
##   Asian       0    3
##   Black      40   30
##   Hispanic 1154 1472
##   Native      0    1
##   Other      65   60

## ======================================================================================
##                         m.cuba.f.late  m.cuba.m.late  m.cuba.f.early  m.cuba.m.early
## --------------------------------------------------------------------------------------
##   (Intercept)              85.833***      83.639***       85.844***       83.725***
##                            (0.525)        (0.543)         (0.123)         (0.122)
##   Race: Asian/White        -3.692
##                            (2.961)
##   Race: Black/White        -2.148*        -1.065          -0.873***       -0.691*
##                            (1.050)        (0.878)         (0.247)         (0.275)
##   Race: Hispanic/White     -0.210         -0.157
##                            (0.300)        (0.293)
##   Race: Native/White      -10.095
##                            (5.604)
##   Race: Other/White        -0.445          1.335          -0.138          -0.093
##                            (0.771)        (0.745)         (0.238)         (0.256)
## ...
## --------------------------------------------------------------------------------------
##   R-squared                 0.297          0.305           0.257           0.256
##   N                      2089           1864           16449           15932
## ======================================================================================

## From the earlier period, we clearly see that Blacks are disadvantaged compared to Whites.
## For the later period, the vast majority choose Hispanic, perhaps even the Black Cubans,
## so we can't really say that much.

## I'm curious about the date of first application for the Cubans (the distribution)

my.dt.3[, year_first_app := byear + age_first_application]
my.dt.3[, hist(year_first_app)]

## So everyone has applied by 1980 or so.

## So we can run the whole thing on first race
## But note that in this version of BUNMD
## race_first is NA if only 1 app

## you can only be White, Black, or Other before 1980
my.dt.3[number_apps == 1 & race_last == 1 & race_last_cyear < 1980, Race_pre_1980 := "White"]
my.dt.3[number_apps > 1 & race_first == 1 & race_first_cyear < 1980, Race_pre_1980 := "White"]
##
my.dt.3[number_apps == 1 & race_last == 2 & race_last_cyear < 1980, Race_pre_1980 := "Black"]
my.dt.3[number_apps > 1 & race_first == 2 & race_first_cyear < 1980, Race_pre_1980 := "Black"]
##
my.dt.3[number_apps == 1 & race_last == 3 & race_last_cyear < 1980, Race_pre_1980 := "Other"]
my.dt.3[number_apps > 1 & race_first == 3 & race_first_cyear < 1980, Race_pre_1980 := "Other"]


my.dt.3[, Race_pre_1980 := relevel(factor(Race_pre_1980), ref = "White")]

m.cuba.m.pre_1980 <-  lm(death_age ~ Race_pre_1980 +  Byear,
                      data = my.dt.3,
                      subset = sex == 1,
                      weight = cweight)
m.cuba.f.pre_1980 <- update(m.cuba.m.pre_1980,
                   subset = sex == 2)
mtable(m.cuba.m.pre_1980, m.cuba.f.pre_1980)

my.dt.3[, table(Race_pre_1980, sex)]

## both sexes combined
m.cuba.mf.pre_1980 <-  lm(death_age ~ Race_pre_1980 +  Byear + as.factor(sex),
                      data = my.dt.3,
                      weight = cweight)
mtable(m.cuba.mf.pre_1980)


## For this analysis, we focus on the original answer that people gave to the race question. Most of these responses were prior to 1980, 
## when there were only 3 categories ("White", "Black", and "Other"). (A possible extension of this initial analysis would be to consider 
## responses to post-1980 race classification, which included "Hispanic", "Asian", and "Native American".
## However, analysis of the post-1980 classification is complicated by the fact that most of these respondents were filling out the form for a 2nd time
## proviwe want to focus on the pre-1980 responses
## to the race item. This is because, after 1980,
