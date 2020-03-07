## An example comparing foreign born and native longevity
## Using BUNMD

## 1) read in "complete" sample
## 2) create analysis variables
## 3) example 1: 1 cohort only
## 4) example 2: a bunch of cohorts
## 5) example 3: cubans by race
## 6) note on regression coefficients


## 1) read in "complete" sample
## Note: we already created a small file with !is.na(cweight)
library(data.table)
library(memisc)
source("coef_plot_fun.R")

path <- "~/Documents/data/matched_data/"
dt <- fread(paste0(path, "small_berkeley_unified_mortality_file_v0.csv"))
nrow(dt)
## [1] 18982919 (seems big!)

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

dt[race == 1, Race := "White"]
dt[race == 2, Race := "Black"]
dt[race == 3, Race := "Other"]
dt[race == 4, Race := "Asian"]
dt[race == 5, Race := "Hispanic"]
dt[race == 6, Race := "Native"]
dt[, Race := relevel(as.factor(Race), ref = "White")] ## capital "W"?


dt[, country := relevel(factor(country), ref = "USA")]


## 3) example 1: 1 cohort only

#####################################
## example 1: Cohort born in 1915  ##
#####################################

tab <- table(dt[byear == 1915]$death_age)

head(tab)
##    72    73    74    75    76    77
## 20259 42086 44279 45199 47473 50175
tail(tab)
##    85    86    87    88    89    90
## 62710 62744 60327 59182 54581 23996

## Notice: the end-ages have only half the deaths, because of Lexis
## triangles.

my.dt.1 <- dt[byear == 1915 &
            death_age %in% 73:89 & ## we omit first and last ages
            age_first_application < 65] ## we restrict to those in the US before age 65
##
## male
m.immig.male <- lm(death_age ~ country,
                   data = my.dt.1,
                   weight = cweight,
                   subset = sex  == 1)
## female
m.immig.female <- update(m.immig.male,
                         subset = sex == 2)
##
## figure
##  pdf("birth_place_fig1.pdf", width = 6, height = 6)

par(mfrow = c(2,1),
    mar = c(5, 4, 4, 2) +.1)
color_coef_plot.fun(m.immig.female, "country", labeled_color.vec,
                    xlim = c(-1, 2))
text(0, 16.5, "Natives", pos = 2, col = "darkgrey", cex = .6)
title("Foreign-born female ages of death, cohort 1915")
color_coef_plot.fun(m.immig.male, "country", labeled_color.vec,
                    xlim = c(-1, 2))
title("Foreign-born male ages of death, cohort 1915")
text(0, 16.5, "Natives", pos = 2, col = "darkgrey", cex = .6)
## dev.off()
## system("open birth_place_fig1.pdf")

## sample size table

tab.1 <- my.dt.1[, round(xtabs(cweight ~ country + sex))]
tab.2 <- my.dt.2[, round(xtabs(cweight ~ country + sex))]
format(cbind(tab.1, tab.2), big.mark = ",")

#########################################
## example 2: Cohorts born in 1910-19  ##
#########################################
my.dt.2 <- dt[byear %in% 1910:1919 &
            dyear %in% 1988:2005 &
            age_first_application < 65]
## male
m.immig.male.2 <- lm(death_age ~ country +  Byear,
                   data = my.dt.2,
                   weight = cweight,
                   subset = sex  == 1)
## female
m.immig.female.2 <- update(m.immig.male.2,
                         subset = sex == 2)

## figure
pdf("birth_place_fig2.pdf", width = 6, height = 6)
par(mfrow = c(2,1),
    mar = c(5, 4, 4, 2) +.1)
color_coef_plot.fun(m.immig.female.2, "country", labeled_color.vec,
                    xlim = c(-1, 2))
text(0, 16.5, "Natives", pos = 2, col = "darkgrey", cex = .6)
title("Foreign-born female ages of death, cohorts 1910-19")
color_coef_plot.fun(m.immig.male.2, "country", labeled_color.vec,
                    xlim = c(-1, 2))
title("Foreign-born male ages of death, cohorts 1910-19")
text(0, 16.5, "Natives", pos = 2, col = "darkgrey", cex = .6)
dev.off()
system("open birth_place_fig2.pdf")


## sample size table
tab.1 <- my.dt.1[, round(xtabs(cweight ~ country + sex))]
tab.2 <- my.dt.2[, round(xtabs(cweight ~ country + sex))]
format(cbind(tab.1, tab.2), big.mark = ",")


##########################################################
## example 3: Racial disparities among Cuban Immigrants ##
##########################################################

my.dt.3 <- dt[country %in% c("Cuba") &
              byear %in% 1900:1920 &
              dyear %in% 1988:2005 &
              age_first_application < 65]

m.cuba.m <-  lm(death_age ~ Race +  Byear,
                data = my.dt.3,
                subset = sex == 1,
                weight = cweight)
m.cuba.f <- update(m.cuba.m,
                   subset = sex == 2)
mtable(m.cuba.m, m.cuba.f)


## check to see what happens if we restrict only to 1-time appliers
m.cuba.f.tilde <- update(m.cuba.f,
                         subset = sex == 2 & number_apps == 1)
m.cuba.m.tilde <- update(m.cuba.f,
                         subset = sex == 1 & number_apps == 1)
mtable(m.cuba.f, m.cuba.f.tilde, m.cuba.m, m.cuba.m.tilde)

## check on "race_change"
m.cuba.m.star <- update(m.cuba.m, . ~ . + race_change)
m.cuba.f.star <- update(m.cuba.m.star,
                        subset = sex == 2)
mtable(m.cuba.m, m.cuba.f, m.cuba.m.star, m.cuba.f.star)

## get top 20 names by race
get.top20 <- function(dt)
{
    foo <- dt[ , .N, by = fname]
    bar <- foo[order(-N)][1:20]
    bar
}

get.top20(my.dt.3[Race == "White"])
get.top20(my.dt.3[Race == "Hispanic"])
get.top20(my.dt.3[Race == "Black"])

foo <- my.dt.3[ , .N, by = .(fname, Race)]
foo[order(-N)][1:30]

## we can do indirectly to see if there's eni variance and if that predicts name

tt <- my.dt.3[, table(fname, Race)]
ptt <- round(prop.table(tt, 2), 6)

tth <- my.dt.3[Race %in% c("White", "Hispanic") & sex == 1, table(fname, Race == "Hispanic")]
ptth <- prop.table(tth, 2)
eni_hisp.vec <- ptth[,"TRUE"]/(ptth[, "TRUE"] + ptth[, "FALSE"])
N.vec <- apply(tth, 1, sum)
## pyramid
s <- N.vec > 20
par(mfrow = c(1,1))
plot(eni_hisp.vec[s], N.vec[s], log = "y", type = "n")
text(eni_hisp.vec[s], N.vec[s], names(eni_hisp.vec)[s], cex = .5)

tth <- my.dt.3[Race %in% c("White", "Hispanic") & sex == 2, table(fname, Race == "Hispanic")]
ptth <- prop.table(tth, 2)
eni_hisp.vec <- ptth[,"TRUE"]/(ptth[, "TRUE"] + ptth[, "FALSE"])
N.vec <- apply(tth, 1, sum)
## pyramid
s <- N.vec > 20
par(mfrow = c(1,1))
plot(eni_hisp.vec[s], N.vec[s], log = "y", type = "n")
text(eni_hisp.vec[s], N.vec[s], names(eni_hisp.vec)[s], cex = .5)


eni.dt <- data.table(fname = names(eni_hisp.vec), "eni_hisp" = eni_hisp.vec, "N" = N.vec)

out <- merge(my.dt.3,  eni.dt, by = "fname")

## model
m.cuba.eni <- lm(death_age ~ eni_hisp + Byear + as.factor(sex),
                 data = out,
                 subset = N >= 25 & sex == 2)
mtable(m.cuba.eni)


m.cuba.eni.f <- lm(death_age ~ eni_hisp + Byear,
                 data = out,
                 subset = N >= 25 & sex == 2)
m.cuba.eni.m <- update(m.cuba.eni.f,
                       subset = N >= 25 & sex == 1)
mtable(m.cuba.eni, m.cuba.eni.m, m.cuba.eni.f)


## check names

my.dt.3[Race == "Asian", .( fname, lname, mother_fname,  mother_lname, father_lname)]

########## all done
