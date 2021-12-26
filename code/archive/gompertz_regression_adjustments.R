#########################################################
# Calculate BUNMD Gompertz Regression Adjustment Factors
######################################################### 

## library Functions
library(data.table)
library(memisc)
library(tidyverse)

## Source Functions
source("Documents/papers/bunmd_paper/code/00_helper_functions.R")

b = 1/10
a = 10^-4

## Function to get bunmd adjustment factors
get.bunmd.adjust.factor <- function(byear.vec = 1988:2005,
                                    a, b = 1/10, M = 84,
                                    e65.diff = 1,
                                    N = 1 * 10^6)
{
  
  ## usage: input the birth years you ran your regression on
  ## and obtain an adjustment factor to multiply your regression coefficients by.
  
  ## optional: modify b and M based on some external knowledge
  
  ## hard code year.vec
  year.vec <- 1988:2005
  
  ## Notes: the defaults for e65.diff and N seem to work fine
  
  ## get M2
  M1 = ifelse(is.null(M), ab2M(a,b), M)
  e65.M1 <- get.ex.gomp.M(65, b = b, M = M1)
  M2 = get.M.from.ex(e65.target = e65.M1 + e65.diff, b = b)
  
  ## now compute FE regression on simulated data
  
  ## simulate data
  ## simulate a big data set with 1 million  per cohort
  
  
  ##       1988   2005
  ## 1910  78     95
  ## 1911  77     94
  ## 1912  76     93
  ## 1913  75     92
  ## 1914  74     91
  ## 1915  73     90
  ## 1916  72     89
  ## 1917  71     88
  ## 1918  70     87
  ## 1919  69     86
  l.vec <- 78:69
  r.vec <- 86:95
  
  ## construct l.vec and r.vec from year.vec
  l.vec <- min(year.vec) - byear.vec
  l.vec[l.vec < 65] <- 65
  r.vec <- max(year.vec) - byear.vec
  r.vec[r.vec > 100] <- 100
  
  ## simulating
  print("simulating: please be patient")
  x1 <- rgompertz.M(N = N,
                    b = b,
                    M = M1)
  x2 <- rgompertz.M(N = N,
                    b = b,
                    M = M2)
  ##
  x1.list <- vector("list", length(l.vec))
  x2.list <- vector("list", length(l.vec))
  names(x1.list) <- names(x2.list) <- byear.vec
  for (i in 1:length(l.vec))
  {
    x1.list[[i]] <- data.table(x = x1[x1 > l.vec[i] & x1 < r.vec[i]],
                               type = 1)
    x2.list[[i]] <- data.table(x = x2[x2 > l.vec[i] & x2 < r.vec[i]],
                               type = 2)
  }
  ##
  dt.1 <- rbindlist(x1.list, idcol = TRUE)
  dt.2 <- rbindlist(x2.list, idcol = TRUE)
  dt <- rbindlist(list(dt.1, dt.2))
  
  ## run regression
  print("regressing: please continue to be patient")
  ##     m <- dt[, lm(x ~ as.factor(.id) + as.factor(type), subset = .id %in% 1910:1919)]
  ##    regression.beta <- coef(m)["as.factor(type)2"]
  ## faster
  dt[.id %in% byear.vec, year_mean := mean(x), by = .id]
  dt[, x_demean := x - year_mean]
  foo <- dt[, .(effect = mean(x_demean)), by = type]
  regression.beta.2 <- diff(foo$effect)
  adjust.factor.2 = e65.diff/regression.beta.2
  ##    print(adjust.factor.2)
  ##     adjust.factor = e65.diff/regression.beta
  return(round(adjust.factor.2, 2))
}


## plot adjustment factors by birth year
byear.vec <- 1890:1930
adjust.factor.vec <- rep(NA, length(byear.vec))
for (i in 1:length(byear.vec))
{
  print(byear.vec[i])
  adjust.factor.vec[i] <- get.bunmd.adjust.factor(byear.vec[i])
}

plot(byear.vec, adjust.factor.vec) # crazy aver 1930 or so
abline(h = 10)
s <- byear.vec %in% 1895:1920
plot(byear.vec[s], adjust.factor.vec[s]) # crazy aver 1930 or so

set.seed(12)
print(get.bunmd.adjust.factor(1895:1920))
## [1] 3.46
## other seeds
## [1] 3.42
## [1] 3.38

##
e65.diff.vec <- c(.1, .5, 1, 2, 4)
adjust.factor.vec <- rep(NA, length(e65.diff.vec))
for (i in 1:length(e65.diff.vec))
{
  adjust.factor.vec[i] <-
    get.bunmd.adjust.factor(1895:1920, e65.diff = e65.diff.vec[i])
}

## [1] 3.42 3.44 3.38 3.29 3.41
## [1] 3.41 3.41 3.34 3.32 3.33
## [1] 3.22 3.37 3.35 3.38 3.35


## now do varying M and varying beta -- table in paper

beta.vec <- seq(.08, .12, .01)
M.vec <- 77:87
adjust.factor.mat <- matrix(NA, length(M.vec), length(beta.vec))
dimnames(adjust.factor.mat) <- list(M.vec, beta.vec)
e65.diff.vec <- c(.1, .5, 1, 2, 4)
adjust.factor.vec <- rep(NA, length(e65.diff.vec))
for (i in 1:length(M.vec))
  for (j in 1:length(beta.vec))
  {
    adjust.factor.mat[i,j] <-
      get.bunmd.adjust.factor(1895:1920, b = beta.vec[j],
                              M = M.vec[i])
  }

adjust.factor.mat

## Regression coefficient adjustment factors for combined birth
## cohorts 1895 to 1920, estimated by simulation for different
## Gompertz parameter values $b$ and $M$.

#    0.08 0.09 0.1 0.11 0.12
# 77  3.0  2.5 2.2  2.1  1.9
# 78  3.2  2.7 2.4  2.2  2.0
# 79  3.3  2.9 2.5  2.4  2.1
# 80  3.5  3.0 2.8  2.4  2.2
# 81  3.9  3.2 2.8  2.5  2.4
# 82  4.1  3.3 3.1  2.6  2.5
# 83  4.3  3.7 3.2  2.9  2.6
# 84  4.7  3.8 3.4  3.1  2.7
# 85  4.9  4.2 3.6  3.2  2.9
# 86  5.6  4.6 3.9  3.4  3.0
# 87  6.1  5.0 4.2  3.7  3.3
