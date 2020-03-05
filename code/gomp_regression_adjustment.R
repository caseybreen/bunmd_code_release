## code for get.bunmd.adjust.factor()

## a figure
## pdf("bunmd_lexis.pdf", height = 6, width = 6)
par(mfrow = c(1,1))
plot(x = c(1890, 2010),
     y = c(0, 110),
     ylim = c(55, 110),
     xlim = c(1985, 2010),
     axes = F,
     xlab = "Year",
     ylab = "Age",
     type = "n")
axis(1)
## abline(v = c(1988, 2005))
segments(x0 = 1988, x1 = 2005,
         y0 = 65, lwd = 3)
segments(x0 = 1988, x1 = 1988,
         y0 = 65, y1 = 105, lwd = 3)
segments(x0 = 2005, x1 = 2005,
         y0 = 65, y1 = 105, lwd = 3)
byear.vec <- seq(1895, 1940, 5)
for (i in 1:length(byear.vec))
{
    by <- byear.vec[i]
    print(by)
    segments(x0 = by,
             x1 = by + 105,
             y0 = 0, y1 = 105,
             col = "grey")
}
text(rep(1988, 8), seq(68, by = 5, length.out = 8),
     seq(68, by = 5, length.out = 8), pos = 2,
     cex = .6, col = "black")
text(rep(2005, 8), seq(-15 + 68 + 17, by = 5, length.out = 8),
     seq(-15 + 68 + 17, by = 5, length.out = 8),
     cex = .6, col = "black", pos = 4)
## label cohorts
text(byear.vec + 1997 - byear.vec, y = 0 + 1997 - byear.vec,
     byear.vec, cex = .6, srt = 30, pos = 3, offset = .1,
     col = "grey")
s <- byear.vec %in% seq(1895, 1925, 5)
text(byear.vec[s] + 1997 - byear.vec[s], y = 0 + 1997 - byear.vec[s],
     byear.vec[s], cex = .6, srt = 25, pos = 3, offset = .1, font = 2)
text(c(1988, 1997, 2005), y = c(108, 108, 108),
     c("left age limit", "birth cohort", "right age limit"))
## dev.off()
## system("open bunmd_lexis.pdf")



library(data.table)
source("~/Documents/sandbox/gompertz_package/gomp_funs.R")
source("../gompertz_maxlik/hmd_validation_functions.R")

b = 1/10
a = 10^-4

pgomp.M <-
    function(x,b, M = NULL)
{
    if (!is.null(M)) {
        a <- Mb2a(M = M, b = b)
    }
    Hx <- (a/b) * (exp(b*x) - 1)
    lx <- exp(-Hx)
    Px <-  1 - lx
    return(Px)
}
sgomp.M <- function(x, b, M)
{
    1 - pgomp.M(x, b, M)
}

get.ex.gomp.M <- function(x, b, M = NULL)
{
    Tx <- integrate(f = sgomp.M, b = b, M = M, lower = x, upper = Inf)$value
    lx <- sgomp.M(x, b = b, M = M)
    ex <- Tx/lx
    return(ex)
}
get.ex.gomp.M(65, b = 1/10, M = 80)



get.M.from.ex <- function(e65.target, b)
{
    f <- function(M, b, e65.target) {
        out <- e65.target - get.ex.gomp.M(x = 65,
                                   b = b,
                                   M)
        return(out)
    }
    M.target <- uniroot(f, b = b, e65.target = e65.target,
                    c(60, 100))$root
    return(M.target)
}

## figure of two different death schedules


get.M.from.ex(17, b = 1/10)
get.M.from.ex(18, b = 1/10)



## get.M.from.ex(17, b = 1/10)
## [1] 83.95157
## > get.M.from.ex(18, b = 1/10)
## [1] 85.27804

Dx1 <- diff(pgomp(65:110, b = 1/10,  M = 83.95157))
Dx2 <- diff(pgomp(65:110, b = 1/10,  M = 85.27804))

plot(65:109, Dx1, type = "l", xlim = c(60, 110))
lines(65:109, Dx2, lty = 2)
abline(v = c(70, 90), col = "grey")

x <- 65:109
s <- x %in% 70:89
trunc.x.bar.diff <-     sum((x * Dx2)[s])/sum(Dx2[s]) -
    sum((x * Dx1)[s])/sum(Dx1[s])




get.bunmd.adjust.factor <- function(byear.vec,
                                    a, b = 1/10, M = 84,
                                    e65.diff = 1,
                                    N = 1 * 10^6)
{

    ## usage: input the birth years you ran your regression on
    ## and obtain an adjustment factor to multiply your regression coefficients by.

    ## optional: modify b and M based on some external knowledge

    ## Notes: the defaults for e65.diff and N seem to work fine

    ## hard code year.vec
    year.vec <- 1988:2005
    e65.diff = 1

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
    r.vec <- 95:86

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
    print("regressing: please continue to e patient")
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

get.bunmd.adjust.factor(1915, b = 1/10, M = 80, N = 100000)
get.bunmd.adjust.factor(1915)
get.bunmd.adjust.factor(1925)
get.bunmd.adjust.factor(1920)

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


## now do varying M and varying beta

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

   0.08 0.09  0.1 0.11 0.12
77 2.95 2.54 2.24 2.10 1.93
78 3.16 2.73 2.44 2.17 2.00
79 3.27 2.88 2.52 2.36 2.12
80 3.48 2.96 2.76 2.39 2.21
81 3.86 3.16 2.80 2.55 2.36
82 4.10 3.31 3.08 2.65 2.46
83 4.30 3.74 3.22 2.88 2.56
84 4.71 3.83 3.38 3.06 2.70
85 4.92 4.24 3.59 3.20 2.88
86 5.64 4.58 3.94 3.44 3.03
87 6.14 4.96 4.24 3.67 3.26

## Regression coefficient adjustment factors for combined birth
## cohorts 1895 to 1920, estimated by simulation for different
## Gompertz parameter values $b$ and $M$.

   0.08 0.09 0.1 0.11 0.12
77  3.0  2.5 2.2  2.1  1.9
78  3.2  2.7 2.4  2.2  2.0
79  3.3  2.9 2.5  2.4  2.1
80  3.5  3.0 2.8  2.4  2.2
81  3.9  3.2 2.8  2.5  2.4
82  4.1  3.3 3.1  2.6  2.5
83  4.3  3.7 3.2  2.9  2.6
84  4.7  3.8 3.4  3.1  2.7
85  4.9  4.2 3.6  3.2  2.9
86  5.6  4.6 3.9  3.4  3.0
87  6.1  5.0 4.2  3.7  3.3
