#########################################################
# Helper Functions for reconstructing the figures in BUNMD. 
#########################################################

## Gompertz Hazard Functions 
hgomp <- function(x, b, a)
{
  a * exp(b * x)
}

pgomp <- function(x,b, a)
{
  Hx <- (a/b) * (exp(b*x) - 1)
  lx <- exp(-Hx)
  1-lx
}

dgomp <- function(x, b, a)
{
  hx <- hgomp(x,b,a)
  lx <- 1-pgomp(x,b,a)
  dx <- hx*lx
  dx
}

idfgomp <- function(u, b, a)
{
  x = (1/b) * log(-log(1-u)*b/a + 1)
  x
}

## idfgomp(u = .5, b = .1, a = .001)
rgomp <- function(N, b, a)
{
  u <- runif(N)
  x <- idfgomp(u, b, a)
  x
}

## HMD Validation Functions

hgompertz.M <- function(x, b, M)
{
  b * exp(b * (x-M))
}

pgompertz.M <- function(x, b, M)
{
  Hx <- exp(b*x - b*M)
  lx <- exp(-Hx)
  1-lx
}

dgompertz.M <- function(x, b, M)
{
  hx <- hgompertz.M(x,b,M)
  lx <- 1-pgompertz.M(x,b,M)
  dx <- hx*lx
  dx
}
## a = b* exp(-b * M)

bM2a <- function(b, M)
{
  a = b* exp(-b * M)
  a
}

inverse.distribution.function.gomp <- function(u, b, a)
{
  -(  log(a) - log(b - log(1-u)*b) ) / b
}
igomp <- inverse.distribution.function.gomp

idfgomp <- function(u, b, a)
{
  x = (1/b) * log(-log(1-u)*b/a + 1)
  x
}
idfgomp(u = .5, b = .1, a = .001)

rgompertz2 <- function(N, b, a)
{
  u <- runif(N)
  x <- idfgomp(u, b, a)
  x
}

rgompertz.M <- function(N, b, M)
{
  a = bM2a(b,M)
  x = rgompertz2(N, b, a)
  x
}

d.counts.gomp.negLL <- function(par, Dx, x.left, x.right)
{
  ## discrte gomp neg log likelihood for
  ## counts of deaths by single years of age (Dx)
  if(is.null(names(Dx)))
    print("names(Dx) is NULL; please define with ages")
  b = exp(par[1])
  M = exp(par[2])
  age.x = as.numeric(names(Dx))
  num.vec.unwt <- pgompertz.M(age.x+1, b, M) - pgompertz.M(age.x, b, M)
  denom.vec.unwt <- pgompertz.M(x.right, b, M) - pgompertz.M(x.left, b, M)
  LL <- sum( Dx*( log(num.vec.unwt) - log(denom.vec.unwt)) )
  negLL = -LL
  return(negLL)
}

get.se.from.hess <- function(hess)
{
  fisher_info <- solve(hess)
  se.vec <- sqrt(diag(fisher_info))
  se.vec
}

counts.trunc.gomp.est <- function(Dx, x.left, x.right, b.start, M.start)
{
  par.start = c(log(b.start), log(M.start))
  my.control = list(trace = 0,
                    parscale = c(par.scale = par.start))
  
  fit <- optim(par = par.start,
               fn = d.counts.gomp.negLL,
               hessian = TRUE,
               Dx = Dx,
               x.left = x.left,
               x.right = x.right,
               control = my.control)
  fit$se <- get.se.from.hess(fit$hess)*exp(fit$par) ## because log
  
  return(fit)
  
}

get.ex <- function(x, b, M, N = 10^6)
{
  y = rgompertz.M(N, b , M)
  ex <- mean(y[y > x]) - x
  ex
}


## coef_plot_fun
## Functions for plotting regression coefficinets

extract_coef.fun <- function(m, string, kind = "Estimate")
{
  cc <- summary(m)$coef[,paste(kind)]
  ## select
  my.cc <- cc[grepl(string, names(cc))]
  ## remove string
  names(my.cc) <- gsub(string, "", names(my.cc))
  return(my.cc)
}

coef_plot.fun  <- function(m, string = "country", ...)
{
  coef.vec <- extract_coef.fun(m = m, string,
                               kind = "Estimate")
  se.vec <- extract_coef.fun(m = m, string,
                             kind = "Std. Error")
  ## order
  o <- order(coef.vec)
  coef.vec <- coef.vec[o]
  se.vec <- se.vec[o]
  ##
  dotchart(coef.vec, pch = 19, ...,
           cex = .6)
  segments(x0 = coef.vec - 2*se.vec,
           x1 = coef.vec + 2*se.vec,
           y0 = 1:length(coef.vec),
           lwd = 1)
  abline(v = 0, col = "grey")
}

color_coef_plot.fun  <- function(m, string = "country",
                                 labeled_color.vec, ...)
{
  coef.vec <- extract_coef.fun(m = m, string,
                               kind = "Estimate")
  se.vec <- extract_coef.fun(m = m, string,
                             kind = "Std. Error")
  ## order
  o <- order(coef.vec)
  coef.vec <- coef.vec[o]
  se.vec <- se.vec[o]
  ##
  dotchart(coef.vec, pch = 19, ...,
           color = labeled_color.vec[names(coef.vec)],
           cex = .6)
  ## keep points black
  points(coef.vec, seq(coef.vec), pch = 19, cex = .6)
  segments(x0 = coef.vec - 2*se.vec,
           x1 = coef.vec + 2*se.vec,
           y0 = 1:length(coef.vec),
           lwd = 1)
  abline(v = 0, col = "grey")
}

labeled_color.vec <- c("Italy" = "darkgreen",
                       "Greece" = "darkgreen",
                       "China" = "red",
                       "Cuba" = "blue",
                       "Portugal" = "darkgreen",
                       "Poland" = "black",
                       "Philippines" = "red",
                       "Russia" = "black",
                       "Jamaica" = "blue",
                       "Yugoslavia" = "black",
                       "Mexico" = "blue",
                       "Ireland" = "black",
                       "England" = "black",
                       "Germany" = "black",
                       "Czechoslovakia" = "black",
                       "Canada" = "black",
                       "Austria" = "black")

## Lexis function
## From Schöley, Jonas, and Frans Willekens. "Visualizing compositional data on the Lexis surface." 
## Demographic research 36 (2017): 627-658.

lexis <- function(data, fill_column, year_begin, age_begin,
                  breaks = c(0, .05, .1, .15, .2, .25, .3, .4, .5, .75, .9, 5),
                  labels = c("0 to 5%", 
                             "5 to 10%",
                             "10 to 15%",
                             "15 to 20%",
                             "20 to 25%",
                             "25 to 30%",
                             "30 to 40%",
                             "40 to 50%",
                             "50 to 75%",
                             "75 to 90%",
                             ">90% Death Coverage"
                  )) {
  # 
  fill_column <- enquo(fill_column)
  
  # discretize sex ratio
  data <- data %>%
    mutate(coverage_categorical =
             cut(!! fill_column,
                 breaks, labels,
                 include.lowest = FALSE))
  
  ## plot mortality sex ratio Lexis surface
  lexis_plot <- ggplot(data) +
    
    # heatmap
    geom_raster(aes(x = Year+1, y = Age+1,
                    fill = coverage_categorical)) +
    ## Lexis grid
    geom_hline(yintercept = seq(age_begin, 100, 10),
               alpha = 0.2, lty = "dotted") +
    geom_vline(xintercept = seq(year_begin, 2010, 10),
               alpha = 0.2, lty = "dotted") +
    geom_abline(intercept = seq(-100, 100, 10)-1910,
                alpha = 0.2, lty = "dotted") +
    
    scale_fill_brewer(name = NULL) +
    scale_x_continuous("Calendar Time (Period)", expand = c(0.02, 0),
                       breaks = seq(year_begin, 2010, 10)) +
    scale_y_continuous("Age", expand = c(0, 0),
                       breaks = seq(age_begin, 110, 10)) +
    guides(fill = guide_legend(reverse = TRUE)) +
    # coord
    coord_equal() +
    # theme
    theme_void() +
    theme(
      axis.text = element_text(colour = "black"),
      axis.text.y = element_text(size=15),
      axis.text.x = element_text(size = 16, angle = 45, hjust = .7), 
      plot.title = element_text(size=20, vjust = 2),
      legend.text = element_text(size = 12), 
      axis.title=element_text(size=14,face="bold")
    ) + 
    labs(x = "Calendar Time (Period)",
         y = "Age")
  
  return(lexis_plot)
}


pgomp.M <- function(x, b, M = NULL)
{
  if (!is.null(M)) {
    a <- bM2a(M = M, b = b)
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
  lx <- sgomp.M(x, b = b, M = M)
  Tx <- integrate(f = sgomp.M, b = b, M = M, lower = x, upper = Inf)$value
  ex <- Tx/lx
  return(ex)
}

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

get.bunmd.adjust.factor <- function(byear.vec = 1988:2005,
                                    a, b = 1/10, M = 84,
                                    e65.diff = 1,
                                    N = 1 * 10^6)
{
  
  ## usage: input the birth years you ran your regression on
  ## and obtain an adjustment factor to multiply your regression coefficients by.
  
  ## hard code year.vec
  year.vec <- 1988:2005
  
  ## get M2
  M1 = ifelse(is.null(M), ab2M(a,b), M)
  e65.M1 <- get.ex.gomp.M(65, b = b, M = M1)
  M2 = get.M.from.ex(e65.target = e65.M1 + e65.diff, b = b)

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


## Source .Rmd 

source_rmd = function(file, ...) {
  tmp_file = tempfile(fileext=".R")
  on.exit(unlink(tmp_file), add = TRUE)
  knitr::purl(file, output=tmp_file, quiet = T)
  source(file = tmp_file, ...)
}
