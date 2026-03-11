## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(earthUI)

set.seed(42)
n <- 2000
df <- data.frame(
  gas_gallons  = runif(n, 5, 30),
  electric_kwh = runif(n, 100, 500),
  labor_hours  = runif(n, 10, 80),
  region       = factor(sample(c("North", "South", "West"), n, replace = TRUE)),
  shift        = factor(sample(c("Day", "Night"), n, replace = TRUE)),
  product      = factor(sample(c("Canned", "Dried", "Frozen"), n, replace = TRUE))
)
df$region <- relevel(df$region, ref = "West")
df$cost <- 2000 +
  40 * pmax(0, df$gas_gallons - 15) + 25 * pmax(0, 15 - df$gas_gallons) +
  3 * pmax(0, df$electric_kwh - 300) +
  ifelse(df$region == "North", 400, ifelse(df$region == "South", 200, 100)) +
  ifelse(df$shift == "Night", 100, 0) +
  ifelse(df$product == "Frozen", 200, ifelse(df$product == "Dried", 100, 0)) +
  ifelse(df$region == "North", 15, ifelse(df$region == "South", 35, 5)) *
    pmax(0, df$gas_gallons - 12) +
  ifelse(df$shift == "Night", 5, 1) * pmax(0, df$electric_kwh - 250) +
  ifelse(df$region == "South" & df$shift == "Night", 1200,
         ifelse(df$region == "North" & df$shift == "Night", 600, 0)) +
  0.8 * pmax(0, df$gas_gallons - 15) * pmax(0, df$electric_kwh - 300) +
  0.02 * pmax(0, df$gas_gallons - 15) * pmax(0, df$electric_kwh - 300) *
    pmax(0, df$labor_hours - 40) +
  ifelse(df$region == "North", 0.2, ifelse(df$region == "South", 0.5, 0.05)) *
    pmax(0, df$gas_gallons - 12) * pmax(0, df$electric_kwh - 250) +
  (ifelse(df$region == "South" & df$shift == "Night", 35,
     ifelse(df$region == "North" & df$shift == "Night", 30, 0))) *
    pmax(0, df$gas_gallons - 10) +
  ifelse(df$product == "Frozen" & df$region == "South" & df$shift == "Night", 800,
    ifelse(df$product == "Frozen" & df$region == "North" & df$shift == "Night", 500,
      ifelse(df$product == "Dried" & df$region == "South" & df$shift == "Night", 300,
        ifelse(df$product == "Dried" & df$region == "North" & df$shift == "Night", 200, 0)))) +
  rnorm(n, 0, 30)

result <- fit_earth(
  df = df,
  target = "cost",
  predictors = c("gas_gallons", "electric_kwh", "labor_hours",
                 "region", "shift", "product"),
  categoricals = c("region", "shift", "product"),
  degree = 3,
  nk = 100
)

## -----------------------------------------------------------------------------
gf <- list_g_functions(result)
gf

## ----fig.width=7, fig.height=4------------------------------------------------
idx <- which(gf$g_j == 1 & gf$g_f == 0)[1]
plot_g_function(result, idx)

## ----fig.width=7, fig.height=4------------------------------------------------
idx <- which(gf$g_j == 1 & gf$g_f == 1)[1]
plot_g_function(result, idx)

## ----fig.width=7, fig.height=5------------------------------------------------
idx <- which(gf$g_j == 2 & gf$g_f == 0)[1]
plot_g_contour(result, idx)

## ----fig.width=7, fig.height=4------------------------------------------------
idx <- which(gf$g_j == 2 & gf$g_f == 1)[1]
plot_g_function(result, idx)

## ----fig.width=7, fig.height=4------------------------------------------------
idx <- which(gf$g_j == 2 & gf$g_f == 2)[1]
plot_g_function(result, idx)

## ----fig.width=7, fig.height=5------------------------------------------------
idx <- which(gf$g_j == 3 & gf$g_f == 0)[1]
if (!is.na(idx)) plot_g_contour(result, idx)

## ----fig.width=7, fig.height=5------------------------------------------------
idx <- which(gf$g_j == 3 & gf$g_f == 1)[1]
if (!is.na(idx)) plot_g_contour(result, idx)

## ----fig.width=8, fig.height=5------------------------------------------------
idx <- which(gf$g_j == 3 & gf$g_f == 2)[1]
if (!is.na(idx)) plot_g_function(result, idx)

## ----fig.width=12, fig.height=5-----------------------------------------------
idx <- which(gf$g_j == 3 & gf$g_f == 3)[1]
if (!is.na(idx)) plot_g_function(result, idx)

