## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# library(earthUI)
# launch()

## -----------------------------------------------------------------------------
library(earthUI)

# For this example, we use the built-in mtcars dataset
df <- mtcars
head(df)

## ----eval=FALSE---------------------------------------------------------------
# df <- import_data("my_data.csv")        # CSV
# df <- import_data("my_data.xlsx")       # Excel

## -----------------------------------------------------------------------------
cats <- detect_categoricals(df)
cats

## -----------------------------------------------------------------------------
result <- fit_earth(
  df = df,
  target = "mpg",
  predictors = c("cyl", "disp", "hp", "wt", "qsec", "am", "gear"),
  categoricals = c("am", "gear"),
  degree = 1
)

## -----------------------------------------------------------------------------
# Model summary
s <- format_summary(result)
cat(sprintf("R²: %.4f\nGRSq: %.4f\nTerms: %d\n",
            s$r_squared, s$grsq, s$n_terms))

## -----------------------------------------------------------------------------
# Coefficients
s$coefficients

## -----------------------------------------------------------------------------
# Variable importance
format_variable_importance(result)

## -----------------------------------------------------------------------------
# ANOVA decomposition
format_anova(result)

## ----fig.width=7, fig.height=4------------------------------------------------
plot_variable_importance(result)

## ----fig.width=7, fig.height=4------------------------------------------------
plot_partial_dependence(result, "wt")

## ----fig.width=7, fig.height=4------------------------------------------------
plot_actual_vs_predicted(result)

## ----fig.width=7, fig.height=4------------------------------------------------
plot_residuals(result)

## -----------------------------------------------------------------------------
# Build default all-allowed matrix
preds <- c("wt", "hp", "cyl", "disp")
mat <- build_allowed_matrix(preds)

# Block wt-cyl interaction
mat["wt", "cyl"] <- FALSE
mat["cyl", "wt"] <- FALSE

# Convert to earth-compatible function
allowed_fn <- build_allowed_function(mat)

# Fit with interactions
result2 <- fit_earth(
  df = df,
  target = "mpg",
  predictors = preds,
  degree = 2,
  allowed_func = allowed_fn
)

s2 <- format_summary(result2)
cat(sprintf("Training R²: %.4f\nCV R²: %s\n",
            s2$r_squared,
            if (!is.na(s2$cv_rsq)) sprintf("%.4f", s2$cv_rsq) else "N/A"))

## ----eval=FALSE---------------------------------------------------------------
# render_report(result, output_format = "html", output_file = "my_report.html")

