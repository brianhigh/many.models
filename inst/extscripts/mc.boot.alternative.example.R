# Example multicore bootstrapping using the "boot" package.

# Load packages
library(boot)
library(datasets)
library(reshape2)
library(doBy)

# Load data
data("mtcars")

# Define variables
df <- mtcars
num_cores <- 2
nboot <- 100
formulas <- c('mpg ~ cyl', 'mpg ~ cyl + disp', 'mpg ~ cyl + disp + hp')

# Define function to run model and return variables of interest
stat.fun <- function(data, ind, formula, model.fun, elem, cols, ...) {
  rand.ind <- sample(ind, replace = TRUE)
  summary(
    do.call(model.fun, list(formula, data[rand.ind,], ...))
  )[[elem]][, cols]
}

# Define function to clean model output
clean.fun <- function(df, f) {
  df$dataset <- row.names(df)
  df$formula <- f
  n <- ncol(df) - 2
  df <- df[, c('formula', 'dataset', names(df)[1:n])]
  df <- melt(df, id.vars = c('formula', 'dataset'))
  row.names(df) <- NULL
  names(df) <- c('formula', 'dataset', 'variable', 'value')
  df
}

# Define function to summarize results
summarize.fun <- function(x, alpha = 0.05) {
  list(est.mean = mean(x, na.rm = TRUE),
       est.LCI = c(quantile(x, c(alpha / 2))),
       est.UCI = c(quantile(x, 1 - c(alpha / 2))))
}

# Run model for all formulas with bootstraps
df.long <- do.call('rbind', lapply(formulas, function(f) {
  myBoot <- boot(data = df, statistic = stat.fun, R = nboot,
                 parallel = 'multicore', ncpus = num_cores,
                 formula = f, model.fun = 'lm', elem = 'coefficients',
                 cols = 'Estimate')
  out <- as.data.frame(myBoot$t)
  names(out) <- names(myBoot$t0)
  clean.fun(out, f)
}))

# Summarize results by formula and variable
summaryBy(value ~ formula + variable, data = df.long, FUN = summarize.fun)
