# Example multicore bootstrapping using the "boot" package.

# Load packages
library(boot)
library(datasets)
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

# Define function to reshape model output
reshape.fun <- function(df) {
  stats::reshape(
    df,
    varying = list(names(df[, 3:ncol(df)])),
    times = names(df[, 3:ncol(df)]),
    timevar = 'variable',
    idvar = c('formula', 'dataset'),
    ids = row.names(df),
    direction = "long"
  )
}

# Define function to clean model output
clean.fun <- function(df, f) {
  df$dataset <- row.names(df)
  df$formula <- f
  n <- ncol(df) - 2
  df <- df[, c('formula', 'dataset', names(df)[1:n])]
  df <- reshape.fun(df)
  row.names(df) <- NULL
  names(df) <- c('formula', 'dataset', 'variable', 'value')
  df
}

# Define function to summarize results
summarize.fun <- function(x, alpha = 0.05) {
  est.mean <- mean(x, na.rm = TRUE)
  LCI <- c(quantile(x, c(alpha / 2)))
  UCI <- c(quantile(x, 1 - c(alpha / 2)))
  list(est.mean = est.mean, est.LCI = LCI, est.UCI = UCI)
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
summaryBy(value ~ formula + variable, df.long, summarize.fun)
