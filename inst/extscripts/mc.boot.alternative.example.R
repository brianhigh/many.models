# Example multicore bootstrapping using the "boot" package.

# Load packages
library(boot)
library(datasets)
library(dplyr)

# Load data
data("mtcars")

# Define variables
df <- mtcars
num_cores <- 2
nboot <- 4
formulas <- c('mpg ~ cyl', 'mpg ~ cyl + disp', 'mpg ~ cyl + disp + hp')

# Define function to run model
stat.fun <- function(data, ind, formula, model.fun, elem, ...) {
  rand.ind <- sample(ind, replace = TRUE)
  do.call(model.fun, list(formula, data[rand.ind,], ...))[[elem]]
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

# Run model for all formulas with bootstraps
df.long <- do.call('rbind', lapply(formulas, function(f) {
  myBoot <- boot(data = df, statistic = stat.fun, R = nboot,
                 parallel = 'multicore', ncpus = num_cores,
                 formula = f, model.fun = 'lm', elem = 'coefficients')
  out <- as.data.frame(myBoot$t)
  names(out) <- names(myBoot$t0)
  clean.fun(out, f)
}))

# Summarize mean, LCI, and UCI by formula and variable
alpha <- 0.05
df.mean <- df.long %>% group_by(formula, variable) %>%
  summarise(mean = mean(value),
            LCI = quantile(value, c(alpha/2)),
            UCI = quantile(value, c(1 - alpha/2)))
df.mean
