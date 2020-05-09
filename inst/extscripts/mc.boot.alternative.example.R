# Example multicore bootstrapping using the "boot" package.

# Load packages
library(boot)
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
    do.call(model.fun, list(formula, data[rand.ind,], ...))[[elem]]
}

# Define function to clean model output
clean.fun <- function(df, .formula) {
  df$boot <- row.names(df)
  df$formula <- .formula
  n <- ncol(df) - 2
  df <- df[, c('formula', 'boot', names(df)[1:n])]
  df <- melt(df, id.vars = c('formula', 'boot'))
  row.names(df) <- NULL
  names(df) <- c('formula', 'boot', 'variable', 'est')
  df
}

# Define function to calculate mean and confidence interval
mean.ci.fun <- function(x, alpha = 0.05) {
    c(mean = mean(x), quantile(x, probs = c(alpha / 2, 1 - alpha / 2)))
}

# Run model for all formulas with bootstraps
df.long <- do.call('rbind', lapply(formulas, function(f) {
  boot.res <- boot(data = df, statistic = stat.fun, R = nboot,
                   parallel = 'multicore', ncpus = num_cores,
                   formula = f, model.fun = 'lm', elem = 'coefficients')
  out <- as.data.frame(boot.res$t)
  names(out) <- names(boot.res$t0)
  clean.fun(out, f)
}))

# Summarize results by formula and variable
summaryBy(est ~ formula + variable, data = df.long, FUN = mean.ci.fun)
