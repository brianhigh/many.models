# Example multicore bootstrapping using the "boot" package.

library(boot)
library(datasets)
library(dplyr)

# Load data
data("mtcars")
df <- mtcars

num_cores <- 2
nboot <- 4

formulas <- c('mpg ~ cyl', 'mpg ~ cyl + disp', 'mpg ~ cyl + disp + hp')

stat.fun <- function(data, ind, formula, model.fun, elem, ...) {
  ind <- sample(ind, replace = TRUE)
  do.call(model.fun, list(formula, data[ind,], ...))[[elem]]
}

clean.fun <- function(df, f) {
  df$dataset <-row.names(df)
  df$formula <- f
  n <- ncol(df)-2
  df <- df[, c('formula', 'dataset', names(df)[1:n])]

  df <- stats::reshape(
    df,
    varying = list(names(df[, 3:ncol(df)])),
    times = names(df[, 3:ncol(df)]),
    timevar = 'variable',
    idvar = c('formula', 'dataset'),
    ids = row.names(df),
    direction = "long"
  )

  row.names(df) <- NULL
  names(df) <- c('formula', 'dataset', 'variable', 'value')
  df
}

df.long <- do.call('rbind', lapply(formulas, function(f) {
  myBoot <- boot(data = df, statistic = stat.fun, R = nboot,
                 parallel = 'multicore', ncpus = num_cores,
                 formula = f, model.fun = 'lm', elem = 'coefficients')
  out <-as.data.frame(myBoot$t)
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
