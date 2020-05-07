# Example of using the many.models package with multicore bootstrapping.

# Load packages
library(many.models)
library(parallel)
library(datasets)
library(dplyr)

# Set options
options(mc.cores = 2)

# Load data
data("mtcars")
df <- mtcars

# Create bootstraps
set.seed(1)
n <- 4
boots <- mclapply(1:n, function(i) df[base::sample(1:nrow(df), replace = T),])

# Create formulas
formulas <- c('mpg ~ cyl', 'mpg ~ cyl + disp', 'mpg ~ cyl + disp + hp')

# Run many models on each bootstrap sample and combine into a single dataframe
df.long <- do.call('rbind', mclapply(seq_along(boots), function(i) {
  out <- extract.elem(.data = boots[[i]], .formula = formulas,
                      .fun = 'lm', elem = 'coefficients')
  out$dataset <- i
  out
  })
)

# Reshape to wide format
df.wide <- elem.to.wide(df, idvar = c('dataset', 'formula'))
df.wide

# Summarize mean, LCI, and UCI by formula and variable
alpha <- 0.05
df.mean <- df.long %>% group_by(formula, variable) %>%
  summarise(mean = mean(value),
            LCI = quantile(value, c(alpha/2)),
            UCI = quantile(value, c(1 - alpha/2)))
df.mean
