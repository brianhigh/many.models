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

# Create boostraps
set.seed(1)
n <- 4
boot <- mclapply(1:n, function(i) df[base::sample(1:nrow(df), replace = TRUE),])

# Create formulas
formulas <- c('mpg ~ cyl', 'mpg ~ cyl + disp', 'mpg ~ cyl + disp + hp')

# Run many models on each bootstrap sample and combine into a single dataframe
df <- do.call('rbind', mclapply(seq_along(boot), function(i) {
  out <- extract.elem(.data = boot[[i]], .formula = formulas,
                      .fun = 'lm', elem = 'coefficients')
  out$dataset <- i
  out
  })
)

# Reshape to wide format
df.wide <- elem.to.wide(df, idvar = c('dataset', 'formula'))

# Summarize mean, LCI, and UCI by formula and variable
alpha <- 0.05
df %>% group_by(formula, variable) %>%
  summarise(mean = mean(value),
            LCI = quantile(value, c(alpha/2)),
            UCI = quantile(value, c(1 - alpha/2)))