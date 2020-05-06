# Many Models

## Automating Many Models

This package provides helper functions for automating modeling of "many models".

* [extract.elem](R/extract.elem.R): Extract an element from a model fit.
* [elem.to.wide](R/elem.to.wide): Reshape element results to "wide".

## Package installation

You can either use `devtools` or an alternative like `pacman`:

```
devtools::install_github("brianhigh/many.models")
library(many.models)
```

```
pacman::p_load_gh("brianhigh/many.models")
```

## Example usage

```
library(datasets)
data(mtcars)

formulas <- c('mpg ~ cyl', 'mpg ~ cyl + disp', 'mpg ~ cyl + disp + hp')
df <- extract.elem(.data = mtcars, .formula = formulas, 
                   .fun = 'lm', elem = 'coefficients')
df
```

```
##         value    variable               formula
## 1 37.88457649 (Intercept)             mpg ~ cyl
## 2 -2.87579014         cyl             mpg ~ cyl
## 3 34.66099474 (Intercept)      mpg ~ cyl + disp
## 4 -1.58727681         cyl      mpg ~ cyl + disp
## 5 -0.02058363        disp      mpg ~ cyl + disp
## 6 34.18491917 (Intercept) mpg ~ cyl + disp + hp
## 7 -1.22741994         cyl mpg ~ cyl + disp + hp
## 8 -0.01883809        disp mpg ~ cyl + disp + hp
## 9 -0.01467933          hp mpg ~ cyl + disp + hp
```

```
df.wide <- elem.to.wide(df)
df.wide
```

```
##                 formula (Intercept)       cyl        disp          hp
## 1             mpg ~ cyl    37.88458 -2.875790          NA          NA
## 2      mpg ~ cyl + disp    34.66099 -1.587277 -0.02058363          NA
## 3 mpg ~ cyl + disp + hp    34.18492 -1.227420 -0.01883809 -0.01467933
```

