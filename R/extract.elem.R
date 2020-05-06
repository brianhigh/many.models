#' Extract a Model Fit Element
#'
#' Extracts an element from a model fit of many models as a data frame.
#'
#' @param .data Data frame.
#' @param .formula Formula to use with .fun.
#' @param .fun Model function such as: "lm".
#' @param elem Element to extract from fit object, such as: "coefficients".
#' @param ... Additional options for .fun such as: family = "inverse.gaussian".
#'
#' @return Data frame combining an element of fit results for many models.
#'
#' @examples
#' library(datasets)
#' data(mtcars)
#'
#' formulas <- c('mpg ~ cyl', 'mpg ~ cyl + disp', 'mpg ~ cyl + disp + hp')
#'
#' set.seed(1)
#' weights <- c(abs(rnorm(nrow(mtcars))))
#'
#' df <- extract.elem(.data = mtcars, .formula = formulas,
#'                    .fun = 'lm', elem = 'coefficients')
#'
#' df.residuals <- extract.elem(.data = mtcars, .formula = formulas,
#'                              .fun = 'lm', elem = 'residuals')
#'
#' df.weighted <- extract.elem(.data = mtcars, .formula = formulas,
#'                             .fun = 'lm', elem = 'coefficients',
#'                             weights = weights)
#'
#' df.inv.gauss <- extract.elem(.data = mtcars, .formula = formulas,
#'                              .fun = 'glm', elem = 'coefficients',
#'                              family = "inverse.gaussian")
#'
#' @export
extract.elem <- function(.data, .formula, .fun, elem, ...) {
  as.data.frame(do.call('rbind', lapply(.formula, function(f) {
    df <- as.data.frame(
      do.call(.fun, list(formula = quote(f), data = .data, ...))[[elem]])
    names(df) <- c('value')
    df$variable <- row.names(df)
    df$formula <- f
    row.names(df) <- NULL
    df
  })), optional = TRUE)
}

