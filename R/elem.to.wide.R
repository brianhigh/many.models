#' Reshape a Model Fit Element
#'
#' Reshapes as extracted model fit element data frame to "wide" format.
#'
#' @param .data Data frame with "variable", "value", and "formula" columns.
#' @param idvar Names of one or more grouping variables, passed to reshape().
#' @param strip String (literal) to remove from variable names in result.
#'
#' @return Data frame in "Wide" format.
#'
#' @examples
#' library(datasets)
#' data(mtcars)
#' formulas <- c('mpg ~ cyl', 'mpg ~ cyl + disp', 'mpg ~ cyl + disp + hp')
#' df <- extract.elem(.data = mtcars, .formula = formulas,
#'                    .fun = 'lm', elem = 'coefficients')
#' df.wide <- elem.to.wide(df)
#' df.wide
#'
#' df.sum <- extract.elem(.data = mtcars, .formula = formulas, .fun = "lm",
#'                        elem = "coefficients", mod.sum = TRUE)
#' df.wide <- elem.to.wide(df.sum[, c("formula", "variable", "Estimate")],
#'                         strip = "Estimate.")
#' df.wide
#'
#' @export
elem.to.wide <- function(.data, idvar = 'formula', strip = 'value.') {
  df.wide <- stats::reshape(.data,
                 timevar = 'variable',
                 idvar = idvar,
                 direction = 'wide')
  names(df.wide) <- gsub(strip, '', names(df.wide), fixed = TRUE)
  row.names(df.wide) <- NULL
  df.wide
}

