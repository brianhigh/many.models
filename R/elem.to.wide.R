#' Reshape a Model Fit Element
#'
#' Reshapes as extracted model fit element data frame to "wide" format.
#'
#' @param .data Data frame with "variable", "value", and "formula" columns.
#' @param idvar Names of one or more grouping variables, passed to reshape().
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
#' @export
elem.to.wide <- function(.data, idvar = 'formula') {
  df.wide <- stats::reshape(.data,
                 timevar = 'variable',
                 idvar = idvar,
                 direction = 'wide')
  names(df.wide) <- gsub('^value\\.', '', names(df.wide))
  row.names(df.wide) <- NULL
  df.wide
}

