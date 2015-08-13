#' @title Title
#'
#' @description \code{this_function} What does it do?.
#'
#' @details Give the details of the \code{code} here.
#'
#' @param parameter1 Does something...
#' @param parameter2 Also does something.
#' @return What do you get back?
#'
#' @examples
#'
#' library(dplyr)
#'
#' @export

print.summary.vlrr <- function(x, ...) {

  cat("Vectorised linear regression by optim:\n")
  cat("\n")
  cat("Call:\n")

  print(x$call)
  cat("\n")

  cat("Lambda:\n")

  print(x$lambda)
  cat("\n")

  printCoefmat(
    x$coefficients,
    P.value = FALSE,
    has.Pvalue = FALSE
  )
}
