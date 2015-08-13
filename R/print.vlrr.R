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

print.vlrr <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\nError:\n")
  print(x$error)
  cat("\nCoefficients:\n")
  print(x$coefficients)
}
