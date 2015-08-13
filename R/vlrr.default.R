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

vlrr <- function(x, ...) UseMethod("vlrr")

vlrr.default <- function(x, y, lambda = 0, ...) {

  x <- as.matrix(x)
  y <- as.numeric(y)
  lambda <- as.numeric(lambda)

  est <- vlrr_est(x, y, lambda)

  est$lambda = lambda
  est$fitted.values <- tcrossprod(est$coefficients, x)
  est$residuals <- y - est$fitted.values
  est$call <- match.call()

  class(est) <- "vlrr"
  est
}
