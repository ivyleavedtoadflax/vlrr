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

gR <- function(X, y, theta, lambda) {

  theta1 <- theta
  theta1[1] <- 0

  m <- length(y)

  error <- tcrossprod(theta,X)
  error <- as.vector(error) - y
  error <- (1/m) * crossprod(error,X)

  reg <- (lambda/(m)) * theta1

  delta <- error + reg

  return(delta)

}
