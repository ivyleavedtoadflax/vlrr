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


J <- function(X, y, theta, lambda) {

  m <- length(y)

  theta1 <- theta

  # Ensure that regularisation is not operating on \theta_0

  theta1[1] <- 0

  error <- tcrossprod(theta,X)
  error <- as.vector(error) - y
  error1 <- crossprod(error,error)

  reg <- (lambda/(2*m)) * crossprod(theta1, theta1)

  cost <- (1/(2 * m)) * error1 + reg

  return(cost)

}
