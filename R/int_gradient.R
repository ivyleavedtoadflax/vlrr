#    vlrr: Vectorised linear regression with regularisation
#    A package for the R statistical environment
#    Copyright (C) 2015  Matthew Upson <ivyleavedtoadflax@gmail.com>
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#' @title Gradient function
#'
#' @description \code{gradient()} .
#'
#' @details Internal function supplied to \code{optim} which calculates gradient.
#'
#' @param X Design matrix X.
#' @param y Vector of observations.
#' @param theta Vector of starting values for coefficients of length \code{ncol(X)+1}.
#' @param lambda Regularisation parameter.
#'
#' @return Returns the gradient of the current parameters theta.
#'
#' @examples
#'
#' library(dplyr)
#' theta <- rep(1,2)
#' lambda <- 0
#' x <- mpg$displ
#' y <- mpg$hwy
#'
#' optim_out <- optim(
#'   par = theta,
#' fn = function(t) cost(cbind(1, x), y, t, lambda),
#' gr = function(t) gradient(cbind(1, x), y, t, lambda),
#' method = "BFGS"
#' )
#'

gradient <- function(X, y, theta, lambda) {

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
