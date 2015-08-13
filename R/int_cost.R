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
#' @title Cost function
#'
#' @description \code{cost()} .
#'
#' @details Internal function supplied to \code{optim} which calculates cost function
#'
#' @param X Design matrix X.
#' @param y Vector of observations.
#' @param theta Vector of starting values for coefficients of length \code{ncol(X)+1}.
#' @param lambda Regularisation parameter.
#'
#' @return Returns the cost of the current parameters theta.
#'
cost <- function(X, y, theta, lambda) {

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
