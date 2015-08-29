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
#' @title Vectorised linear regression with regularisation
#'
#' @description \code{vlrr_est} Main function behind the vlrr package.
#'
#' @details Essential a wrapper function for the \code{optim} function..
#'
#' @param X Design matrix x.
#' @param y Vector of observations.
#' @param lambda Regularisation parameter.
#' @return Returns a list giving the original model call, the final gradient
#'   given by the last iteration of the \code{gradient} function call, and the
#'   final coefficients.
#'
#' @examples
#'
#' suppressPackageStartupMessages(
#' library(dplyr)
#' )
#'
#' @export


vlrr_est <- function(x, y, lambda, ...)

{

  theta <- rep(1,ncol(x))

  optim_out <- optim(
    par = theta,
    fn = function(t) cost(x, y, t, lambda),
    gr = function(t) gradient(x, y, t, lambda),
    method = "BFGS",
    control = list(
      trace = 0,
      REPORT = 1
    )
  )

  df = nrow(x) - ncol(x)

  # Calculate the variance

  sigma2 <- sum((y - x %*% optim_out$par)^2) / df

  # Compute covariance with sigma^2 * (X^{T}X)^-1. Note that Leisch (2009)
  # counsels against using this method of calculating the covariance matrix.
  # Instead he recommends using QR decomposition. As my present implementation
  # does not solve the normal equation (X^{T}X)^{−1}X^{T}y but instead relies on
  # optim I do not have the products of the QR decomposition to use in the
  # matrix inverse in the standrd lm implementation: vcov <- sigma2 *
  # chol2inv(qx$qr). It'll do for now... Note that this requires MASS::ginv().
  # Mildly faster to use crossprod than %*%

  vcov <- sigma2 * solve(crossprod(x,x))

  list(
    coefficients = optim_out$par,
    gradient = optim_out$value,
    convergence = optim_out$convergence,
    message = optim_out$message,
    vcov = vcov,
    sigma = sqrt(sigma2),
    df = df
  )
}
