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
#' @description \code{vlrr} Main function behind the vlrr package.
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
#' library(vlrr)
#' data(mpg, package="ggplot2")
#' model <- vlrr(mpg ~ disp, data = mtcars)
#' model
#'
#' @export

vlrr <- function(x, y, lambda, ...) UseMethod("vlrr")

vlrr.default <- function(x, y, lambda = 0, ...) {

  # Could put in some more verbose checks of the input here.

  x <- as.matrix(x)
  y <- as.numeric(y)
  lambda <- as.numeric(lambda)

  est <- vlrr_est(x, y, lambda)

  est$lambda = lambda
  est$fitted.values <- as.vector(x %*% est$coefficients)
  est$residuals <- y - est$fitted.values
  est$call <- match.call()

  class(est) <- "vlrr"
  est
}
