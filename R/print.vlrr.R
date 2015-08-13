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
#' @title Print method for \code{vlrr} function.
#'
#' @description \code{print.vlrr} Provides print method for the \code{vlrr}
#'   class..
#'
#' @details Simple method to reduce the size of output from the \code{vlrr}
#'   class..
#'
#' @param x A model fo class \code{vlrr}.
#' @return A list of length three giving model call, final gradient, and coefficients
#'
#' @examples
#'
#' library(vlrr)
#' data(mpg, package="ggplot2")
#'
#' model <- vlrr(mpg ~ disp, data = mtcars)
#' print(model)
#'
#' @export

print.vlrr <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\nGradient:\n")
  print(x$gradient)
  cat("\nCoefficients:\n")
  print(x$coefficients)
}
