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
#' @title Print method for summary method for the \code{vlrr} function.
#'
#' @description \code{print.summary.vlrr} Print method for summary method for
#'   the \code{vlrr} function.
#'
#' @details Print method method for summary method of \code{vlrr} function.
#'
#' @param x A \code{summary.vlrr} object.
#' @param newdata If A list containing new training examples. See examples.
#' @return Prediction based on the model object and newdata.
#'
#' @examples
#'
#' library(vlrr)
#' data(mpg, package="ggplot2")
#'
#' model <- vlrr(hwy ~ displ, data = mpg)
#' summary(model)
#'
#' @export

print.summary.vlrr <- function(x, ...) {

  cat("Vectorised linear regression by optim:\n")
  cat("\n")
  cat("Convergence (see ?optim):\n")
  print(x$convergence)
  cat("Message (see ?optim):\n")
  print(x$message)

  cat("\n")
  cat("Call:\n")

  print(x$call)
  cat("\n")

  cat("Lambda:\n")

  print(x$lambda)
  cat("\n")

  printCoefmat(
    x$coefficients,
    P.value = TRUE,
    has.Pvalue = TRUE
  )

  cat("\n")
  cat("MSE:\n")
  print(x$MSE)
}
