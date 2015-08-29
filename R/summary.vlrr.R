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
#' @title Summarise a \code{vlrr} model
#'
#' @description Summarise a \code{vlrr} model.
#'
#' @details S3 method for prducing summaries of \code{vlrr} model objects.
#'
#' @param object A model object of class \code{vlrr}.
#' @param ... arguments to be passed to or from other methods.
#' @return A list giving: convergence and message (from \code{optim} call),
#'   model formula call, regularisation parameter lambda, a summary of model
#'   coefficients, and a mean squared error of the fitted values and the
#'   original observations.
#'
#' @examples
#'
#' library(vlrr)
#' data(mpg, package="ggplot2")
#' model <- vlrr(mpg ~ disp, data = mtcars)
#' summary(model)
#'
#' @export

summary.vlrr <- function(object, ...) {

  # It would be nice to be able to compare the success of models from the
  # summary output. Here I include a simple mean squared error.

  m <- length(object$fitted.values)
  error <- object$residuals
  MSE <- as.numeric(1/m * crossprod(error, error))

  se <- sqrt(diag(object$vcov))
  tval <- coef(object) / se

  # Note that here the p value is for testing the null hypothesis that the
  # coefficients are different from zero

  TAB <- cbind(
    Estimate = coef(object),
    StdErr = se,
    t.value = tval,
    p.value = 2 * pt( - abs(tval), df = object$df)
  )

  colnames(TAB) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

  res <- list(
    convergence = object$convergence,
    message = object$message,
    call = object$call,
    coefficients = TAB,
    MSE = MSE
  )

  class(res) <-"summary.vlrr"
  res
}
