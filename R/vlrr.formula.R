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
#' @title Use a formula to define \code{vlrr} model
#'
#' @description S3 formula method for \code{vlrr} class.
#'
#' @details Give the details of the \code{code} here.
#'
#' @param formula Formula specifying model.
#' @param data List specifying location or vectors called in formula.
#' @param ... Further arguments to or from other methods.
#' @return Object of class formula
#'
#' @examples
#'
#' library(vlrr)
#' model <- vlrr(mpg ~ disp, data = mtcars)
#' model
#'
#' @export

vlrr.formula <- function(formula, data = list(), ...) {

  mf <- model.frame(formula = formula, data = data)
  x <- model.matrix(attr(mf, "terms"), data = mf)
  y <- model.response(mf)

  est <- vlrr.default(x, y, ...)
  est$call <- match.call()
  est$formula <- formula
  est

}
