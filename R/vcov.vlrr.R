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
#' @title Return Variance-Covariance matrix of a vlrr object
#'
#' @description Returns the variance-covariance matrix of the main parameters of a \code{vlrr} object.
#'
#' @details S3 method for returning the variance-covriance matrix of a \code{vlrr} model object.
#'
#' @param object A model object of class \code{vlrr}.
#' @param ... arguments to be passed to or from other methods.
#'
#' @examples
#'
#' library(vlrr)
#' data(mpg, package="ggplot2")
#' model <- vlrr(mpg ~ disp, data = mtcars)
#' vcov(model)
#'
#' @export

vcov.vlrr <- function(object, ...) {

  object$vcov

}
