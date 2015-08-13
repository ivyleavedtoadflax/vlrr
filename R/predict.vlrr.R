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
#' @title Predict method for the \code{vlrr} method.
#'
#' @description \code{predict.vlrr} Predict method for \code{vlrr} function.
#'
#' @details Predict method for \code{vlrr} function.
#'
#' @param object A \code{vlrr} model object.
#' @param newdata If A list containing new training examples. See examples.
#' @return Prediction based on the model object and newdata.
#'
#' @examples
#'
#' library(vlrr)
#' library(ggplot2) # to load mpg data
#'
#' model <- vlrr(hwy ~ displ, data = mpg)
#'
#' predict(model, newdata = list(hwy = mtcars$disp, displ = mtcars$disp * 0.0163))
#'
#' @export

predict.vlrr <- function(object, newdata = NULL, ...) {

  # If no new data, the just present the fitted values from the model

  if(is.null(newdata)) {
    y <- fitted(object)
  }
  else{
    if(!is.null(object$formula)){
      ## model has been fitted using formula interface
      x <- model.matrix(object$formula, newdata)
    }
    else{
      # newdata is just a matrix
      x <- newdata
    }
    y <- as.vector(x %*% coef(object))
  }
  y
}
