#' @title Title
#'
#' @description \code{this_function} What does it do?.
#'
#' @details Give the details of the \code{code} here.
#'
#' @param parameter1 Does something...
#' @param parameter2 Also does something.
#' @return What do you get back?
#'
#' @examples
#'
#' library(dplyr)
#'
#' @export

summary.vlrr <- function(object, ...) {

  #se <- sqrt(diag(object$vcov))
  #tval <- coef(object) / se

  TAB <- cbind(
    Estimate = coef(object)#,
    #StdErr = se,
    #t.value = tval,
    #p.value = 2*pt(-abs(tval), df=object$df)
  )
  res <- list(
    call = object$call,
    coefficients = TAB
  )

  class(res) <-"summary.vlrr"
  res
}
