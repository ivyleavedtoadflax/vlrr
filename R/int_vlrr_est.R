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


vlrr_est <- function(x, y, lambda)

{

  theta <- rep(1,ncol(x))

  optim_out <- optim(
    par = theta,
    fn = function(t) J(x, y, t, lambda),
    gr = function(t) gR(x, y, t, lambda),
    method = "BFGS",
    control = list(
      trace = 0,
      REPORT = 1
    )
  )

  df = nrow(x) - ncol(x)

  list(
    coefficients = optim_out$par,
    error = optim_out$value,
    convergence = optim_out$convergence,
    message = optim_out$message
  )
}
