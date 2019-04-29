#' Calculate minimal detectable effect for binary variable
#'
#' Calculate the minimal detectable effect (Hazard Ratio) given an observed number of events for a binary variable (based on Schoenfeld's formula)
#' @param number_of_events Number of events
#' @param alpha 1-sided alpha (Type I error)
#' @param v Proportion of treatment allocation (default is 1:1)
#' @return Minimal detectable effect
#' @export

calculate_MDE <- function(number_of_events, alpha = 0.05, v = 0.5) {
  1 / exp( sqrt(((((qnorm(1- alpha) + qnorm(1-0.5)))^2) / number_of_events ) / (v *(1-v))))
  }
