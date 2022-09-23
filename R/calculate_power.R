#' Calculate Power
#'
#' Based on Schoenfeld's formula
#' @param number_of_events Number of Events
#' @param alpha 1-sided Alpha (Type I error)
#' @param HR Assumed Effect Size
#' @param v Proportion
#' @return Power
#' @export

calculate_power <- function(number_of_events, HR, alpha, v){

  pnorm(sqrt(number_of_events) * sqrt(v * (1-v) * ((log(HR))^2) ) - qnorm(1- alpha))
}
