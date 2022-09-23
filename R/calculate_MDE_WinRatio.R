#' Calculate minimal detectable effect for binary variable using the WinRatio
#'
#' Calculate the minimal detectable effect (Win Ratio) given the total sample size (Yu and Ganju, Statistics in Medicine 2021)
#' @param sample_size Total Sample Size
#' @param P_tie Proportion of ties
#' @param alpha 1-sided alpha (Type I error)
#' @param v Proportion of treatment allocation (default is 1:1)
#' @return Minimal detectable effect
#' @export

calculate_MDE_WinRatio <- function(sample_size, P_tie, alpha = 0.05, v = 0.5) {

  exp(sqrt((((4*(1 + P_tie)) / (3 * v * (1-v) * (1-P_tie))) * (qnorm(1- alpha) + qnorm(1-0.5))^2) / sample_size))

  }
