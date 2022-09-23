#' Calculate minimal detectable effect for a continuous variable
#'
#' Calculate the minimal detectable effect (Hazard Ratio) given an observed number of events for a continuous variable (based on Schoenfeld's formula)
#' Reference: "Sample-Size Calculations for the Cox Proportional Hazards Regression Model with Nonbinary Covariates" by F.Y. Hsieh, PhD, and Philip W. Lavori, PhD (2000)
#'
#' @param number_of_events Number of events
#' @param alpha 1-sided alpha (Type I error)
#' @param sd Standarad deviation of the continuous covariate (default is 1 assuming the continuous marker is standardized)
#' @return Minimal detectable effect
#' @export

calculate_MDE_biomarker <- function(number_of_events, alpha = 0.05, sd = 1) {
  1 / exp( sqrt(((((qnorm(1- alpha) + qnorm(1-0.5)))^2) / number_of_events ) / (sd^2)))
  }
