#' Calculate number of events
#'
#' Based on Schoenfeld's formula
#' @param alpha 1-sided alpha (Type I error)
#' @param beta Type II error
#' @param HR Assumed effect size
#' @param v Proportion of patients that are assigned to the intervention arm  (Default is  0.5)
#' @return Number of events required
#' @export

number_events_Cox <-   function(alpha, beta, HR, v = 0.5) {
  (((qnorm(1- alpha) + qnorm(1-beta)))^2) / ((log(HR))^2 * v *(1-v))
  }
