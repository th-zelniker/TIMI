#' Calculate number of events for NI-trials
#'
#' Based on Schoenfeld's formula
#' @param alpha 1-sided alpha (Type I error)
#' @param beta Type II error
#' @param HR_effect Assumed effect size
#' @param NI_margin Non-inferiority Margin
#' @param v Proportion of patients that are assigned to the intervention arm  (Default is  0.5)
#' @return Number of events required
#' @export

number_events_NI <-   function(alpha, beta, HR_effect, NI_margin, v = 0.5) {
  (((qnorm(1- alpha) + qnorm(1-beta)))^2) / ((log(HR_effect) - log(NI_margin))^2 * v *(1-v))
  }
