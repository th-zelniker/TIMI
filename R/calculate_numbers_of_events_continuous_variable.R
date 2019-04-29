#' Calculate number of events
#'
#'#' Calculate the number of events for a continuous covariate (based on Schoenfeld's formula)
#' Reference: "Sample-Size Calculations for the Cox Proportional Hazards Regression Model with Nonbinary Covariates" by F.Y. Hsieh, PhD, and Philip W. Lavori, PhD (2000)
#'
#' @param alpha 1-sided Alpha (Type I error)
#' @param beta Type II error
#' @param HR Hazard ratio associated with a one-unit change in a continuous covariate (X1)
#' @param sigma standard deviation of the continuous covariate (X1)
#' @return Number of events required
#' @export

required_events_biomarker <-   function(alpha, beta, HR, sigma) {
  ((qnorm(1- alpha) + qnorm(1-beta))^2) *   ((  ((sigma)^2) * (log(HR)^2) )^-1)
}
