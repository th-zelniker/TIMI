#' Calculate number of events using the winratio. Sample size calculatin based on Yu and Ganju, Statistics in Medicine 2021
#'
#' Based on Schoenfeld's formula
#' @param alpha 1-sided alpha (Type I error)
#' @param beta Type II error
#' @param WR_true Assumed effect size
#' @param P_tie Proportion of ties
#' @param v Proportion of patients that are assigned to the intervention arm  (Default is  0.5)
#' @return Total sample size required
#' @export

sample_size_winratio <-   function(alpha, beta, WR_true, P_tie, v = 0.5) {

  (((4*(1 + P_tie)) / (3 * v * (1-v) * (1-P_tie))) * (qnorm(1- alpha) + qnorm(1-beta))^2) / ((log(WR_true))^2)
  }


