#' Calculate power for subgroup interaction
#'
#' Based on Alosh, Huque, Koch  (2015)
#' @param alpha_s 1-sided alpha (Type I error) for subgroup interaction
#' @param alpha Type I error of the study (one-sided alpha)
#' @param beta Type II error of the study
#' @param delta_rel Interaction effect relative to the overall effect [(log(HR_s - log(delta_c)) / log(HR_overall)], see separate function
#' @param K Proportion
#' @return Power of subgroup interaction
#' @export

power_subgroup <- function(alpha_s, delta_rel, alpha, beta, K){

  1 -  pnorm( qnorm(1-alpha_s) -  (qnorm(1-alpha) + qnorm(1-beta)) * sqrt(K * (1-K)) * delta_rel)

}
