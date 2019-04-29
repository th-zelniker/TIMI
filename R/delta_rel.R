#' Interaction effect relative to the effect size for the total population
#'
#' Based on Alosh, Huque, Koch  (2015)
#' @param HR_s HR in the Subgroup of Interest
#' @param HR_c HR in the Comparative Subgroup
#' @param HR_overall HR of the Overall Cohort
#' @return Interaction Effect Relative to the Effect Size for the Total Population
#' @export

delta_rel <- function(HR_s, HR_c, HR_overall){

  (log(HR_s) - log(HR_c)) / log(HR_overall)

}
