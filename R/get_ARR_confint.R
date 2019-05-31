#' Calculate Absolute Risk Difference, the SE, and confidence interval
#'
#' @param trt_n Number of events with Treatment
#' @param trt_N Total number of patients in the treatment group
#' @param placebo_n Number of events with Placebo
#' @param placebo_N Total number of patients in the placebo group
#' @return P-value
#' @export

get_ARR_confint <- function(trt_n, trt_N, placebo_n, placebo_N) {

  n_NA = placebo_n / placebo_N *100
  tot_NA = placebo_N

  n_NB = trt_n / trt_N *100
  tot_NB = trt_N

  ARR = n_NA - n_NB
  SE_ARR = sqrt( (n_NA * (100 - n_NA) / tot_NA) + (n_NB * (100 - n_NB) / tot_NB))

  ARR_lower = ARR - 1.96*SE_ARR

  ARR_upper = ARR + 1.96*SE_ARR

  df <- data.frame(ARR = ARR,
                   SE_ARR = SE_ARR,
                   ARR_lower = ARR_lower,
                   ARR_lower = ARR_upper)
  df
}
