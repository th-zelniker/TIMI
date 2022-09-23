#' Estimate the negative binomial dispersion parameter k (according to Zhu H and Lakkis H, Stat Med 2014)
#'
#' @param r Event rate per patient year
#' @param CI_lower Lower confidence interval of r
#' @param CI_lower Upper confidence interval of r
#' @param exposure_time Total exposure time in years
#' @return Negative binomial dispersion parameter k
#' @export

dispersion_nb <-    function(r, CI_lower, CI_upper, exposure_time){

  # SE_log_r <- ( log(CI_upper) - log(r) ) / qnorm(0.975)
  SE_log_r <- ( log(CI_upper) - log(CI_lower) ) / (2*qnorm(0.975))

  overdispersion_parameter <- exposure_time * r * SE_log_r^2
  k = (overdispersion_parameter - 1) / r #theta
  k
}
