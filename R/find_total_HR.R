#' Calculate the HR of the Total Cohort given HRs, Number of Pts Included, and Target Events
#'
#' Based on Alosh, Huque, Koch  (2015)
#' @param n_CVD Number of patients with cardiovascular disease
#' @param n_MRF Number of patients with multiple risk factors
#' @param target_events Number of targeted events
#' @param ratio_event_rate_CVDvsMRF Ratio btw. event rate of CVD versus event rate of MRF
#' @return Total hazard ratio
#' @export

find_total_HR <- function(n_CVD, n_MRF, HR_CVD, HR_MRF, target_events, ratio_event_rate_CVDvsMRF){

  event_CVD_total <- target_events * (n_CVD * ratio_event_rate_CVDvsMRF) / ((n_CVD * ratio_event_rate_CVDvsMRF) + n_MRF)
  event_CVD_tx <- event_CVD_total * HR_CVD / (HR_CVD + 1)

  event_MRF_total <- target_events - event_CVD_total
  event_MRF_tx <- event_MRF_total * HR_MRF / (HR_MRF + 1)

  total_events_tx <- event_MRF_tx + event_CVD_tx
  HR_total <- total_events_tx / (target_events - total_events_tx)

  HR_total

  # total_events_CVD <- target_events * (n_CVD * ratio_event_rate_CVDvsMRF) / ((n_CVD * ratio_event_rate_CVDvsMRF) + n_MRF)
  # CVD_events_tx <- total_events_CVD * HR_CVD / (HR_CVD + 1)
  #
  # total_events_MRF <- target_events - total_events_CVD
  # MRF_events_tx <- total_events_MRF * HR_MRF / (HR_MRF + 1)
  #
  # total_events_tx <- MRF_events_tx + CVD_events_tx
  # HR_total <- total_events_tx / (target_events - total_events_tx)
  #
  # HR_total
}



