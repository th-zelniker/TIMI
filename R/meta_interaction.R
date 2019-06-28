#' Calculate the interaction term incl. 95 Percent-CI when the RR or HR for two subgroups are given.
#'
#' @param Group_A_HR HR or RR of Group A
#' @param Group_A_CI_lower Lower CI of Group A
#' @param Group_A_CI_upper Upper CI of Group A
#' @param Group_B_HR HR or RR of Group A
#' @param Group_B_CI_lower Lower CI of Group B
#' @param Group_B_CI_upper Upper CI of Group B
#' @return Interaction term as HR/RR incl. 95 percent -CI
#' @export

meta_interaction_term <- function(Group_A_HR, Group_A_CI_lower, Group_A_CI_upper, Group_B_HR, Group_B_CI_lower, Group_B_CI_upper){


  interaction_HR <- 1 / exp(log(Group_A_HR) - log(Group_B_HR))

  interaction_HR_upper <- 1 / (exp ( (log(Group_A_HR) - log(Group_B_HR)) -
                                       (1.96*(sqrt(( (log(Group_A_CI_upper) - log(Group_A_CI_lower)) / (2*1.96))^2 +
                                                     ((log(Group_B_CI_upper) - log(Group_B_CI_lower)) / (2*1.96))^2)))))

  interaction_HR_lower <- 1 / (exp ( (log(Group_A_HR) - log(Group_B_HR)) +
                                       (1.96*(sqrt(( (log(Group_A_CI_upper) - log(Group_A_CI_lower)) / (2*1.96))^2 +
                                                     ((log(Group_B_CI_upper) - log(Group_B_CI_lower)) / (2*1.96))^2)))))

  paste0("HR ", interaction_HR, " (", interaction_HR_lower, "-", interaction_HR_upper, ")")

}
