#' Format p-values
#'
#' Format CI with 95%-CI
#' @param HR Point estimate
#' @param HR_lower Lower CI
#' @param HR_upper Upper CI
#' @return Formatted confidence interval as a string
#' @export

format_pval <- function(HR, HR_lower, HR_upper){
  paste0(formatC(as.numeric(HR), digits = 2, format = "f"),
         " (",
         formatC(as.numeric(HR_lower), digits = 2, format = "f"),
         "-",
         formatC(as.numeric(HR_lower), digits = 2, format = "f"),
        ")")
}
