#' Format p-values
#'
#' Format p-values: p-values <= 0.10 are printed with 3 digits, otherwise 2 digits
#' @param x Numeric variable
#' @return P-value as string
#' @export

format_pval <- function(x){
  ifelse(x < 0.001,
         "<0.001",
         ifelse(x == 0.001,
                "0.001",
                ifelse(x >0.001 & x <= 0.1,
                       formatC(x, digits = 3, format = "f"),
                       formatC(x, digits = 2, format = "f"))))
}
