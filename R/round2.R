#' Rounding of Numbers
#'
#' R  “rounds to even” when rounding off a 5. This formula rounds numbers ending in .5  up ("rounding in commerce").
#' This formula is based on http://alandgraf.blogspot.com/2012/06/rounding-in-r.html
#' @param x a numeric vector
#' @param digits integer indicating the number of decimal places
#' @export

round2 = function(x, digits = 1) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5
  z = trunc(z)
  z = z/10^digits
  z * posneg
}
