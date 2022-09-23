#' Calculate sample size or power for comparing 2 negative binomial rates
#' (either n or power needs to included)
#'
#' @param n Sample Size; Leave NULL to calculate power
#' @param r0 Event Rate (Placebo)
#' @param r1 Event Rate (Treatment); Alernatively, enter RR
#' @param RR Rate Ratio
#' @param k Negative binomial dispersion parameter (see dispersion_nb Function)
#' @param duration Average treatment duration
#' @param alpha 1-sided alpha (Type I error)
#' @param beta Type II error; Leave NULL to calculate sample size
#' @param ssize.ratio Sample allocation ratio n1/n0 (Default is  1)
#' @param approach 1 (Use reference group rate), 2 (Use true rates), or 3 (Use ML estimation); see Zhu and Lakkis (2014).
#' @return Either power of Number of events reported
#' @export

power_nb <-   function (n = NULL,
                        r0, r1, RR,
                        k,
                        duration = 1,
                        ssize.ratio = 1,
                        alpha = 0.025,
                        power = NULL,
                        approach = 3) {

  if (sum(sapply(list(n, power), is.null)) != 1)
    stop("exactly one of 'n' and 'power' must be NULL")
  if (!is.numeric(alpha) || any(0 > alpha | alpha >
                                    1))
    stop("'alpha' must be numeric in (0, 1)")
  if (!is.null(power) && (!is.numeric(alpha) || any(0 >
                                                        alpha | alpha > 1)))
    stop("'power' must be numeric in (0, 1)")
  if (missing(r0))
    stop("'r0' must be given")
  if (missing(r1) && missing(RR))
    stop("either 'r1' or 'RR' must be given")
  if (missing(k))
    stop("'k (negative binomial dispersion parameter)' must be given")
  if (missing(r1))
    r1 <- r0 * RR
  if (missing(RR))
    RR <- r1/r0
  if (!is.numeric(r0) || r0 <= 0)
    stop("'r0' must be numeric and > 0")
  if (!is.numeric(r1) || r1 <= 0)
    stop("'r1' must be numeric and > 0")
  if (!is.numeric(duration) || duration <= 0)
    stop("'duration' must be numeric and > 0")

  if (!is.numeric(ssize.ratio) || ssize.ratio <= 0)
    stop("'ssize.ratio' must be numeric and > 0")


  if (!(approach %in% 1:3))
    stop("'approach' must be equal to 1, 2, or 3")

  if (approach == 1)
    V0 <- (1 + ssize.ratio)/(duration * ssize.ratio * r0) +
    (1 + ssize.ratio) * k/ssize.ratio

  if (approach == 2)
    V0 <- 1/duration * (1/r0 + 1/(ssize.ratio * r1)) +
    (1 + ssize.ratio) * k/ssize.ratio

  if (approach == 3)
    V0 <- (1 + ssize.ratio)^2/(duration * ssize.ratio * (r0 +
                                                           ssize.ratio * r1)) + (1 + ssize.ratio) * k/ssize.ratio

  V1 <- 1/duration * (1/r0 + 1/(ssize.ratio * r1)) + (1 +
                                                        ssize.ratio) * k/ssize.ratio
  if (is.null(power)) {
    power <- pnorm((sqrt(n) * abs(log(r1/r0)) - qnorm(1 -
                                                        alpha) * sqrt(V0))/sqrt(V1))
  }

  else if (is.null(n)) {
    beta <- 1 - power

    n <- ((qnorm(1 - alpha) * sqrt(V0) + qnorm(1 -
                                                 beta) * sqrt(V1))/log(r1/r0))^2
  }

  else stop("internal error", domain = NA)

  n1 <- ssize.ratio * n

  data.frame(n0 = n,
             n1 = n1,
             ssize = n + n1,
             r0 = r0,
             RR = RR,
             k = k,
             duration = duration,
             alpha = alpha,
             power = power,
             approach = approach)
}
