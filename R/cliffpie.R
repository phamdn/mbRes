#' Compute Point and Interval Estimates of Cliff's delta
#'
#' \code{cliffpie} calculates Cliff's delta and its bias-corrected and
#' accelerated bootstrap confidence interval.
#'
#' @param v1 a vector, biomarker values from the treatment group.
#' @param v0 a vector, biomarker values from the control group.
#' @param nboot an integer, the number of bootstrap samples. The default value
#'   is 1999.
#' @param conf.level a numeric, the confidence level. The default value is 0.95.
#' @param seed an integer, the seed for random number generation. Setting a seed
#'   ensures the reproducibility of the result. See \code{\link{set.seed}} for
#'   more details.
#'
#' @return \code{cliffpie} returns a one row data frame with three numerics:
#'   \item{\code{delta}}{the Cliff's delta of the treatment group.}
#'   \item{\code{ci.lower}}{the lower bound of the confidence interval.}
#'   \item{\code{ci.upper}}{the upper bound of the confidence interval.}
#'
#' @references Efron, B., & Tibshirani, R. (1993). An introduction to the
#'   bootstrap. Chapman & Hall. \cr \cr Ruscio, J., & Mullen, T. (2012).
#'   Confidence Intervals for the Probability of Superiority Effect Size Measure
#'   and the Area Under a Receiver Operating Characteristic Curve. Multivariate
#'   Behavioral Research, 47(2), 201–223.
#'   \doi{10.1080/00273171.2012.658329}.
#'
#' @seealso \code{\link[RProbSup]{A1}} and \code{\link[orddom]{dmes.boot}}.
#'
#' @examples
#' cliffpie(sokolova2019[7:12, 3], sokolova2019[1:6, 3])
#'
#' @export
cliffpie <- function (v1,
                      v0,
                      nboot = 1999,
                      conf.level = 0.95,
                      seed = 1) {
  if (any(is.na(c(v1, v0))))
    stop("missing values detected in the vectors \n")

  delta <- cliff(v1, v0)

  dboot <- rep(0, nboot)
  n1 <- length(v1)
  n0 <- length(v0)
  set.seed(seed)
  for (i in 1:nboot) {
    pos1 <- sample(1:n1, n1, replace = TRUE)
    pos0 <- sample(1:n0, n0, replace = TRUE)
    dboot[i] <- cliff(v1[pos1], v0[pos0])
  }
  dboot <- sort(dboot)

  ci.lower <- ci.upper <- pi

  if (min(dboot) == max(dboot))
    ci.lower <- ci.upper <- dboot[1]

  alpha <- 1 - conf.level

  if ((delta < min(dboot)) | (delta > max(dboot))) {
    ci.lower <- dboot[round((alpha / 2) * nboot)]
    ci.upper <- dboot[round((1 - alpha / 2) * nboot)]
  }

  if ((ci.lower == pi) & (ci.upper == pi)) {
    z0 <- qnorm(mean(dboot < delta))

    jk <- rep(0, (n1 + n0))
    for (i in 1:n1)
      jk[i] <- cliff(v1[-i], v0)
    for (i in 1:n0)
      jk[n1 + i] <- cliff(v1, v0[-i])
    diff <- mean(jk) - jk
    ahat <- sum(diff ^ 3) / (6 * (sum(diff ^ 2)) ^ 1.5)

    alpha1 <-
      pnorm(z0 + (z0 + qnorm(alpha / 2)) / (1 - ahat * (z0 + qnorm(alpha /
                                                                     2))))
    alpha2 <-
      pnorm(z0 + (z0 - qnorm(alpha / 2)) / (1 - ahat * (z0 - qnorm(alpha /
                                                                     2))))

    if (is.na(alpha1))
      alpha1 <- alpha / 2
    if (is.na(alpha2))
      alpha2 <- 1 - alpha / 2

    if (round(alpha1 * nboot) < 1) {
      ci.lower <- dboot[1]
    } else {
      ci.lower <- dboot[round(alpha1 * nboot)]
    }
    ci.upper <- dboot[round(alpha2 * nboot)]
  }
  data.frame(delta, ci.lower, ci.upper)
}
