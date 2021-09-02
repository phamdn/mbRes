#' Measure Statistical Uncertainty
#'
#' \code{resampling} performs randomization test and bootstrapping to calculate
#' P-value and percentile bootstrap confidence interval of Cliff's delta.
#'
#' @param v1 a vector, biomarker values from the treatment group.
#' @param v0 a vector, biomarker values from the control group.
#' @param nrand an integer, the number of randomization samples. The default
#'   value is 1999.
#' @param nboot an integer, the number of bootstrap samples. The default value
#'   is 1999.
#' @param conf.level a numeric, the confidence level to calculate percentile
#'   bootstrap confidence interval. The default value is 0.95.
#' @param seed an integer, the seed for random number generation. Setting a seed
#'   ensures the reproducibility of the result. See \code{\link{set.seed}} for
#'   more details.
#'
#' @return \code{resampling} returns a one-row data frame with four numerics:
#'   \item{\code{delta}}{the Cliff's delta of the treatment group.}
#'   \item{\code{pval}}{the observed P-value p under the null hypothesis.}
#'   \item{\code{sval}}{the S-value s calculated from P-value p.}
#'   \item{\code{se}}{the standard error of Cliff's delta.}
#'   \item{\code{ci.lower}}{the lower bound of the confidence interval.}
#'   \item{\code{ci.upper}}{the upper bound of the confidence interval.}
#'   \item{\code{ciw}}{the width of the confidence interval.}
#'
#' @references Greenland, S. (2019). Valid P-Values Behave Exactly as They
#'   Should: Some Misleading Criticisms of P-Values and Their Resolution With
#'   S-Values. The American Statistician, 73(sup1), 106–114.
#'   \doi{10.1080/00031305.2018.1529625}. \cr \cr Phipson, B., & Smyth, G. K.
#'   (2010). Permutation P-values Should Never Be Zero: Calculating Exact
#'   P-values When Permutations Are Randomly Drawn. Statistical Applications in
#'   Genetics and Molecular Biology, 9(1). \doi{10.2202/1544-6115.1585}. \cr \cr
#'   Efron, B., & Tibshirani, R. (1993). An introduction to the bootstrap.
#'   Chapman & Hall. \cr \cr Ruscio, J., & Mullen, T. (2012). Confidence
#'   Intervals for the Probability of Superiority Effect Size Measure and the
#'   Area Under a Receiver Operating Characteristic Curve. Multivariate
#'   Behavioral Research, 47(2), 201–223. \doi{10.1080/00273171.2012.658329}.
#'
#' @seealso \code{\link[RProbSup]{A1}}.
#'
#' @examples
#' resampling(unlist(sokolova2021[7:12, 2]), unlist(sokolova2021[1:6, 2]))
#'
#' @export
resampling <- function (v1,
                        v0,
                        nrand = 1999,
                        nboot = 1999,
                        conf.level = 0.95,
                        seed = 1) {
  set.seed(seed)

  n1 <- length(v1)
  n0 <- length(v0)

  delta <- cliff(v1, v0)

  # randomization test
  drand <- rep(0, nrand)
  pool <- sort(c(v1, v0))
  for (i in 1:nrand) {
    newlist <- split(sample(pool), rep(c("v1", "v0"), c(n1, n0)))
    drand[i] <- cliff(newlist$v1, newlist$v0)
  }

  pval <- (sum(abs(drand) >= abs(delta)) + 1) / (nrand + 1)
  sval <- -log(pval, 2)

  # bootstrapping
  dboot <- rep(0, nboot)
  v1n <- sort(v1)
  v0n <- sort(v0)
  for (i in 1:nboot) {
    pos1 <- sample(1:n1, n1, replace = TRUE)
    pos0 <- sample(1:n0, n0, replace = TRUE)
    dboot[i] <- cliff(v1n[pos1], v0n[pos0])
  }

  se <- sd(dboot)

  alpha <- 1 - conf.level

  ci.lower <- quantile(dboot, alpha / 2) %>% unname
  ci.upper <- quantile(dboot, 1 - alpha / 2) %>% unname

  ciw <- ci.upper - ci.lower

  data.frame(delta, pval, sval, se, ci.lower, ci.upper, ciw)
}
