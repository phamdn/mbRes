#' Compute Cliff's delta
#'
#' \code{cliff} calculates Cliff's delta statistic using the rank sum method.
#'
#' @param v1 a vector, biomarker values from the treatment group.
#' @param v0 a vector, biomarker values from the control group.
#'
#' @return \code{cliff} returns a numeric that is the Cliff's delta of the
#'   treatment group.
#'
#' @references Cliff, N. (1993). Dominance statistics: Ordinal analyses to
#'   answer ordinal questions. Psychological Bulletin, 114(3), 494–509.
#'   \doi{10.1037/0033-2909.114.3.494}. \cr \cr Vargha, A., &
#'   Delaney, H. D. (2000). A Critique and Improvement of the CL Common Language
#'   Effect Size Statistics of McGraw and Wong. Journal of Educational and
#'   Behavioral Statistics, 25(2), 101–132.
#'   \doi{10.3102/10769986025002101}. \cr \cr Ruscio, J., &
#'   Mullen, T. (2012). Confidence Intervals for the Probability of Superiority
#'   Effect Size Measure and the Area Under a Receiver Operating Characteristic
#'   Curve. Multivariate Behavioral Research, 47(2), 201–223.
#'   \doi{10.1080/00273171.2012.658329}.
#'
#' @seealso \code{\link[RProbSup]{CalcA1}} and \code{\link[orddom]{dmes}}.
#'
#' @examples
#' cliff(sokolova2019[7:12, 3], sokolova2019[1:6, 3])
#'
#' @export
cliff <- function(v1, v0) {
  if (any(is.na(c(v1, v0))))
    stop("missing values detected in the vectors \n")

  rnk <- rank(c(v1, v0), ties.method = "average")
  r1 <- sum(rnk[seq_along(v1)])
  n1 <- length(v1)
  n0 <- length(v0)

  d <- (2 * r1 - n1 * (n1 + 1) - n1 * n0) / (n1 * n0)
  d
}
