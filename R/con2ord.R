#' Ordinalize Biomarker Ordinal Estimator
#'
#' \code{con2ord} ordinalizes the Biomarker Ordinal Estimator (BOE) into very
#' small, small, medium, and large differences.
#'
#' @param x a vector, the BOE values.
#' @param thresholds a numeric vector of length 3, the cut points for
#'   ordinalization. The default values are 0.25, 0.5, and 0.75.
#'
#' @return \code{con2ord} returns a factor with 5 levels (i.e., no, very small,
#'   small, medium, and large) that are the perceived differences from BOE.
#'
#' @examples
#' con2ord(seq(0, 1, 0.1))
#'
#' @export
con2ord <- function(x,
                    thresholds = c(0.25, 0.5, 0.75)) {
  if (is.numeric(x)) {
    x <- case_when(
      x > thresholds[3] ~ "Large",
      x > thresholds[2] ~ "Medium",
      x > thresholds[1] ~ "Small",
      x > 0 ~ "Very small",
      TRUE ~ "No"
    )

  }
  x <- factor(x, c("No", "Very small", "Small", "Medium", "Large"))
  x
}
