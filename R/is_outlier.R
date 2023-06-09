#' @title Find outliers using interquartile range rule
#'
#' @description This function aims at finding outliers in a vector using the interquartile range rule
#'
#' @param x A vector in which outliers need to be found
#'
#' @return A boolean vector, \code{TRUE} for outliers, \code{FALSE} when not
#'
#' @export
#'
#' @examples # generate data from gaussian and exponential distribution
#' X <- sample(c(rnorm(100), c(rexp(50), -rexp(50))))
#'
#' # plot data
#' plot(X, col = ifelse(is_outlier(X), "red", "black"))
#'
is_outlier <- function(x) {
  # Q1 & Q3 calculation
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  # IQR calculation
  iqr <- q3 - q1
  # boolean is_outlier
  is_out <- x < q1 - 1.5 * iqr | x > q3 + 1.5 * iqr
  # return
  return(is_out)
}
