#' Bootstrap confidence interval
#'
#' Computes a bootstrap confidence interval for a summary statistic.
#'
#' @param x Numeric vector of data values
#' @param func Function to compute the statistic (e.g., mean, median)
#' @param conf Confidence level (default: 0.95)
#' @param R Number of bootstrap replicates (default: 1000)
#' @param colname Column name for the output statistic (default: "median")
#'
#' @return A tibble with three columns: the statistic value, lower CI bound, and upper CI bound
#'
#' @importFrom boot boot boot.ci
#' @importFrom tibble tibble
#' @importFrom magrittr set_colnames %>%
#'
#' @export
boot_ci <- function(x, func, conf = 0.95, R = 1000, colname = "boot") {
  b <- boot::boot(x, statistic = function(data, indices) {
    func(data[indices], na.rm = TRUE)
  }, R = R)
  ci <- boot::boot.ci(b, type = "perc", conf = conf)
  return(tibble::tibble(
    func(x, na.rm = TRUE),
    ci$percent[4],
    ci$percent[5]
  ) %>% magrittr::set_colnames(c(colname, paste0(colname, c("_lower", "_upper")))))
}

#' Median confidence interval via bootstrap
#'
#' Computes a bootstrap confidence interval for the median.
#'
#' @inheritParams boot_ci
#'
#' @return A tibble with three columns: median, median_lower, median_upper
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- rnorm(100)
#' median_ci(x)
#' }
median_ci <- function(x, conf = 0.95, R = 1000, colname = "median") {
  return(boot_ci(x, median, conf = conf, R = R, colname = colname))
}

#' Mean confidence interval via bootstrap
#'
#' Computes a bootstrap confidence interval for the mean.
#'
#' @inheritParams boot_ci
#'
#' @return A tibble with three columns: mean, mean_lower, mean_upper
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- rnorm(100)
#' mean_ci(x)
#' }
mean_ci <- function(x, conf = 0.95, R = 1000, colname = "mean") {
  return(boot_ci(x, mean, conf = conf, R = R, colname = colname))
}

#' Binomial confidence interval
#'
#' Computes a confidence interval for a binomial proportion.
#'
#' @param x Number of successes
#' @param n Number of trials
#' @param methods Method for CI computation (default: "wilson"). See \code{\link[binom]{binom.confint}} for available methods.
#' @param colname Column name for the output fraction (default: "frac")
#'
#' @return A tibble with three columns: the proportion, lower CI bound, and upper CI bound
#'
#' @importFrom binom binom.confint
#'
#' @export
#'
#' @examples
#' \dontrun{
#' binom_ci(x = 30, n = 100)
#' }
binom_ci <- function(x, n, methods = "wilson", colname = "frac") {
  cols <- c(colname, paste0(colname, c("_lower", "_upper")))
  na_mask <- is.na(x)
  frac <- rep(NA_real_, length(x))
  lower <- rep(NA_real_, length(x))
  upper <- rep(NA_real_, length(x))
  if (any(!na_mask)) {
    ci <- binom::binom.confint(x[!na_mask], n[!na_mask], methods = methods)
    frac[!na_mask] <- x[!na_mask] / n[!na_mask]
    lower[!na_mask] <- ci$lower
    upper[!na_mask] <- ci$upper
  }
  tibble::tibble(frac, lower, upper) %>%
    magrittr::set_colnames(cols)
}

#' Spearman correlation confidence interval
#'
#' Computes Spearman's rank correlation with a confidence interval using
#' Fisher's z-transform on the ranks (i.e., Pearson correlation of ranks).
#'
#' @param x Numeric vector
#' @param y Numeric vector of the same length as \code{x}
#' @param conf Confidence level (default: 0.95)
#' @param colname Column name prefix for the output (default: "rho")
#'
#' @return A tibble with three columns: rho, rho_lower, rho_upper
#'
#' @importFrom stats cor.test
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- rnorm(100)
#' y <- x + rnorm(100, sd = 0.5)
#' spearman_ci(x, y)
#' }
spearman_ci <- function(x, y, conf = 0.95, colname = "rho") {
  complete <- stats::complete.cases(x, y)
  ct <- stats::cor.test(rank(x[complete]), rank(y[complete]),
    method = "pearson", conf.level = conf
  )
  tibble::tibble(
    ct$estimate,
    ct$conf.int[1],
    ct$conf.int[2]
  ) %>%
    magrittr::set_colnames(c(colname, paste0(colname, c("_lower", "_upper"))))
}
