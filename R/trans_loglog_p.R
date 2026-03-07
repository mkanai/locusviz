#' Create log-log transformation for p-values
#'
#' This function creates a custom transformation for p-values that applies
#' logarithmic scaling above a threshold to better visualize extreme p-values
#' in Manhattan plots.
#'
#' @param loglog_p Numeric threshold above which to apply log-log transformation
#'   (default: 10). Values below this threshold are unchanged, values above are
#'   log-transformed
#'
#' @return A scales transformation object for use with ggplot2 scale functions
#'
#' @importFrom scales trans_new extended_breaks
#'
#' @examples
#' \dontrun{
#' # Use in a Manhattan plot
#' ggplot(gwas_data, aes(x = position, y = -log10(p))) +
#'   geom_point() +
#'   scale_y_continuous(trans = trans_loglog_p(10))
#'
#' # With higher threshold
#' ggplot(gwas_data, aes(x = position, y = -log10(p))) +
#'   geom_point() +
#'   scale_y_continuous(trans = trans_loglog_p(20))
#' }
#'
#' @export
trans_loglog_p <- function(loglog_p = 10) {
  .transform <- function(x) {
    ifelse(x < loglog_p, x, loglog_p * log10(x) / log10(loglog_p))
  }
  .inverse <- function(x) {
    ifelse(x < loglog_p, x, 10**(log10(loglog_p) * x / loglog_p))
  }
  scales::trans_new(
    "loglog_p",
    transform = .transform,
    inverse = .inverse,
    breaks = function(x, n = 5) {
      round(.inverse(scales::extended_breaks(n = n)(x = .transform(x))))
    }
  )
}
