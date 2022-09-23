#' Title
#'
#' @param loglog_p
#'
#' @return
#' @export
#'
#' @examples
trans_loglog_p <- function(loglog_p = 10) {
  .transform = function(x) {
    ifelse(x < loglog_p, x, loglog_p * log10(x) / log10(loglog_p))
  }
  .inverse = function(x) {
    ifelse(x < loglog_p, x, 10 ** (log10(loglog_p) * x / loglog_p))
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
