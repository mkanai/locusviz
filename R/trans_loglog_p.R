#' Title
#'
#' @param loglog_p 
#'
#' @return
#' @export
#'
#' @examples
trans_loglog_p <- function(loglog_p = 10) {
  scales::trans_new(
    "loglog_p",
    transform = function(x)
      ifelse(x < loglog_p, x, loglog_p * log10(x) / log10(loglog_p)),
    inverse = function(x)
      ifelse(x < loglog_p, x, 10 ** (log10(loglog_p) * x / loglog_p))
  )
}