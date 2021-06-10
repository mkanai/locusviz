#' stat_summary_irq
#'
#' @param color
#'
#' @export
stat_summary_irq = function(color = "black", size = 0.1) {
  stat_summary(
    size = size,
    fun = median,
    fun.min = function(y) {
      quantile(y, 0.25)
    },
    fun.max = function(y) {
      quantile(y, 0.75)
    },
    color = color
  )
}
