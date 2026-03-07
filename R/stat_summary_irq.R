#' Summary statistic with interquartile range
#'
#' This function creates a ggplot2 stat_summary layer that displays the median
#' with error bars showing the interquartile range (25th to 75th percentile).
#'
#' @param color Character string specifying the color for the summary statistics
#'   (default: "black")
#' @param size Numeric size for the lines (default: 0.1)
#'
#' @return A ggplot2 stat_summary layer
#'
#' @import ggplot2
#' @importFrom stats median quantile
#'
#' @examples
#' \dontrun{
#' # Add IQR summary to a plot
#' ggplot(df, aes(x = group, y = value)) +
#'   geom_point() +
#'   stat_summary_irq(color = "red")
#' }
#'
#' @export
stat_summary_irq <- function(color = "black", size = 0.1) {
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
