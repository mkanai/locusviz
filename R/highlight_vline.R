#' Add vertical highlight lines to a plot
#'
#' This function adds dashed vertical lines at specified positions to highlight
#' variants or regions of interest across multiple plot panels.
#'
#' @param highlight_pos Numeric vector of x-axis positions to highlight.
#'   If NULL, no lines are added
#' @param size Numeric line width (default: 0.5)
#'
#' @return A geom_vline ggplot2 layer or NULL if highlight_pos is NULL
#'
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' # Add highlight lines to a plot
#' ggplot(df, aes(x = position, y = value)) +
#'   geom_point() +
#'   highlight_vline(c(100000, 200000))
#' }
#'
#' @export
highlight_vline <- function(highlight_pos, size = 0.5) {
  or_missing(
    !is.null(highlight_pos),
    geom_vline(
      xintercept = highlight_pos,
      linetype = "dashed",
      color = "grey80",
      size = size
    )
  )
}
