#' Annotate horizontal range on a plot
#'
#' This function adds a horizontal line annotation with optional tips and label
#' to indicate a range or distance on a plot.
#'
#' @param xmin Numeric value for the start position of the range
#' @param xmax Numeric value for the end position of the range
#' @param y Numeric value for the y-axis position of the annotation
#' @param scale Numeric scaling factor for partial ranges (default: 1).
#'   Values < 1 create a broken line to indicate continuation
#' @param label Character string to display at the center of the range (optional)
#' @param tip_length Numeric length of vertical tips at range endpoints (default: 0)
#' @param line.size Numeric width of the annotation lines (default: 0.2)
#' @param text.size Numeric size for the label text (default: 2)
#'
#' @return A list of ggplot2 layers (geom_segment and optionally geom_text)
#'
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' ggplot(data, aes(x, y)) +
#'   geom_point() +
#'   annotate_hrange(xmin = 100, xmax = 200, y = 5, label = "100 kb")
#' }
#'
#' @export
annotate_hrange <- function(xmin,
                            xmax,
                            y,
                            scale = 1,
                            label = NULL,
                            tip_length = 0,
                            line.size = 0.2,
                            text.size = 2) {
  length <- xmax - xmin
  list(
    geom_segment(
      x = xmin,
      xend = xmin + length * scale,
      y = y,
      yend = y,
      size = line.size
    ),
    or_missing(
      scale < 1,
      geom_segment(
        x = xmin + length * (1 - scale),
        xend = xmax,
        y = y,
        yend = y,
        size = line.size
      )
    ),
    or_missing(
      tip_length > 0,
      geom_segment(
        x = xmin,
        xend = xmin,
        y = y - tip_length / 2,
        yend = y + tip_length / 2,
        size = line.size
      )
    ),
    or_missing(
      tip_length > 0,
      geom_segment(
        x = xmax,
        xend = xmax,
        y = y - tip_length / 2,
        yend = y + tip_length / 2,
        size = line.size
      )
    ),
    or_missing(!is.null(label), geom_text(
      data = data.frame(
        x = xmin + length / 2,
        y = y,
        label = label
      ),
      aes(x, y, label = label),
      size = text.size
    ))
  )
}
