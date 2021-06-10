#' Title
#'
#' @param xmin
#' @param xmax
#' @param y
#' @param scale
#' @param label
#' @param tip_length
#' @param line.size
#' @param text.size
#'
#' @return
#' @export
annotate_hrange = function(xmin,
                           xmax,
                           y,
                           scale = 1,
                           label = NULL,
                           tip_length = 0,
                           line.size = 0.2,
                           text.size = 2) {
  length = xmax - xmin
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
