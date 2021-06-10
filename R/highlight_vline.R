#' Title
#'
#' @param highlight_pos
#' @param size
#'
#' @return
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
