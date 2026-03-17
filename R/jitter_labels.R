#' Jitter labels to avoid overlapping
#'
#' This function adjusts label positions to prevent overlapping when plotting
#' multiple labels at similar x-coordinates. It uses the trackViewer package's
#' label adjustment algorithms.
#'
#' @param label.pos Numeric vector or data frame of label positions
#' @param xscale Numeric vector of length 2 specifying the x-axis scale limits
#'
#' @return Adjusted label positions with jittering applied to avoid overlaps
#'
#' @importFrom grid pushViewport viewport convertX unit popViewport
#'
#' @examples
#' \dontrun{
#' # Adjust label positions for a plot
#' positions <- c(100, 105, 110, 115)
#' adjusted <- jitter_labels(positions, xscale = c(0, 1000))
#' }
#'
#' @export
jitter_labels <- function(label.pos, xscale) {
  if (!requireNamespace("trackViewer", quietly = TRUE)) {
    stop("Package 'trackViewer' is required for jitter_labels(). ",
      "Install it with: BiocManager::install('trackViewer')",
      call. = FALSE
    )
  }

  grid::pushViewport(grid::viewport(xscale = xscale))
  lineW <- as.numeric(grid::convertX(unit(1, "line"), "npc"))
  label.pos <- trackViewer:::jitterLables(label.pos, xscale, lineW)
  label.pos <- trackViewer:::reAdjustLabels(label.pos, lineW)
  grid::popViewport()
  return(label.pos)
}
