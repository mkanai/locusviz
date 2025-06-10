#' Get color mapping for credible sets
#'
#' This function creates a color mapping for credible set IDs, with optional
#' prioritization of highlighted credible sets.
#'
#' @param cs_ids Character or numeric vector of credible set IDs
#' @param highlight_cs_ids Optional character or numeric vector of credible set IDs
#'   to prioritize in color assignment (will receive the first colors)
#' @param colors Character vector of colors to use for mapping. Default uses
#'   BuenColors corona palette excluding certain values
#'
#' @return A named character vector mapping credible set IDs to colors
#'
#' @importFrom BuenColors jdb_palette
#'
#' @examples
#' # Basic usage
#' cs_colors <- get_cs_color_mapping(c("CS1", "CS2", "CS3"))
#'
#' # With highlighted credible sets
#' cs_colors <- get_cs_color_mapping(
#'   c("CS1", "CS2", "CS3", "CS4"),
#'   highlight_cs_ids = c("CS2", "CS4")
#' )
#'
#' @export
get_cs_color_mapping <- function(cs_ids,
                                 highlight_cs_ids = NULL,
                                 colors = BuenColors::jdb_palette("corona")[setdiff(seq(15), c(8, 15))]) {
  if (!is.null(highlight_cs_ids)) {
    cs_ids <- c(highlight_cs_ids, setdiff(cs_ids, highlight_cs_ids))
  }
  cs_ids <- unique(cs_ids)
  if (length(colors) < length(cs_ids)) {
    warning("Supplied # colors is smaller than # CS ids.")
  }
  colors <- rep(colors, length.out = length(cs_ids))
  names(colors) <- as.character(cs_ids)
  return(colors)
}
