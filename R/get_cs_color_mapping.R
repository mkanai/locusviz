#' Title
#'
#' @param cs_ids
#' @param highlight_cs_ids
#' @param colors
#'
#' @return
#' @export
#'
#' @examples
get_cs_color_mapping = function(cs_ids,
                                highlight_cs_ids = NULL,
                                colors = BuenColors::jdb_palette("corona")[setdiff(seq(15), c(8, 15))]) {
  if (!is.null(highlight_cs_ids)) {
    cs_ids = c(highlight_cs_ids, setdiff(cs_ids, highlight_cs_ids))
  }
  cs_ids = unique(cs_ids)
  if (length(colors) < length(cs_ids)) {
    warning("Supplied # colors is smaller than # CS ids.")
  }
  colors = rep(colors, length.out = length(cs_ids))
  names(colors) = as.character(cs_ids)
  return(colors)
}
