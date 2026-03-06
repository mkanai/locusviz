#' Create chromosome color scale for Manhattan plots
#'
#' This function creates a ggplot2 color scale that alternates colors between
#' odd and even chromosomes for better visualization in Manhattan plots.
#'
#' @param odd_color Character string specifying the color for odd-numbered
#'   chromosomes (default: "darkblue")
#' @param even_color Character string specifying the color for even-numbered
#'   chromosomes (default: "grey50")
#' @param reference_genome Character string specifying the reference genome:
#'   "GRCh37" or "GRCh38" (default: "GRCh37")
#'
#' @return A ggplot2 scale_color_manual object
#'
#' @import ggplot2
#' @importFrom magrittr set_names
#'
#' @examples
#' # Default alternating colors
#' ggplot(data, aes(x = position, y = -log10(p), color = chromosome)) +
#'   geom_point() +
#'   scale_color_chromosome()
#'
#' # Custom colors
#' ggplot(data, aes(x = position, y = -log10(p), color = chromosome)) +
#'   geom_point() +
#'   scale_color_chromosome(odd_color = "red", even_color = "blue")
#'
#' @export
scale_color_chromosome <- function(odd_color = "darkblue", even_color = "grey50", reference_genome = "GRCh37") {
  chromosomes <- CHROMOSOME_SIZES[[reference_genome]]$chromosome
  chromosome_colors <- rep(c(odd_color, even_color), length.out = length(chromosomes)) %>%
    magrittr::set_names(chromosomes)
  return(scale_color_manual(values = chromosome_colors))
}
