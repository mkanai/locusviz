#' Title
#'
#' @param odd_color
#' @param even_color
#'
#' @return
#' @export
#'
#' @examples
scale_color_chromosome = function(odd_color = "darkblue", even_color = "grey50", reference_genome = "GRCh37") {
  chromosomes = locusviz:::CHROMOSOME_SIZES[[reference_genome]]$chromosome
  chromosome_colors = rep(c(odd_color, even_color), length.out = length(chromosomes)) %>%
    magrittr::set_names(chromosomes)
  return(scale_color_manual(values = chromosome_colors))
}
