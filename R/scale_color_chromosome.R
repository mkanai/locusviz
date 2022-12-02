#' Title
#'
#' @param odd_color
#' @param even_color
#'
#' @return
#' @export
#'
#' @examples
scale_color_chromosome = function(odd_color = "darkblue", even_color = "grey50") {
  chromosomes = locusviz:::CHROMOSOME_SIZES[["GRCh37"]]$chromosome
  chromosome_colors = rep(c("darkblue", "grey50"), length.out = length(chromosomes)) %>%
    magrittr::set_names(chromosomes)
  return(scale_color_manual(values = chromosome_colors))
}
