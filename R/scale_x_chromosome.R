#' Title
#'
#' @param reference_genome
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
scale_x_chromosome = function(reference_genome, ...) {
  chromosome_sizes = CHROMOSOME_SIZES[[reference_genome]]

  scale_x_continuous(
    breaks = chromosome_sizes$mid,
    minor_breaks = sort(c(
      chromosome_sizes$start, chromosome_sizes$end
    )),
    labels = stringr::str_remove(chromosome_sizes$chromosome, "^chr"),
    expand = expansion(mult = 0.01),
    ...
  )
}

