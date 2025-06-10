#' Create chromosome x-axis scale for genome-wide plots
#'
#' This function creates a ggplot2 x-axis scale for genome-wide plots with
#' chromosome labels at the center of each chromosome and minor breaks at
#' chromosome boundaries.
#'
#' @param reference_genome Character string specifying the reference genome:
#'   "GRCh37" or "GRCh38"
#' @param ... Additional arguments passed to scale_x_continuous
#'
#' @return A ggplot2 scale_x_continuous object with chromosome-specific breaks
#'   and labels
#'
#' @import ggplot2
#' @importFrom stringr str_remove
#'
#' @examples
#' # Create a Manhattan plot with chromosome scale
#' ggplot(gwas_data, aes(x = global_position, y = -log10(p))) +
#'   geom_point() +
#'   scale_x_chromosome("GRCh38")
#'
#' # With custom expansion
#' ggplot(gwas_data, aes(x = global_position, y = -log10(p))) +
#'   geom_point() +
#'   scale_x_chromosome("GRCh37", expand = expansion(mult = 0.02))
#'
#' @export
scale_x_chromosome <- function(reference_genome, ...) {
  chromosome_sizes <- CHROMOSOME_SIZES[[reference_genome]]

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
