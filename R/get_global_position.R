#' Convert chromosomal position to global genomic position
#'
#' This function converts chromosome-specific positions to global genomic positions
#' for genome-wide plotting by adding the cumulative chromosome offset.
#'
#' @param chromosome Character vector of chromosome identifiers
#' @param position Numeric vector of positions within chromosomes
#' @param reference_genome Character string specifying the reference genome:
#'   "GRCh37" or "GRCh38"
#'
#' @return Numeric vector of global genomic positions
#'
#' @importFrom tibble tibble
#' @importFrom dplyr left_join mutate pull
#'
#' @examples
#' # Convert single position
#' global_pos <- get_global_position("chr2", 1000000, "GRCh38")
#'
#' # Convert multiple positions
#' global_pos <- get_global_position(
#'   c("chr1", "chr2", "chr3"),
#'   c(1000000, 2000000, 3000000),
#'   "GRCh37"
#' )
#'
#' @export
get_global_position <- function(chromosome, position, reference_genome) {
  tibble::tibble(chromosome = chromosome, position = position) %>%
    dplyr::left_join(CHROMOSOME_SIZES[[reference_genome]], by = "chromosome") %>%
    dplyr::mutate(new_position = start + position) %>%
    dplyr::pull(new_position)
}
