#' Title
#'
#' @param chromosome
#' @param position
#' @param reference_genome
#'
#' @return
#' @export
get_global_position = function(chromosome, position, reference_genome) {
  tibble::tibble(chromosome = chromosome, position = position) %>%
    dplyr::left_join(CHROMOSOME_SIZES[[reference_genome]], by = "chromosome") %>%
    dplyr::mutate(new_position = start + position) %>%
    dplyr::pull(new_position)
}
