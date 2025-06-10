#' Get chromosome sizes and cumulative positions
#'
#' This function retrieves chromosome sizes from UCSC genome database and calculates
#' cumulative positions for genome-wide plotting.
#'
#' @param reference_genome Character string specifying the reference genome:
#'   "GRCh37" or "GRCh38"
#' @param chromosomes Character vector of chromosome names to include
#'   (default: chr1-chr22, chrX, chrY, chrM)
#'
#' @return A data frame containing chromosome information with columns:
#'   chromosome, seqlengths, genome, start, end, mid
#'
#' @importFrom rtracklayer SeqinfoForUCSCGenome
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr filter mutate select
#' @importFrom stringr str_remove
#'
#' @examples
#' \dontrun{
#' # Get sizes for all default chromosomes in GRCh38
#' chr_sizes_38 <- get_chromosome_sizes("GRCh38")
#'
#' # Get sizes for specific chromosomes in GRCh37
#' chr_sizes_37 <- get_chromosome_sizes("GRCh37", c("chr1", "chr2", "chr3"))
#' }
#'
#' @export
get_chromosome_sizes <- function(reference_genome, chromosomes = paste0("chr", c(seq(22), "X", "Y", "M"))) {
  if (reference_genome == "GRCh38") {
    ucsc_reference_genome <- "hg38"
  } else if (reference_genome == "GRCh37") {
    ucsc_reference_genome <- "hg19"
  }
  chromosome_sizes <- rtracklayer::SeqinfoForUCSCGenome(ucsc_reference_genome) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "chromosome") %>%
    dplyr::filter(chromosome %in% chromosomes) %>%
    dplyr::mutate(
      start = cumsum(as.numeric(c(0, seqlengths)))[seq_along(chromosomes)],
      end = cumsum(as.numeric(seqlengths)),
      mid = (start + end) / 2
    ) %>%
    dplyr::select(-isCircular)

  if (reference_genome == "GRCh37") {
    chromosome_sizes <- dplyr::mutate(
      chromosome_sizes,
      chromosome = stringr::str_remove(chromosome, "^chr")
    )
  }

  return(chromosome_sizes)
}

CHROMOSOME_SIZES <- list(
  "GRCh37" = get_chromosome_sizes("GRCh37"),
  "GRCh38" = get_chromosome_sizes("GRCh38")
)
