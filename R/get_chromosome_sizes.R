get_chromosome_sizes = function(reference_genome, chromosomes = paste0("chr", c(seq(22), "X", "Y", "M"))) {
  if (reference_genome == "GRCh38") {
    ucsc_reference_genome = "hg38"
  } else if (reference_genome == "GRCh37") {
    ucsc_reference_genome = "hg19"
  }
  chromosome_sizes = rtracklayer::SeqinfoForUCSCGenome(ucsc_reference_genome) %>%
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
    chromosome_sizes = dplyr::mutate(
      chromosome_sizes,
      chromosome = stringr::str_remove(chromosome, "^chr")
    )
  }

  return(chromosome_sizes)
}

CHROMOSOME_SIZES = list(
  "GRCh37" = get_chromosome_sizes("GRCh37"),
  "GRCh38" = get_chromosome_sizes("GRCh38")
)
