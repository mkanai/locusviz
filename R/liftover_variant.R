#' Title
#'
#' @param variant
#' @param genome_build
#'
#' @return
#' @export
liftover_variant = function(variant, genome_build = c("hg19", "hg38")) {
  genome_build = match.arg(genome_build)

  gr = parse_variant(variant) %>%
    dplyr::mutate(start = position - 1,
                  end = position) %>%
    GenomicRanges::makeGRangesFromDataFrame(seqnames.field = "chromosome", keep.extra.columns = TRUE)

  chain = switch(
    genome_build,
    hg19 = rtracklayer::import.chain(
      system.file(package = "locusviz", "extdata", "hg19ToHg38.over.chain")
    ),
    hg38 = rtracklayer::import.chain(
      system.file(package = "locusviz", "extdata", "hg38ToHg19.over.chain")
    )
  )
  ensembldb::seqlevelsStyle(gr) = "UCSC"

  ret =
    rtracklayer::liftOver(gr, chain) %>%
    tibble::as_tibble()

  if (genome_build == "hg38") {
    ret = dplyr::mutate(ret, seqnames = stringr::str_remove(seqnames, "^chr"))
  }

  dplyr::mutate(
    ret,
    variant = variant,
    new_variant = stringr::str_c(seqnames, end, ref, alt, sep = ":"),
    new_chromosome = seqnames,
    new_position = end,
    new_ref = ref,
    new_alt = alt
  ) %>%
    dplyr::select(variant, dplyr::starts_with("new_"))
}
