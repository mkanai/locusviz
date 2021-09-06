#' Title
#'
#' @param variant
#' @param genome_build
#'
#' @return
#' @export
liftover_variant = function(variant, genome_build = c("hg19", "hg38")) {
  genome_build = match.arg(genome_build)

  uniq_variant = unique(variant)

  gr = parse_variant(uniq_variant) %>%
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
    as.list() %>%
    purrr::map_dfr(function(x) {
      if (length(x) == 0) {
        return(tibble::tibble(
          seqnames = NA,
          end = NA,
          ref = NA,
          alt = NA
        ))
      }
      return(tibble::as_tibble(x))
    })

  if (genome_build == "hg38") {
    ret = dplyr::mutate(ret, seqnames = stringr::str_remove(seqnames, "^chr"))
  }

  ret = dplyr::mutate(
    ret,
    variant = uniq_variant,
    new_variant = stringr::str_c(seqnames, end, ref, alt, sep = ":"),
    new_chromosome = seqnames,
    new_position = end,
    new_ref = ref,
    new_alt = alt
  ) %>%
    dplyr::select(variant, dplyr::starts_with("new_"))
  ret = dplyr::left_join(data.frame(variant = variant), ret)

  return(ret)
}
