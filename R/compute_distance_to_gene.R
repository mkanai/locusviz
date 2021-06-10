#' Title
#'
#' @param txdb
#' @param chromosome
#' @param start
#' @param end
#' @param ref_position
#' @param type
#'
#' @return
#' @export
compute_distance_to_gene = function(txdb,
                                    chromosome,
                                    start,
                                    end,
                                    ref_position,
                                    type = c("GB", "TSS")) {
  type = match.arg(type)

  if (!stringr::str_starts(chromosome, "chr")) {
    chromosome = stringr::str_c("chr", chromosome)
  }

  if (type == "GB") {
    gr = GenomicRanges::GRanges(seqnames = chromosome, ranges = IRanges(start, end))
    gr.txdb <- biovizBase::crunch(txdb, which = gr)

    df <- GenomicRanges::as.data.frame(gr.txdb) %>%
      dplyr::filter(type == "exon") %>%
      dplyr::group_by(tx_name) %>%
      dplyr::summarize(start = min(start), end = max(end)) %>%
      dplyr::mutate(
        method = "Distance",
        score = ifelse(
          start <= ref_position &
            ref_position <= end,
          0,
          pmin(abs(start - ref_position), abs(end - ref_position))
        ),
        gene = forcats::fct_reorder(tx_name, start)
      ) %>%
      dplyr::select(gene, method, score)
  } else if (type == "TSS") {
    df =
      GenomicRanges::promoters(txdb, upstream = 0, downstream = 0) %>%
      plyranges::filter(seqnames == chromosome) %>%
      GenomicRanges::as.data.frame(row.names = "tx_id") %>%
      dplyr::filter(.env$start <= start & end <= .env$end) %>%
      dplyr::mutate(
        tss = ifelse(strand == "+", start, end),
        score = abs(tss - ref_position),
        method = "Distance_TSS",
        gene = forcats::fct_reorder(tx_name, tss)
      ) %>%
      dplyr::select(gene, method, score)
  }

  return(df)
}
