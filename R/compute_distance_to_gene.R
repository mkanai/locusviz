#' Compute distance from reference position to genes
#'
#' This function calculates the distance from a reference genomic position to
#' genes in a specified region, using either gene body or TSS distance metrics.
#'
#' @param txdb A TxDb object containing transcript annotations
#' @param chromosome Character string specifying the chromosome (e.g., "chr1" or "1")
#' @param start Numeric start position of the genomic region
#' @param end Numeric end position of the genomic region
#' @param ref_position Numeric reference position from which to calculate distances
#' @param type Character string specifying distance type: "GB" (gene body) or
#'   "TSS" (transcription start site)
#'
#' @return A data frame with columns: gene (factor ordered by position),
#'   method ("Distance" or "Distance_TSS"), and score (numeric distance)
#'
#' @importFrom GenomicRanges GRanges as.data.frame promoters
#' @importFrom IRanges IRanges
#' @importFrom biovizBase crunch
#' @importFrom dplyr filter group_by summarize mutate select
#' @importFrom tibble tibble
#' @importFrom stringr str_starts str_c
#' @importFrom forcats fct_reorder
#'
#' @examples
#' \dontrun{
#' # Calculate distance to gene bodies
#' distances <- compute_distance_to_gene(
#'   txdb, "chr1", 1000000, 2000000, 1500000,
#'   type = "GB"
#' )
#' }
#'
#' @export
compute_distance_to_gene <- function(txdb,
                                     chromosome,
                                     start,
                                     end,
                                     ref_position,
                                     type = c("GB", "TSS")) {
  type <- match.arg(type)

  if (!stringr::str_starts(chromosome, "chr")) {
    chromosome <- stringr::str_c("chr", chromosome)
  }

  if (type == "GB") {
    gr <- GenomicRanges::GRanges(seqnames = chromosome, ranges = IRanges(start, end))
    gr.txdb <- tryCatch(
      {
        biovizBase::crunch(txdb, which = gr)
      },
      error = function(msg) {
        message(msg)
        return(GenomicRanges::GRanges())
      }
    )
    if (length(gr.txdb) == 0) {
      return(tibble::tibble(gene = NA, method = "Distance", score = NA))
    }

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
    df <-
      GenomicRanges::promoters(txdb, upstream = 0, downstream = 0) %>%
      subset(seqnames == chromosome) %>%
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
