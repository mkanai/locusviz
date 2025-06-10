#' Get Pfam domain annotations for a gene
#'
#' This function retrieves Pfam domain annotations for a specified gene and maps
#' them to genomic coordinates.
#'
#' @param gene_symbol Character string specifying the gene symbol
#' @param remove.unknown.domains Logical indicating whether to remove unknown
#'   domains (DUF domains). Default: TRUE
#' @param genome_build Character string specifying the genome build: 'hg19' or 'hg38'
#' @param txdb Optional TxDb object. If NULL, will be loaded based on genome_build
#' @param pfam Data frame containing Pfam domain annotations. Default reads from
#'   a specific RDS file path
#'
#' @return A data frame containing Pfam domain information with genomic coordinates.
#'   Columns include: protein_id, chromosome, start, end, Pfam_ID, Pfam_description
#'
#' @importFrom dplyr filter left_join mutate select distinct
#' @importFrom stringr str_detect str_remove
#' @importFrom IRanges IRanges
#' @importFrom GenomicFeatures transcripts
#' @importFrom biovizBase crunch
#' @importFrom purrr map_dfr
#' @importFrom tidyr drop_na
#'
#' @examples
#' \dontrun{
#' # Get Pfam domains for a gene
#' domains <- get_pfam_domains("BRCA2", genome_build = "hg38")
#'
#' # Include unknown domains
#' all_domains <- get_pfam_domains("BRCA2", remove.unknown.domains = FALSE)
#' }
#'
#' @export
get_pfam_domains <- function(gene_symbol,
                             remove.unknown.domains = TRUE,
                             genome_build = c("hg19", "hg38"),
                             txdb = NULL,
                             pfam = readRDS("~/src/github.com/mkanai/ukbb-finemapping/data/pfam.domains.rds")) {
  genome_build <- match.arg(genome_build)
  if (is.null(txdb)) {
    txdb <- load_txdb(genome_build)
  }

  # Domains
  pfam.domains <-
    dplyr::filter(pfam, gene == gene_symbol)
  if (remove.unknown.domains) {
    pfam.domains <- dplyr::filter(pfam.domains, !stringr::str_detect(Pfam_ID, "^DUF"))
  }
  if (nrow(pfam.domains) == 0) {
    return(data.frame())
  }

  # cf. ensembldb::proteinToGenome
  x <-
    with(
      pfam.domains,
      IRanges(
        start = Pfam_start,
        end = Pfam_end,
        names = protein_id
      )
    )
  coords_cds <- ensembldb:::.proteinCoordsToTx(x)

  gr <-
    range(
      subset(
        GenomicFeatures::transcripts(txdb),
        tx_name == gene_symbol
      ),
      ignore.strand = TRUE
    )
  gr <- biovizBase::crunch(txdb, gr, ratio = 0.0025)

  cds_genome <- subset(gr, type == "cds")
  strand <- unique(as.character(strand(cds_genome)))
  if (strand == "-") {
    cds_genome <- rev(cds_genome)
  }

  pfam.domains.genome <-
    purrr::map_dfr(
      seq_along(x),
      function(i) {
        cds <- coords_cds[i]
        prt <- x[i]
        res <-
          ensembldb:::.to_genome(cds_genome, cds) %>%
          as.data.frame() %>%
          dplyr::mutate(
            chromosome = seqnames,
            protein_id = names(prt),
            protein_start = start(prt),
            protein_end = end(prt)
          ) %>%
          dplyr::select(chromosome, start, end, dplyr::starts_with("protein_"))
        return(res)
      }
    )


  pfam.domains <-
    dplyr::left_join(
      pfam.domains,
      pfam.domains.genome,
      by = c(
        "protein_id" = "protein_id",
        "Pfam_start" = "protein_start",
        "Pfam_end" = "protein_end"
      )
    ) %>%
    tidyr::drop_na(chromosome, start, end) %>%
    dplyr::distinct(
      protein_id,
      chromosome,
      start,
      end,
      Pfam_ID,
      Pfam_description
    )

  if (genome_build == "hg19") {
    pfam.domains <- dplyr::mutate(pfam.domains,
      chromosome = stringr::str_remove(chromosome, "^chr")
    )
  }

  return(pfam.domains)
}
