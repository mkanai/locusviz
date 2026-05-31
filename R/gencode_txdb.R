#' Create a TxDb object from GENCODE annotations
#'
#' This function creates a TxDb object from GENCODE annotation files, filtering
#' for canonical transcripts and MANE Select transcripts.
#'
#' @param version Character string specifying the GENCODE version (default: '19')
#' @param genome Character string specifying the genome build: 'hg19' or 'hg38'
#' @param chrs Character vector of chromosome names to keep (default: chr1-chr22)
#'
#' @return A TxDb object containing filtered GENCODE annotations
#'
#' @importFrom rtracklayer import
#' @importFrom data.table fread
#' @importFrom GenomicRanges elementMetadata
#' @importFrom GenomeInfoDb keepSeqlevels Seqinfo
#' @importFrom dplyr filter
#' @importFrom stringr str_split_fixed
#'
#' @examples
#' \dontrun{
#' # Create TxDb for hg38
#' txdb_hg38 <- gencode_txdb(genome = "hg38")
#'
#' # Create TxDb for hg19 with specific chromosomes
#' txdb_hg19 <- gencode_txdb(genome = "hg19", chrs = c("chr1", "chr2"))
#' }
#'
#' @export
# cf. GenomicState::gencode_txdb
gencode_txdb <- function(version = "19",
                         genome = c("hg19", "hg38"),
                         chrs = paste0("chr", seq_len(22))) {
  if (!requireNamespace("txdbmaker", quietly = TRUE)) {
    stop("Package 'txdbmaker' is required for gencode_txdb(). ",
      "Install it with: BiocManager::install('txdbmaker')",
      call. = FALSE
    )
  }

  genome <- match.arg(genome)

  # The GENCODE GTFs are ~55 MB; bump the download.file timeout (default 60s)
  # so the transfer has time to finish on slower connections.
  old_timeout <- getOption("timeout")
  options(timeout = max(old_timeout, 1200))
  on.exit(options(timeout = old_timeout), add = TRUE)

  ## Locate file
  # EBI serves the same paths over HTTPS; prefer it over the flaky ftp:// host.
  if (genome == "hg19") {
    gtf_file <-
      "https://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_human/release_19/gencode.v19.annotation.gff3.gz"
    canonical_transcript_file <- system.file(package = "locusviz", "extdata", "canonical_transcripts_grch37.tsv.gz")
  } else if (genome == "hg38") {
    # gnomAD v4.1.1 uses VEP v105 / GENCODE v39 on GRCh38.
    gtf_file <-
      "https://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_human/release_39/gencode.v39.annotation.gff3.gz"
    # https://github.com/broadinstitute/gnomad-browser/blob/master/data/docs/gene_models.md#canonical-transcripts
    canonical_transcript_file <- system.file(package = "locusviz", "extdata", "canonical_transcripts_grch38_v39.tsv.gz")
  }


  ## Import the data
  message(paste(Sys.time(), "importing", gtf_file))
  gencode_gtf <- rtracklayer::import(gtf_file)
  canonical_transcripts <- data.table::fread(canonical_transcript_file, data.table = F)

  # filter transcripts by
  # 1) MANE_Select, or
  # 2) Gencode basic, 3) protein coding, 4) ensembl canonical, and 5) has HGNC ID
  filter_transcripts <- function(gencode_gtf, canonical_transcripts) {
    meta <- GenomicRanges::elementMetadata(gencode_gtf)
    MANE_Select <- any(meta[, "tag"] == "MANE_Select")

    basic <- any(meta[, "tag"] == "basic")
    protein_coding <- meta$gene_type == "protein_coding"
    canonical_transcripts <- dplyr::filter(canonical_transcripts, !(gene_id %in% stringr::str_split_fixed(meta$gene_id[MANE_Select], "\\.", 2)))
    canonical <- stringr::str_split_fixed(meta$transcript_id, "\\.", 2)[, 1] %in% canonical_transcripts$canonical_transcript_id
    has_hgnc_id <- if ("hgnc_id" %in% colnames(meta)) {
      !is.na(meta$hgnc_id)
    } else {
      hgnc <- data.table::fread(system.file(package = "locusviz", "extdata", "ensembl_hgncid.tsv.gz"), data.table = F, col.names = c("gene_id", "transcript_id", "hgnc_id")) %>%
        dplyr::filter(!is.na(hgnc_id) & hgnc_id != "")
      stringr::str_split_fixed(meta$gene_id, "\\.", 2)[, 1] %in% hgnc$gene_id
    }
    return(gencode_gtf[MANE_Select | (basic & protein_coding & canonical & has_hgnc_id)])
  }
  gencode_gtf <- filter_transcripts(gencode_gtf, canonical_transcripts)

  ## Keep only the main chrs
  message(paste(Sys.time(), "keeping relevant chromosomes"))
  gencode_gtf <- GenomeInfoDb::keepSeqlevels(gencode_gtf, chrs,
    pruning.mode = "coarse"
  )

  # Doesn't work because of the different seqlevels
  # txdb <- makeTxDbFromGFF(
  #     gtf_file,
  #     organism = 'Homo sapiens',
  #     chrominfo = Seqinfo(genome="hg19")
  # )

  message(paste(Sys.time(), "preparing metadata"))
  metadata <- txdbmaker:::.prepareGFFMetadata(
    file = gtf_file,
    dataSource = NA,
    organism = "Homo sapiens",
    taxonomyId = NA,
    miRBaseBuild = NA,
    metadata = NULL
  )

  message(paste(Sys.time(), "building the txdb object"))
  gr <- txdbmaker:::.tidy_seqinfo(
    gr = gencode_gtf,
    circ_seqs = NULL,
    chrominfo = GenomeInfoDb::Seqinfo(genome = genome)
  )

  ## Prune again since GenomeInfoDb::Seqinfo() will return many seqlevels
  gr <-
    GenomeInfoDb::keepSeqlevels(gr, chrs, pruning.mode = "coarse")

  # For GenomicFeatures >= 1.43.6
  gr$transcript_id <- gr$gene_name
  gr$Name <- gr$gene_name

  txdb <-
    txdbmaker::makeTxDbFromGRanges(gr, metadata = metadata)
  return(txdb)
}

#' Extract TSS and gene body information from TxDb
#'
#' This function extracts transcription start site (TSS) and gene body information
#' from a TxDb object for specified chromosomes.
#'
#' @param txdb A TxDb object containing transcript annotations
#' @param chromosomes Character vector of chromosome names to process
#'   (default: chr1-chr22, chrX)
#'
#' @return A data frame containing transcript information with columns:
#'   tx_id, tx_name, chromosome, strand, start, end, tss
#'
#' @importFrom purrr map_dfr
#' @importFrom GenomicRanges GRanges as.data.frame
#' @importFrom IRanges IRanges
#' @importFrom biovizBase crunch
#' @importFrom dplyr filter group_by summarize arrange
#'
#' @export
get_tss_gene_body <- function(txdb, chromosomes = paste0("chr", c(seq(22), "X"))) {
  purrr::map_dfr(chromosomes, function(chrom) {
    gr <- GenomicRanges::GRanges(
      seqnames = chrom,
      ranges = IRanges::IRanges(1, .Machine$integer.max)
    )
    biovizBase::crunch(txdb, which = gr) %>%
      GenomicRanges::as.data.frame() %>%
      dplyr::filter(type == "exon") %>%
      dplyr::group_by(tx_id) %>%
      dplyr::summarize(
        tx_name = tx_name[1],
        chromosome = seqnames[1],
        strand = strand[1],
        start = min(start),
        end = max(end),
        tss = ifelse(strand == "+", start, end)
      ) %>%
      dplyr::arrange(chromosome, start)
  })
}

#' Write TxDb and TSS data files
#'
#' This function generates and saves TxDb SQLite files and TSS RData files
#' for both hg19 and hg38 genome builds. This is an internal function used
#' to prepare the package data files.
#'
#' @param chromosomes Character vector of chromosome names to include
#'   (default: chr1-chr22, chrX)
#'
#' @return NULL (invisibly). Files are written to inst/extdata/ and data/ directories
#'
#' @importFrom AnnotationDbi saveDb
#' @importFrom dplyr mutate
#' @importFrom stringr str_remove
#'
#' @examples
#' \dontrun{
#' # Generate all data files
#' write_txdb_files()
#' }
#'
#' @export
write_txdb_files <- function(chromosomes = paste0("chr", c(seq(22), "X"))) {
  txdb_v39_hg38 <- gencode_txdb(genome = "hg38", chrs = chromosomes)
  AnnotationDbi::saveDb(txdb_v39_hg38, "inst/extdata/txdb_v39_hg38.sqlite")

  txdb_v19_hg19 <- gencode_txdb(genome = "hg19", chrs = chromosomes)
  AnnotationDbi::saveDb(txdb_v19_hg19, "inst/extdata/txdb_v19_hg19.sqlite")

  tss_v39_hg38 <- get_tss_gene_body(txdb_v39_hg38, chromosomes)
  save(tss_v39_hg38, file = "data/tss_v39_hg38.RData")

  tss_v19_hg19 <- get_tss_gene_body(txdb_v19_hg19, chromosomes) %>%
    dplyr::mutate(chromosome = stringr::str_remove(chromosome, "^chr"))
  save(tss_v19_hg19, file = "data/tss_v19_hg19.RData")
}
