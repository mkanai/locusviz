# cf. GenomicState::gencode_txdb
gencode_txdb <- function(version = '19',
                         genome = c('hg19', 'hg38'),
                         chrs = paste0('chr', seq_len(22))) {
  genome <- match.arg(genome)

  ## Locate file
  if (genome == 'hg19') {
    gtf_file <-
      'ftp://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_human/release_19/gencode.v19.annotation.gff3.gz'
    canonical_transcript_file <- system.file(package = "locusviz", "extdata", "canonical_transcripts_grch37.tsv.gz")
  } else if (genome == 'hg38') {
    gtf_file <-
      'ftp://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_human/release_35/gencode.v35.annotation.gff3.gz'
    # https://github.com/broadinstitute/gnomad-browser/blob/master/data/docs/gene_models.md#canonical-transcripts
    canonical_transcript_file <- system.file(package = "locusviz", "extdata", "canonical_transcripts_grch38_v35.tsv.gz")
  }


  ## Import the data
  message(paste(Sys.time(), 'importing', gtf_file))
  gencode_gtf <- rtracklayer::import(gtf_file)
  canonical_transcripts <- data.table::fread(canonical_transcript_file, data.table = F)

  # filter transcripts by
  # 1) MANE_Select, or
  # 2) Gencode basic, 3) protein coding, 4) ensembl canonical, and 5) has HGNC ID
  filter_transcripts = function(gencode_gtf, canonical_transcripts) {
    meta = GenomicRanges::elementMetadata(gencode_gtf)
    MANE_Select = any(meta[, "tag"] == "MANE_Select")

    basic = any(meta[, "tag"] == "basic")
    protein_coding = meta$gene_type == "protein_coding"
    canonical_transcripts = dplyr::filter(canonical_transcripts, !(gene_id %in% stringr::str_split_fixed(meta$gene_id[MANE_Select], "\\.", 2)))
    canonical = stringr::str_split_fixed(meta$transcript_id, "\\.", 2)[,1] %in% canonical_transcripts$canonical_transcript_id
    has_hgnc_id = if ("hgnc_id" %in% colnames(meta)) {
      !is.na(meta$hgnc_id)
    } else {
      hgnc = data.table::fread(system.file(package = "locusviz", "extdata", "ensembl_hgncid.tsv.gz"), data.table = F, col.names = c("gene_id", "transcript_id", "hgnc_id")) %>%
        dplyr::filter(!is.na(hgnc_id) & hgnc_id != "")
      stringr::str_split_fixed(meta$gene_id, "\\.", 2)[,1] %in% hgnc$gene_id
    }
    return(gencode_gtf[MANE_Select | (basic & protein_coding & canonical & has_hgnc_id)])
  }
  gencode_gtf <- filter_transcripts(gencode_gtf, canonical_transcripts)

  ## Keep only the main chrs
  message(paste(Sys.time(), 'keeping relevant chromosomes'))
  gencode_gtf <- GenomeInfoDb::keepSeqlevels(gencode_gtf, chrs,
                                             pruning.mode = 'coarse')

  # Doesn't work because of the different seqlevels
  # txdb <- makeTxDbFromGFF(
  #     gtf_file,
  #     organism = 'Homo sapiens',
  #     chrominfo = Seqinfo(genome="hg19")
  # )

  message(paste(Sys.time(), 'preparing metadata'))
  metadata <- GenomicFeatures:::.prepareGFFMetadata(
    file = gtf_file,
    dataSource = NA,
    organism = 'Homo sapiens',
    taxonomyId = NA,
    miRBaseBuild = NA,
    metadata = NULL
  )

  message(paste(Sys.time(), 'building the txdb object'))
  gr <- GenomicFeatures:::.tidy_seqinfo(
    gr = gencode_gtf,
    circ_seqs = GenomicFeatures::DEFAULT_CIRC_SEQS,
    chrominfo = GenomeInfoDb::Seqinfo(genome = genome)
  )

  ## Prune again since GenomeInfoDb::Seqinfo() will return many seqlevels
  gr <-
    GenomeInfoDb::keepSeqlevels(gr, chrs, pruning.mode = 'coarse')

  # For GenomicFeatures >= 1.43.6
  gr$transcript_id = gr$gene_name
  gr$Name = gr$gene_name

  txdb <-
    GenomicFeatures::makeTxDbFromGRanges(gr, metadata = metadata)
  return(txdb)
}

get_tss_gene_body = function(txdb, chromosomes = paste0("chr", c(seq(22), "X"))) {
  purrr::map_dfr(chromosomes, function(chrom) {
    gr = GenomicRanges::GRanges(seqnames = chrom,
                                ranges = IRanges::IRanges(1, .Machine$integer.max))
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

write_txdb_files = function(chromosomes = paste0('chr', c(seq(22), 'X'))) {
  txdb_v35_hg38 = gencode_txdb(genome = 'hg38', chrs = chromosomes)
  AnnotationDbi::saveDb(txdb_v35_hg38, "inst/extdata/txdb_v35_hg38.sqlite")

  txdb_v19_hg19 = gencode_txdb(genome = 'hg19', chrs = chromosomes)
  AnnotationDbi::saveDb(txdb_v19_hg19, "inst/extdata/txdb_v19_hg19.sqlite")

  tss_v34_hg38 = get_tss_gene_body(txdb_v34_hg38, chromosomes)
  save(tss_v34_hg38, file = "data/tss_v34_hg38.RData")

  tss_v19_hg19 = get_tss_gene_body(txdb_v19_hg19, chromosomes) %>%
    dplyr::mutate(chromosome = stringr::str_remove(chromosome, "^chr"))
  save(tss_v19_hg19, file = "data/tss_v19_hg19.RData")
}
