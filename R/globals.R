# Global variables used in NSE (non-standard evaluation) contexts
# such as dplyr pipelines, ggplot2 aes(), and data.table operations.
# Declaring them here suppresses R CMD check NOTEs about
# "no visible binding for global variable".

utils::globalVariables(c(
  ".", ".env", ":=",
  "ClinicalSignificance", "GeneSymbol", "Pfam_ID", "Pfam_description",
  "Type",
  "alt", "chromosome", "cohort", "cohort_shapes", "consequence",
  "correlation", "cs_id",
  "data", "degree",
  "end",
  "gene", "gene_id", "gof_overlap",
  "hgnc_id",
  "i", "idx", "interval", "isCircular", "item",
  "label", "large_sv", "lead_variant", "locus", "lof_overlap",
  "max_maf", "max_maf_bin", "max_pip", "max_pip_bin", "method",
  "new_position", "nlog10p",
  "pindex", "pip", "pop", "position", "position2", "protein_id",
  "pvalue",
  "r2", "ref", "row",
  "score", "se", "seqlengths", "seqnames", "set", "sets",
  "sets_collapsed", "signed_pip", "start", "strand", "symbol",
  "susie.beta_posterior",
  "total", "trait", "tss", "tx_id", "tx_name", "txdb_v39_hg38", "type",
  "variant", "variant2", "variant_normalized", "variation2",
  "x", "xend", "xmax", "xmin", "y", "yend", "ymax", "ymin"
))
