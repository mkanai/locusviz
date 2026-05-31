#' TSS and gene body data for GENCODE v19 (hg19/GRCh37)
#'
#' Transcription start site and gene body information extracted from
#' GENCODE v19 annotations for the GRCh37/hg19 genome build.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{tx_id}{Transcript ID}
#'   \item{tx_name}{Transcript/gene name}
#'   \item{chromosome}{Chromosome (without 'chr' prefix)}
#'   \item{strand}{Strand (+ or -)}
#'   \item{start}{Transcript start position}
#'   \item{end}{Transcript end position}
#'   \item{tss}{Transcription start site position}
#' }
"tss_v19_hg19"

#' TSS and gene body data for GENCODE v34 (hg38/GRCh38)
#'
#' Transcription start site and gene body information extracted from
#' GENCODE v34 annotations for the GRCh38/hg38 genome build.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{tx_id}{Transcript ID}
#'   \item{tx_name}{Transcript/gene name}
#'   \item{chromosome}{Chromosome}
#'   \item{strand}{Strand (+ or -)}
#'   \item{start}{Transcript start position}
#'   \item{end}{Transcript end position}
#'   \item{tss}{Transcription start site position}
#' }
"tss_v34_hg38"

#' TSS and gene body data for GENCODE v39 (hg38/GRCh38)
#'
#' Transcription start site and gene body information extracted from
#' GENCODE v39 annotations for the GRCh38/hg38 genome build. GENCODE v39
#' matches the gene models used by gnomAD v4.1.1 (VEP v105).
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{tx_id}{Transcript ID}
#'   \item{tx_name}{Transcript/gene name}
#'   \item{chromosome}{Chromosome}
#'   \item{strand}{Strand (+ or -)}
#'   \item{start}{Transcript start position}
#'   \item{end}{Transcript end position}
#'   \item{tss}{Transcription start site position}
#' }
"tss_v39_hg38"
