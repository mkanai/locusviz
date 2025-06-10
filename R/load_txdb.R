#' Load transcript database for gene annotations
#'
#' This function loads a pre-built TxDb object for the specified genome build,
#' or returns a user-provided TxDb object.
#'
#' @param genome_build Character string specifying the genome build: "hg19" or "hg38"
#' @param txdb Optional TxDb object. If provided, this will be returned instead
#'   of loading the default TxDb for the genome build
#'
#' @return A TxDb object containing transcript annotations for the specified
#'   genome build
#'
#' @importFrom AnnotationDbi loadDb
#'
#' @examples
#' \dontrun{
#' # Load default TxDb for hg38
#' txdb <- load_txdb("hg38")
#'
#' # Use custom TxDb
#' custom_txdb <- TxDb.Hsapiens.UCSC.hg38.knownGene
#' txdb <- load_txdb("hg38", txdb = custom_txdb)
#' }
#'
#' @export
load_txdb <- function(genome_build = c("hg19", "hg38"),
                      txdb = NULL) {
  genome_build <- match.arg(genome_build)

  if (is.null(txdb)) {
    txdb <- switch(genome_build,
      hg19 = AnnotationDbi::loadDb(
        system.file(package = "locusviz", "extdata", "txdb_v19_hg19.sqlite")
      ),
      hg38 = AnnotationDbi::loadDb(
        system.file(package = "locusviz", "extdata", "txdb_v35_hg38.sqlite")
      )
    )
  }

  return(txdb)
}
