#' Title
#'
#' @param genome_build
#' @param txdb
#'
#' @return
#' @export
load_txdb <- function(genome_build = c('hg19', 'hg38'),
                      txdb = NULL) {
  genome_build = match.arg(genome_build)

  if (is.null(txdb)) {
    txdb = switch(
      genome_build,
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
