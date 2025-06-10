#' Liftover variant positions between genome builds
#'
#' This function converts variant positions between hg19 and hg38 genome builds
#' using UCSC chain files.
#'
#' @param variant Character vector of variant identifiers in the format
#'   "chromosome:position:ref:alt"
#' @param genome_build Character string specifying the target genome build:
#'   "hg19" (lifts from hg38 to hg19) or "hg38" (lifts from hg19 to hg38)
#'
#' @return A data frame with columns: variant (original), new_variant,
#'   new_chromosome, new_position, new_ref, new_alt. Variants that fail to
#'   lift over will have NA values in the new_* columns
#'
#' @importFrom GenomicRanges makeGRangesFromDataFrame
#' @importFrom rtracklayer import.chain liftOver
#' @importFrom ensembldb seqlevelsStyle
#' @importFrom dplyr mutate select left_join starts_with
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble as_tibble
#' @importFrom stringr str_remove str_c
#'
#' @examples
#' \dontrun{
#' # Liftover from hg19 to hg38
#' lifted <- liftover_variant(c("1:1000000:A:G", "2:2000000:C:T"), "hg38")
#'
#' # Liftover from hg38 to hg19
#' lifted <- liftover_variant(c("chr1:1000000:A:G"), "hg19")
#' }
#'
#' @export
liftover_variant <- function(variant, genome_build = c("hg19", "hg38")) {
  genome_build <- match.arg(genome_build)

  uniq_variant <- unique(variant)

  gr <- parse_variant(uniq_variant) %>%
    dplyr::mutate(
      start = position - 1,
      end = position
    ) %>%
    GenomicRanges::makeGRangesFromDataFrame(seqnames.field = "chromosome", keep.extra.columns = TRUE)

  chain <- switch(genome_build,
    hg19 = rtracklayer::import.chain(
      system.file(package = "locusviz", "extdata", "hg19ToHg38.over.chain")
    ),
    hg38 = rtracklayer::import.chain(
      system.file(package = "locusviz", "extdata", "hg38ToHg19.over.chain")
    )
  )
  ensembldb::seqlevelsStyle(gr) <- "UCSC"

  ret <-
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
    ret <- dplyr::mutate(ret, seqnames = stringr::str_remove(seqnames, "^chr"))
  }

  ret <- dplyr::mutate(
    ret,
    variant = uniq_variant,
    new_variant = stringr::str_c(seqnames, end, ref, alt, sep = ":"),
    new_chromosome = seqnames,
    new_position = end,
    new_ref = ref,
    new_alt = alt
  ) %>%
    dplyr::select(variant, dplyr::starts_with("new_"))
  ret <- dplyr::left_join(data.frame(variant = variant), ret)

  return(ret)
}
