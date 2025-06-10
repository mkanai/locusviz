#' Create gene track panel
#'
#' This function creates a gene track panel showing gene annotations for a genomic region,
#' with exons, introns, and gene direction indicated.
#'
#' @param chromosome Character string specifying the chromosome (e.g., "chr1" or "1")
#' @param start Numeric start position of the genomic region
#' @param end Numeric end position of the genomic region
#' @param genome_build Character string specifying genome build: 'hg19' or 'hg38'
#' @param txdb Optional TxDb object. If NULL, will be loaded based on genome_build
#' @param highlight_pos Numeric vector of positions to highlight with diamonds
#' @param highlight_pos_y Numeric y-position for highlight markers (default: 1)
#' @param gene_col Color for gene tracks (default: blue from calma_azules palette)
#' @param fontsize Numeric font size for plot text (default: 8)
#' @param point.size Numeric size for highlight points (default: 2)
#' @param label.size Numeric size for gene labels (default: 2)
#' @param arrow.rate Numeric rate for directional arrows on genes (default: 0.015)
#' @param length Unit object specifying arrow length (default: unit(0.1, "cm"))
#' @param background.layers List of additional ggplot2 layers to add as background
#'
#' @return A ggplot2 object showing the gene track panel
#'
#' @import ggplot2
#' @importFrom ggbio geom_alignment
#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges IRanges
#' @importFrom BuenColors jdb_palette pretty_plot
#' @importFrom stringr str_starts str_remove
#' @importFrom grid unit
#'
#' @examples
#' \dontrun{
#' # Basic gene panel
#' plot_gene_panel("chr1", 1000000, 2000000)
#'
#' # With highlighted positions
#' plot_gene_panel(
#'   "chr1", 1000000, 2000000,
#'   highlight_pos = c(1234567, 1567890),
#'   genome_build = "hg38"
#' )
#' }
#'
#' @export
plot_gene_panel <- function(chromosome,
                            start,
                            end,
                            genome_build = c("hg19", "hg38"),
                            txdb = NULL,
                            highlight_pos = NULL,
                            highlight_pos_y = 1,
                            gene_col = BuenColors::jdb_palette("calma_azules")[6],
                            fontsize = 8,
                            point.size = 2,
                            label.size = 2,
                            arrow.rate = 0.015,
                            length = unit(0.1, "cm"),
                            background.layers = NULL) {
  genome_build <- match.arg(genome_build)

  if (is.null(txdb)) {
    txdb <- load_txdb(genome_build)
  }
  if (!stringr::str_starts(chromosome, "chr")) {
    chromosome <- paste0("chr", chromosome)
  }
  if (!is.null(background.layers) & !is.list(background.layers)) {
    background.layers <- list(background.layers)
  }
  if (length(highlight_pos) == 0) {
    highlight_pos <- NULL
  }

  gr <- GenomicRanges::GRanges(seqnames = chromosome, ranges = IRanges(start, end))

  p_gene <- ggplot() +
    background.layers +
    highlight_vline(highlight_pos) +
    tryCatch(
      {
        ggbio::geom_alignment(
          txdb,
          which = gr,
          cds.rect.h = 0.1,
          color = gene_col,
          fill = gene_col,
          label.size = label.size,
          arrow.rate = arrow.rate,
          length = length
        )
      },
      error = function(msg) {
        message(conditionMessage(msg))
        NULL
      }
    ) +
    or_missing(
      !is.null(highlight_pos),
      geom_point(
        aes(x = highlight_pos, y = highlight_pos_y),
        shape = 18,
        color = BuenColors::jdb_palette("solar_extra")[1],
        size = point.size
      )
    ) +
    BuenColors::pretty_plot(fontsize = fontsize) +
    theme(
      panel.border = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank()
    ) +
    labs(x = sprintf("Chromosome %s", stringr::str_remove(chromosome, "^chr"))) +
    coord_cartesian(xlim = c(start, end), clip = "off")

  return(p_gene)
}
