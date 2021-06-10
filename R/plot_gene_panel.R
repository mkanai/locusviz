#' Title
#'
#' @param chromosome
#' @param start
#' @param end
#' @param genome_build
#' @param txdb
#' @param highlight_pos
#' @param gene_col
#' @param label.size
#'
#' @import ggplot2
#'
#' @return
#' @export
plot_gene_panel = function(chromosome,
                           start,
                           end,
                           genome_build = c('hg19', 'hg38'),
                           txdb = NULL,
                           highlight_pos = NULL,
                           gene_col = BuenColors::jdb_palette("calma_azules")[6],
                           fontsize = 8,
                           point.size = 2,
                           label.size = 2,
                           arrow.rate = 0.015,
                           length = unit(0.1, "cm")) {
  genome_build = match.arg(genome_build)

  if (is.null(txdb)) {
    txdb = load_txdb(genome_build)
  }
  if (!stringr::str_starts(chromosome, "chr")) {
    chromosome = paste0("chr", chromosome)
  }

  gr = GenomicRanges::GRanges(seqnames = chromosome, ranges = IRanges(start, end))

  p_gene = ggplot() +
    highlight_vline(highlight_pos) +
    tryCatch({
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
    }, error = function(msg) {
      message(msg)
      NULL
    }) +
    or_missing(
      !is.null(highlight_pos),
      geom_point(
        aes(x = highlight_pos, y = 1),
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
