#' Create gene score visualization panel
#'
#' This function creates a dot plot showing gene scores from various methods,
#' including optional distance-based scores. The plot displays genes on the x-axis
#' and scoring methods on the y-axis, with dot size and opacity representing score magnitude.
#'
#' @param chromosome Character string specifying the chromosome
#' @param start Numeric start position of the genomic region
#' @param end Numeric end position of the genomic region
#' @param gene_score.data Data frame with columns: gene, score, method
#' @param genome_build Character string specifying genome build: 'hg19' or 'hg38'
#' @param txdb Optional TxDb object. If NULL, will be loaded based on genome_build
#' @param highlight_pos Optional numeric position to highlight for distance calculations
#' @param append.distance Logical whether to append distance-based scores (default: TRUE)
#' @param distance.type Character string specifying distance type: "GB" (gene body)
#'   or "TSS" (transcription start site)
#' @param method.levels Character vector specifying the order of scoring methods
#' @param colors Named vector of colors for each method
#' @param fontsize Numeric font size for plot text (default: 8)
#' @param area.max_size Numeric maximum size for dots (default: 4)
#'
#' @return A ggplot2 object showing the gene score panel
#'
#' @import ggplot2
#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges IRanges
#' @importFrom biovizBase crunch
#' @importFrom dplyr bind_rows filter group_by mutate
#' @importFrom tibble tibble
#'
#' @examples
#' \dontrun{
#' # Create gene score panel
#' scores <- data.frame(
#'   gene = c("GENE1", "GENE2", "GENE3"),
#'   score = c(0.8, 0.6, 0.9),
#'   method = "MAGMA"
#' )
#' plot_gene_score_panel("chr1", 1000000, 2000000, scores)
#' }
#'
#' @export
plot_gene_score_panel <- function(chromosome,
                                  start,
                                  end,
                                  gene_score.data,
                                  genome_build = c("hg19", "hg38"),
                                  txdb = NULL,
                                  highlight_pos = NULL,
                                  append.distance = TRUE,
                                  distance.type = c("GB", "TSS"),
                                  method.levels = NULL,
                                  colors = NULL,
                                  fontsize = 8,
                                  area.max_size = 4) {
  genome_build <- match.arg(genome_build)
  distance.type <- match.arg(distance.type)

  if (is.null(method.levels)) {
    method.levels <- c(
      or_missing(append.distance, "Distance"),
      unique(as.character(gene_score.data$method))
    )
  }

  txdb <- load_txdb(genome_build, txdb)

  gr <- GenomicRanges::GRanges(seqnames = chromosome, ranges = IRanges(start, end))
  gr.txdb <- tryCatch(
    {
      biovizBase::crunch(txdb, which = gr)
    },
    error = function(msg) {
      message(msg)
      return(GenomicRanges::GRanges())
    }
  )
  if (length(gr.txdb) == 0) {
    plotted.genes <- c("NA")
    distance.df <- NULL
    gene_score.data <- tibble::tibble(gene = plotted.genes, score = NA, method = method.levels)
  } else {
    plotted.genes <- unique(gr.txdb$tx_name)
    distance.df <- or_missing(
      append.distance,
      compute_distance_to_gene(txdb, chromosome, start, end, highlight_pos, distance.type)
    )
  }

  gene_score.data <-
    dplyr::bind_rows(distance.df, gene_score.data) %>%
    dplyr::filter(gene %in% plotted.genes) %>%
    dplyr::group_by(method) %>%
    dplyr::mutate(score = normalize_rank(score)) %>%
    dplyr::mutate(
      gene = factor(gene, levels = levels(distance.df$gene)),
      method = factor(method, levels = rev(method.levels))
    )

  p_gene_score <-
    ggplot(gene_score.data, aes(gene, method)) +
    geom_point(aes(
      size = score,
      color = method,
      alpha = score
    )) +
    get_default_theme(fontsize = fontsize, hide.xtitle = TRUE, hide.ytitle = TRUE) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5),
      panel.grid.major.x = element_line(),
      legend.position = "none"
    ) +
    scale_size_area(max_size = area.max_size) +
    scale_y_discrete(drop = FALSE) +
    or_missing(!is.null(colors), scale_color_manual(values = colors))

  return(p_gene_score)
}
