#' Create gene track panel
#'
#' Builds a gene track panel showing gene annotations for a genomic region.
#' Defaults to a pure-ggplot2 implementation (`engine = "native"`) that
#' packs genes into rows so neither gene bodies nor their text labels
#' collide horizontally — a key improvement over `ggbio::geom_alignment`,
#' which places labels at fixed offsets and overlaps unreadably in dense
#' loci. Pass `engine = "ggbio"` to fall back to the original
#' `ggbio::geom_alignment` rendering.
#'
#' Row assignment (native engine) packs each gene's body interval *unioned
#' with* the bounding box of its text label (estimated from
#' `chars_per_panel`). A row can hold multiple genes only when their labels
#' also fit side-by-side; densely labelled regions naturally spill onto
#' more rows instead of stacking labels on top of each other.
#'
#' Strand direction is shown by repeated open arrowheads spaced along each
#' gene body, with spacing controlled by `arrow.rate`.
#'
#' @param chromosome Character string specifying the chromosome (e.g., "chr1" or "1")
#' @param start Numeric start position of the genomic region
#' @param end Numeric end position of the genomic region
#' @param genome_build Character string specifying genome build: 'hg19' or 'hg38'
#' @param txdb Optional TxDb object. If NULL, will be loaded based on genome_build
#' @param highlight_pos Numeric vector of positions to highlight with diamonds
#' @param highlight_pos_y Numeric y-position for highlight markers
#'   (default: 1, i.e. on the bottom gene row). Identical across engines.
#' @param gene_col Color for gene tracks (default: blue from calma_azules palette)
#' @param fontsize Numeric font size for plot text (default: 7)
#' @param point.size Numeric size for highlight points (default: 2)
#' @param label.size Numeric size for gene labels (default: 2)
#' @param arrow.rate Numeric. Fraction of the panel x-range used as the
#'   target spacing between consecutive strand arrowheads on a gene body.
#'   Default 0.015 yields roughly one arrowhead per ~1.5% of panel width.
#'   Setting this to 0 disables strand arrowheads.
#' @param length Unit object specifying strand arrowhead size (default: unit(0.1, "cm"))
#' @param background.layers List of additional ggplot2 layers to add as background
#' @param chars_per_panel (native engine only) Approximate number of
#'   characters that fit across the panel at the active font size. Lower
#'   values give more horizontal padding per label (and hence more rows);
#'   raise it if your figure is wider than ~6in. Default: 100.
#' @param max_rows (native engine only) Optional integer cap on the number
#'   of gene rows. Genes that don't fit are silently dropped. NULL
#'   (default) means no cap.
#' @param gene_priority (native engine only) Optional character vector of
#'   gene symbols to pack first. Anything not listed competes for the
#'   remaining rows in genomic order. Useful with `max_rows` to guarantee
#'   that specific genes are kept.
#' @param exon.height (native engine only) Numeric vertical extent of exon
#'   rectangles in row units (default: 0.3, so exons span row±0.15).
#' @param label.color (native engine only) Character color for gene labels
#'   (default: "gray30").
#' @param label.offset (native engine only) Numeric vertical offset of the
#'   label above its gene body, in row units (default: 0.45).
#' @param engine Either `"native"` (default; collision-aware ggplot2
#'   implementation) or `"ggbio"` (falls back to `ggbio::geom_alignment` —
#'   useful for reproducing prior figures).
#'
#' @return A ggplot2 object showing the gene track panel
#'
#' @import ggplot2
#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges IRanges
#' @importFrom BuenColors jdb_palette
#' @importFrom stringr str_starts str_remove
#' @importFrom dplyr inner_join filter mutate select %>%
#' @importFrom grid unit
#'
#' @examples
#' \dontrun{
#' # Basic gene panel
#' plot_gene_panel("chr1", 1000000, 2000000)
#'
#' # Dense locus — naturally grows extra rows so labels never overlap
#' plot_gene_panel("chr19", 17500000, 19500000, genome_build = "hg38")
#'
#' # Keep specific genes visible when capping rows
#' plot_gene_panel(
#'   "chr19", 17500000, 19500000,
#'   genome_build = "hg38",
#'   max_rows = 3,
#'   gene_priority = c("JUND", "UBA52", "IFI30")
#' )
#'
#' # Fall back to the original ggbio rendering
#' plot_gene_panel(
#'   "chr19", 17500000, 19500000,
#'   genome_build = "hg38",
#'   engine = "ggbio"
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
                            fontsize = 7,
                            point.size = 2,
                            label.size = 2,
                            arrow.rate = 0.015,
                            length = unit(0.1, "cm"),
                            background.layers = NULL,
                            chars_per_panel = 100,
                            max_rows = NULL,
                            gene_priority = NULL,
                            exon.height = 0.3,
                            label.color = "gray30",
                            label.offset = 0.45,
                            engine = c("native", "ggbio")) {
  engine <- match.arg(engine)
  genome_build <- match.arg(genome_build)

  if (is.null(txdb)) {
    txdb <- load_txdb(genome_build)
  }
  if (!stringr::str_starts(chromosome, "chr")) {
    chromosome <- paste0("chr", chromosome)
  }
  if (!is.null(background.layers) && !is.list(background.layers)) {
    background.layers <- list(background.layers)
  }
  if (length(highlight_pos) == 0) {
    highlight_pos <- NULL
  }
  if (is.null(highlight_pos_y)) {
    highlight_pos_y <- 1
  }

  if (engine == "ggbio") {
    return(plot_gene_panel_ggbio(
      chromosome = chromosome,
      start = start, end = end,
      txdb = txdb,
      highlight_pos = highlight_pos,
      highlight_pos_y = highlight_pos_y,
      gene_col = gene_col,
      fontsize = fontsize,
      point.size = point.size,
      label.size = label.size,
      arrow.rate = arrow.rate,
      length = length,
      background.layers = background.layers
    ))
  }

  gr <- GenomicRanges::GRanges(
    seqnames = chromosome,
    ranges = IRanges::IRanges(start, end)
  )

  gm <- get_gene_models(txdb, gr)
  genes <- gm$genes
  features <- gm$features

  if (nrow(genes) > 0) {
    label_widths <- estimate_label_width_bp(
      genes$symbol,
      x_range = c(start, end),
      chars_per_panel = chars_per_panel
    )
    midpoints <- (genes$start + genes$end) / 2
    pack_start <- pmin(genes$start, midpoints - label_widths / 2)
    pack_end <- pmax(genes$end, midpoints + label_widths / 2)

    if (!is.null(gene_priority) && length(gene_priority) > 0L) {
      ord <- order(!(genes$symbol %in% gene_priority), midpoints)
    } else {
      ord <- order(midpoints)
    }
    rows <- integer(length(midpoints))
    rows[ord] <- pack_rows(pack_start[ord], pack_end[ord])
    genes$row <- rows

    if (!is.null(max_rows)) {
      keep <- genes$row <= max_rows
      genes <- genes[keep, , drop = FALSE]
      features <- features[features$gene_id %in% genes$gene_id, , drop = FALSE]
    }
  }

  exons <- features[features$type %in% c("cds", "utr", "exon"), , drop = FALSE]
  if (nrow(exons) > 0 && nrow(genes) > 0) {
    exons <- dplyr::inner_join(
      exons,
      dplyr::select(genes, gene_id, row),
      by = "gene_id"
    )
  }

  arrows <- strand_arrowheads(genes, x_range = c(start, end), arrow.rate = arrow.rate)

  p_gene <- ggplot() +
    background.layers +
    highlight_vline(highlight_pos) +
    or_missing(
      nrow(genes) > 0,
      geom_segment(
        data = genes,
        aes(x = start, xend = end, y = row, yend = row),
        color = gene_col,
        linewidth = 0.3
      )
    ) +
    or_missing(
      nrow(arrows) > 0,
      geom_segment(
        data = arrows,
        aes(x = x, xend = xend, y = y, yend = yend),
        arrow = arrow(length = length, ends = "last", type = "open"),
        color = gene_col,
        linewidth = 0.3
      )
    ) +
    or_missing(
      nrow(exons) > 0,
      geom_rect(
        data = exons,
        aes(
          xmin = start, xmax = end,
          ymin = row - exon.height / 2,
          ymax = row + exon.height / 2
        ),
        fill = gene_col,
        color = NA
      )
    ) +
    or_missing(
      nrow(genes) > 0,
      geom_text(
        data = genes,
        aes(
          x = (start + end) / 2,
          y = row + label.offset,
          label = symbol
        ),
        size = label.size,
        color = label.color,
        hjust = 0.5,
        vjust = 0
      )
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
    get_default_theme(
      fontsize = fontsize,
      hide.xtext = TRUE,
      hide.ytext = TRUE,
      hide.ytitle = TRUE
    ) +
    theme(
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      plot.margin = margin(5.5, 5.5, 5.5, 5.5, unit = "pt")
    ) +
    labs(x = sprintf("Chromosome %s", stringr::str_remove(chromosome, "^chr"))) +
    scale_y_continuous(expand = expansion(mult = c(0.15, 0.15))) +
    coord_cartesian(xlim = c(start, end), clip = "off")

  return(p_gene)
}

#' Compute repeated strand-arrowhead segments along each gene body
#'
#' For each row of `genes` with a non-`*` strand, generates short segments
#' positioned at roughly `arrow.rate * panel_width` intervals along the
#' gene body. Each segment carries an arrowhead pointing in the strand
#' direction. Returns a zero-row data frame when `arrow.rate == 0` or no
#' arrowheads fit.
#'
#' @param genes Data frame with columns `start`, `end`, `strand`, `row`.
#' @param x_range Numeric length-2 vector of panel x limits in bp.
#' @param arrow.rate Numeric fraction of panel width per arrowhead.
#'
#' @return Data frame with columns `x, xend, y, yend` (possibly empty).
#'
#' @keywords internal
#' @noRd
strand_arrowheads <- function(genes, x_range, arrow.rate = 0.015) {
  empty <- data.frame(
    x = numeric(), xend = numeric(),
    y = numeric(), yend = numeric()
  )
  if (is.null(genes) || nrow(genes) == 0L || arrow.rate <= 0) {
    return(empty)
  }

  span <- diff(x_range)
  spacing <- span * arrow.rate
  if (!is.finite(spacing) || spacing <= 0) {
    return(empty)
  }
  tip_len <- spacing * 0.05

  rows <- lapply(seq_len(nrow(genes)), function(i) {
    g <- genes[i, , drop = FALSE]
    if (!g$strand %in% c("+", "-")) {
      return(NULL)
    }
    g_start <- max(g$start, x_range[1])
    g_end <- min(g$end, x_range[2])
    g_len <- g_end - g_start
    if (g_len <= spacing) {
      pos <- (g_start + g_end) / 2
    } else {
      pos <- seq(g_start + spacing / 2, g_end - spacing / 2, by = spacing)
    }
    if (length(pos) == 0L) {
      return(NULL)
    }
    if (g$strand == "-") {
      data.frame(x = pos, xend = pos - tip_len, y = g$row, yend = g$row)
    } else {
      data.frame(x = pos, xend = pos + tip_len, y = g$row, yend = g$row)
    }
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L) {
    return(empty)
  }
  do.call(rbind, rows)
}

#' Internal: original ggbio-based gene panel implementation
#'
#' Retained as `engine = "ggbio"` fallback for `plot_gene_panel()`.
#'
#' @keywords internal
#' @noRd
plot_gene_panel_ggbio <- function(chromosome, start, end, txdb,
                                  highlight_pos, highlight_pos_y,
                                  gene_col, fontsize,
                                  point.size, label.size,
                                  arrow.rate, length,
                                  background.layers) {
  gr <- GenomicRanges::GRanges(
    seqnames = chromosome,
    ranges = IRanges::IRanges(start, end)
  )

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
    get_default_theme(
      fontsize = fontsize,
      hide.xtext = TRUE,
      hide.ytext = TRUE,
      hide.ytitle = TRUE
    ) +
    theme(
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      plot.margin = margin(5.5, 5.5, 5.5, 5.5, unit = "pt")
    ) +
    labs(x = sprintf("Chromosome %s", stringr::str_remove(chromosome, "^chr"))) +
    scale_y_continuous(expand = expansion()) +
    coord_cartesian(xlim = c(start, end), clip = "off")

  return(p_gene)
}
