#' Create LocusZoom-style visualization
#'
#' This is the main function for creating LocusZoom-style plots. It combines multiple
#' panels (Manhattan plot, fine-mapping, r2/LD, gene track, and gene scores) into a
#' single comprehensive visualization of a genomic locus.
#'
#' @param data Data frame containing variant information with required columns:
#'   chromosome, position, and additional columns depending on enabled panels
#' @param highlight_pos Numeric position to highlight across all panels
#' @param window Numeric window size around lead variant (ignored if xlim is provided)
#' @param xlim Numeric vector of length 2 specifying the x-axis limits (start, end)
#' @param manhattan.args List of additional arguments passed to plot_manhattan_panel
#' @param manhattan.title Character string for Manhattan panel title
#' @param manhattan.breaks Y-axis breaks for Manhattan panel (default: automatic)
#' @param manhattan.loglog_p Logical whether to use log-log p-value transformation
#' @param nlog10p_threshold Numeric threshold for -log10(p) values
#' @param fm.args List of additional arguments passed to plot_fm_panel
#' @param fm.ylim Numeric vector for fine-mapping panel y-axis limits (default: c(0,1))
#' @param fm.breaks Numeric vector for fine-mapping panel y-axis breaks
#' @param fm.legend_title Character string for fine-mapping legend title
#' @param r2.args List of additional arguments passed to plot_r2_panel
#' @param gene.args List of additional arguments passed to plot_gene_panel
#' @param gene_score.args List of additional arguments passed to plot_gene_score_panel
#' @param plot.manhattan Logical whether to include Manhattan panel (default: TRUE)
#' @param plot.fm Logical whether to include fine-mapping panel (default: TRUE)
#' @param plot.r2 Logical whether to include r2/LD panel (default: FALSE)
#' @param plot.gene Logical whether to include gene track panel (default: TRUE)
#' @param plot.gene_score Logical whether to include gene score panel (default: FALSE)
#' @param fontsize Numeric font size for all panels (default: 8)
#' @param ggtheme Optional ggplot2 theme applied on top of every panel's
#'   default theme. Use to override styling uniformly across all panels
#'   (e.g. `theme(legend.position = "bottom")`). Default: NULL (no override).
#' @param patchwork Logical whether to combine panels using patchwork (default: TRUE)
#' @param rasterize Logical whether to rasterize scatter plots (default: FALSE)
#' @param rasterize.dpi Numeric DPI for rasterization (default: 300)
#'
#' @return Either a combined patchwork plot (if patchwork=TRUE) or a list of
#'   individual ggplot2 objects for each panel
#'
#' @import patchwork
#' @importFrom purrr reduce
#'
#' @examples
#' \dontrun{
#' # Basic LocusZoom plot
#' plot_locuszoom(gwas_data, highlight_pos = 123456789)
#'
#' # Custom configuration with specific panels
#' plot_locuszoom(
#'   gwas_data,
#'   window = 500000,
#'   plot.r2 = TRUE,
#'   plot.gene_score = TRUE,
#'   manhattan.title = "GWAS results for trait X"
#' )
#' }
#'
#' @export
plot_locuszoom <- function(data,
                           highlight_pos = NULL,
                           window = NULL,
                           xlim = NULL,
                           manhattan.args = list(),
                           manhattan.title = NULL,
                           manhattan.breaks = ggplot2::waiver(),
                           manhattan.loglog_p = TRUE,
                           nlog10p_threshold = 0,
                           fm.args = list(),
                           fm.ylim = c(0, 1),
                           fm.breaks = seq(0, 1, by = 0.2),
                           fm.legend_title = "95% CS",
                           r2.args = list(),
                           gene.args = list(),
                           gene_score.args = list(),
                           plot.manhattan = TRUE,
                           plot.fm = TRUE,
                           plot.r2 = FALSE,
                           plot.gene = TRUE,
                           plot.gene_score = FALSE,
                           fontsize = 8,
                           ggtheme = NULL,
                           patchwork = TRUE,
                           rasterize = FALSE,
                           rasterize.dpi = 300) {
  if (is.null(xlim) & is.null(window)) {
    xlim <- range(data$position)
  } else if (is.null(xlim) & !is.null(window)) {
    lead_pos <- data$position[data$lead_variant]
    xlim <- c(lead_pos - window, lead_pos + window)
  }

  stopifnot(length(unique(data$chromosome)) == 1)
  chromosome <- data$chromosome[1]
  start <- xlim[1]
  end <- xlim[2]

  p_manhattan <- or_missing(
    plot.manhattan,
    do.call(plot_manhattan_panel, c(
      list(
        data,
        highlight_pos = highlight_pos,
        xlim = xlim,
        title = manhattan.title,
        ybreaks = manhattan.breaks,
        plot.loglog_p = manhattan.loglog_p,
        nlog10p_threshold = nlog10p_threshold,
        rasterize = rasterize,
        rasterize.dpi = rasterize.dpi
      ),
      manhattan.args
    )) +
      get_default_theme(
        fontsize = fontsize,
        hide.xtext = (plot.fm | plot.r2),
        hide.xtitle = TRUE
      ) +
      ggtheme
  )
  p_fm <- or_missing(
    plot.fm,
    do.call(plot_fm_panel, c(
      list(
        data,
        highlight_pos = highlight_pos,
        xlim = xlim,
        ylim = fm.ylim,
        ybreaks = fm.breaks,
        legend_title = fm.legend_title,
        rasterize = rasterize,
        rasterize.dpi = rasterize.dpi
      ),
      fm.args
    )) +
      get_default_theme(
        fontsize = fontsize,
        hide.xtext = plot.r2,
        hide.xtitle = TRUE
      ) +
      ggtheme
  )
  p_r2 <- or_missing(
    plot.r2,
    do.call(plot_r2_panel, c(
      list(
        data,
        highlight_pos = highlight_pos,
        xlim = xlim,
        nlog10p_threshold = nlog10p_threshold,
        rasterize = rasterize,
        rasterize.dpi = rasterize.dpi
      ),
      r2.args
    )) +
      get_default_theme(fontsize = fontsize, hide.xtitle = TRUE) +
      ggtheme
  )
  p_gene <- or_missing(
    plot.gene,
    do.call(
      plot_gene_panel,
      c(
        list(
          chromosome,
          start,
          end,
          highlight_pos = highlight_pos,
          fontsize = fontsize
        ),
        gene.args
      )
    )
  )

  p_gene_score <- or_missing(
    plot.gene_score,
    do.call(plot_gene_score_panel, c(
      list(
        chromosome,
        start,
        end,
        highlight_pos = highlight_pos,
        fontsize = fontsize
      ),
      gene_score.args
    ))
  )

  n_plots <- plot.manhattan + plot.fm + plot.r2 + plot.gene + plot.gene_score
  heights <- c(
    or_missing(plot.manhattan, 1),
    or_missing(plot.fm, 0.25),
    or_missing(plot.r2, 0.3),
    or_missing(plot.gene, 0.1),
    or_missing(plot.gene_score, 0.3)
  )

  panels <- list(
    p_manhattan,
    p_fm,
    p_r2,
    p_gene,
    p_gene_score
  )
  if (patchwork) {
    plt <- purrr::reduce(panels, `+`) + patchwork::plot_layout(ncol = 1, heights = heights)
  } else {
    plt <- panels
  }


  return(plt)
}
