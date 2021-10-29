#' Title
#'
#' @param data
#' @param highlight_pos
#' @param window
#' @param xlim
#' @param manhattan.args
#' @param manhattan.title
#' @param manhattan.breaks
#' @param manhattan.loglog_p
#' @param nlog10p_threshold
#' @param fm.ylim
#' @param fm.breaks
#' @param fm.legend_title
#' @param gene.args
#' @param gene_score.args
#' @param plot.manhattan
#' @param plot.fm
#' @param plot.r2
#' @param plot.gene
#' @param plot.gene_score
#'
#' @import patchwork
#'
#' @return
#' @export
plot_locuszoom = function(data,
                          highlight_pos = NULL,
                          window = NULL,
                          xlim = NULL,
                          manhattan.args = list(),
                          manhattan.title = NULL,
                          manhattan.breaks = ggplot2::waiver(),
                          manhattan.loglog_p = TRUE,
                          nlog10p_threshold = 0,
                          fm.args = list(),
                          fm.ylim = NULL,
                          fm.breaks = NULL,
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
                          patchwork = TRUE,
                          rasterize = FALSE,
                          rasterize.dpi = 300) {
  if (is.null(xlim) & is.null(window)) {
    xlim = range(data$position)
  } else if (is.null(xlim) & !is.null(window)) {
    lead_pos = data$position[data$lead_variant]
    xlim = c(lead_pos - window, lead_pos + window)
  }

  stopifnot(length(unique(data$chromosome)) == 1)
  chromosome = data$chromosome[1]
  start = xlim[1]
  end = xlim[2]

  p_manhattan = or_missing(plot.manhattan,
                           do.call(plot_manhattan_panel, c(
                             list(
                               data,
                               highlight_pos = highlight_pos,
                               xlim = xlim,
                               title = manhattan.title,
                               ybreaks = manhattan.breaks,
                               plot.loglog_p = manhattan.loglog_p,
                               nlog10p_threshold = nlog10p_threshold,
                               ggtheme = get_default_theme(
                                 fontsize = fontsize,
                                 hide.xtext = (plot.fm | plot.r2),
                                 hide.xtitle = TRUE
                               ),
                               rasterize = rasterize,
                               rasterize.dpi = rasterize.dpi
                             ),
                             manhattan.args
                           )))
  p_fm = or_missing(plot.fm,
                    do.call(plot_fm_panel, c(
                      list(
                        data,
                        highlight_pos = highlight_pos,
                        xlim = xlim,
                        ylim = fm.ylim,
                        ybreaks = fm.breaks,
                        legend_title = fm.legend_title,
                        ggtheme = get_default_theme(
                          fontsize = fontsize,
                          hide.xtext = plot.r2,
                          hide.xtitle = TRUE
                        ),
                        rasterize = rasterize,
                        rasterize.dpi = rasterize.dpi
                      ),
                      fm.args
                    )))
  p_r2 = or_missing(plot.r2,
                    do.call(plot_r2_panel, c(
                      list(
                        data,
                        highlight_pos = highlight_pos,
                        xlim = xlim,
                        nlog10p_threshold = nlog10p_threshold,
                        ggtheme = get_default_theme(fontsize = fontsize, hide.xtitle = TRUE),
                        rasterize = rasterize,
                        rasterize.dpi = rasterize.dpi
                      ),
                      r2.args
                    )))
  p_gene = or_missing(plot.gene,
                      do.call(plot_gene_panel,
                              c(
                                list(
                                  chromosome,
                                  start,
                                  end,
                                  highlight_pos = highlight_pos,
                                  fontsize = fontsize
                                ),
                                gene.args
                              )))

  p_gene_score = or_missing(plot.gene_score,
                            do.call(plot_gene_score_panel, c(
                              list(
                                chromosome,
                                start,
                                end,
                                highlight_pos = highlight_pos,
                                fontsize = fontsize
                              ),
                              gene_score.args
                            )))

  n_plots = plot.manhattan + plot.fm + plot.r2 + plot.gene + plot.gene_score
  heights = c(
    or_missing(plot.manhattan, 1),
    or_missing(plot.fm, 0.25),
    or_missing(plot.r2, 0.3),
    or_missing(plot.gene, 0.1),
    or_missing(plot.gene_score, 0.3)
  )

  panels = list(p_manhattan,
                p_fm,
                p_r2,
                p_gene,
                p_gene_score)
  if (patchwork) {
    plt = purrr::reduce(panels, `+`) + patchwork::plot_layout(ncol = 1, heights = heights)
  } else {
    plt = panels
  }


  return(plt)
}
