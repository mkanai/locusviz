#' Create Manhattan plot panel
#'
#' This function creates a Manhattan plot panel showing GWAS p-values for a genomic region,
#' with optional LD coloring relative to a lead variant.
#'
#' @param data Data frame containing variant data with columns: chromosome, position,
#'   nlog10p, lead_variant (logical), and optionally r2
#' @param highlight_pos Numeric vector of positions to highlight with larger diamonds
#' @param xlim Numeric vector of length 2 specifying x-axis limits (start, end)
#' @param ylim Numeric vector of length 2 specifying y-axis limits
#' @param ybreaks Numeric vector specifying y-axis break points
#' @param nlog10p_threshold Numeric minimum -log10(p) value to display (default: 1)
#' @param loglog_p Numeric threshold for log-log transformation (default: 10)
#' @param plot.loglog_p Logical whether to use log-log p-value transformation
#' @param point.size Numeric size for regular variant points (default: 1.5)
#' @param point.size2 Numeric size for highlighted/lead variant points (default: 3)
#' @param line.size Numeric size for genome-wide significance line (default: 0.5)
#' @param title Character string for plot title
#' @param r2_cols Character vector of colors for r² bins
#' @param lead_variant_col Character color for lead variant (default: "purple3")
#' @param background.layers List of additional ggplot2 layers to add as background
#' @param rasterize Logical whether to rasterize the scatter plot (default: FALSE)
#' @param rasterize.dpi Numeric DPI for rasterization (default: 300)
#'
#' @return A ggplot2 object showing the Manhattan plot panel
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom ggrastr rasterize
#' @importFrom scales trans_new
#' @importFrom stringr str_remove
#'
#' @examples
#' \dontrun{
#' # Basic Manhattan plot (default theme applied automatically)
#' plot_manhattan_panel(gwas_data)
#'
#' # Override theme by adding it on top
#' plot_manhattan_panel(gwas_data) + get_default_theme(fontsize = 7)
#'
#' # With custom settings
#' plot_manhattan_panel(
#'   gwas_data,
#'   highlight_pos = c(123456, 789012),
#'   xlim = c(1000000, 2000000),
#'   plot.loglog_p = TRUE,
#'   title = "GWAS results for trait X"
#' )
#' }
#'
#' @export
plot_manhattan_panel <- function(data,
                                 highlight_pos = NULL,
                                 xlim = NULL,
                                 ylim = NULL,
                                 ybreaks = ggplot2::waiver(),
                                 nlog10p_threshold = 1,
                                 loglog_p = 10,
                                 plot.loglog_p = FALSE,
                                 point.size = 1.5,
                                 point.size2 = 3,
                                 line.size = 0.5,
                                 title = NULL,
                                 r2_cols = c("navy", "lightskyblue", "green", "orange", "red"),
                                 lead_variant_col = "purple3",
                                 background.layers = NULL,
                                 rasterize = FALSE,
                                 rasterize.dpi = 300) {
  if (plot.loglog_p) {
    trans_manhattan <- trans_loglog_p(loglog_p = loglog_p)
    if (ggplot2:::is.waive(ybreaks)) {
      y_breaks <- c(0, 1, 10, 100, 1000, 10000)
    }
  } else {
    trans_manhattan <- scales::trans_new(
      "dummy_log_p",
      transform = function(x) {
        x
      },
      inverse = function(x) {
        x
      }
    )
  }

  scale_x <- or_missing(!is.null(xlim), coord_cartesian(xlim = xlim))
  scale_y <- scale_y_continuous(
    breaks = ybreaks,
    trans = trans_manhattan,
    limits = ylim,
    expand = expansion(c(0, 0.1), 0)
  )

  if (!is.null(background.layers) & !is.list(background.layers)) {
    background.layers <- list(background.layers)
  }

  rasterize_f <- ifelse(rasterize, function(p) {
    ggrastr::rasterize(p, dpi = rasterize.dpi)
  }, function(p) {
    p
  })

  p_manhattan <- ggplot() +
    background.layers +
    geom_hline(
      yintercept = -log10(5e-8),
      linetype = "dashed",
      color = "grey50",
      size = line.size
    ) +
    highlight_vline(highlight_pos) +
    rasterize_f( # normal variants
      geom_point(
        data =
          dplyr::filter(
            data,
            nlog10p > nlog10p_threshold &
              !(lead_variant | position %in% highlight_pos)
          ) %>%
            dplyr::arrange(desc(is.na(r2)), r2),
        aes(x = position, y = nlog10p, color = r2),
        size = point.size
      )
    ) +
    # highlighted variants
    geom_point(
      data = dplyr::filter(data, !lead_variant &
        position %in% highlight_pos),
      aes(x = position, y = nlog10p, color = r2),
      shape = 18,
      size = point.size2
    ) +
    # lead variant
    geom_point(
      data = dplyr::filter(data, lead_variant),
      aes(x = position, y = nlog10p),
      shape = 18,
      color = lead_variant_col,
      size = point.size2
    ) +
    labs(
      title = or_missing(!is.null(title), ggtitle(title)),
      x = sprintf("Chromosome %s", stringr::str_remove(data$chromosome[1], "^chr")),
      y = expression(paste(-log[10], "(", italic(P), ")")),
      color = expression(italic(r)^2)
    ) +
    scale_color_stepsn(
      colors = r2_cols,
      breaks = seq(0.2, 0.8, by = 0.2),
      limits = c(0, 1),
      show.limits = TRUE,
      na.value = "grey50"
    ) +
    scale_x +
    scale_y +
    get_default_theme()

  return(p_manhattan)
}
