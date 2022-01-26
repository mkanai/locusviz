#' Title
#'
#' @param data
#' @param lead_variant
#' @param highlight_pos
#' @param window
#' @param xlim
#' @param ylim
#' @param nlog10p_threshold
#' @param loglog_p
#' @param plot.loglog_p
#' @param point.size
#' @param point.size2
#' @param manhattan.title
#' @param manhattan.breaks
#' @param r2_cols
#' @param lead_variant_col
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom ggrastr rasterize
#'
#' @return
#' @export
plot_manhattan_panel = function(data,
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
                                ggtheme = get_default_theme(),
                                background.layers = NULL,
                                rasterize = FALSE,
                                rasterize.dpi = 300) {
  if (plot.loglog_p) {
    trans_manhattan <- trans_loglog_p(loglog_p = loglog_p)
    if (ggplot2:::is.waive(ybreaks)) {
      y_breaks = c(0, 1, 10, 100, 1000, 10000)
    }
  } else {
    trans_manhattan <- scales::trans_new(
      "dummy_log_p",
      transform = function(x)
        x,
      inverse = function(x)
        x
    )
  }

  scale_x = or_missing(!is.null(xlim), coord_cartesian(xlim = xlim))
  scale_y = scale_y_continuous(
    breaks = ybreaks,
    trans = trans_manhattan,
    limits = ylim,
    expand = expansion(c(0, 0.1), 0)
  )

  if (!is.null(title)) {
    ggtheme = ggtheme + theme(plot.title = element_text(
      hjust = 4e-3,
      margin = margin(b = -12 * (stringr::str_count(title, "\n") + 1)),
      size = ggtheme$text$size
    ))
  }

  if (!is.null(background.layers) & !is.list(background.layers)) {
    background.layers = list(background.layers)
  }

  rasterize_f = ifelse(rasterize, function(p) {
    ggrastr::rasterize(p, dpi = rasterize.dpi)
  }, function(p) {
    p
  })

  p_manhattan = ggplot() +
    background.layers +
    geom_hline(
      yintercept = -log10(5e-8),
      linetype = "dashed",
      color = "grey50",
      size = line.size
    ) +
    highlight_vline(highlight_pos) +
    rasterize_f(# normal variants
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
      )) +
    # highlighted variants
    geom_point(
      data = dplyr::filter(data,!lead_variant &
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
      color = expression(italic(r) ^ 2)
    ) +
    scale_color_stepsn(
      colors = r2_cols,
      breaks = seq(0.2, 0.8, by = 0.2),
      limits = c(0, 1),
      show.limits = TRUE,
      na.value = 'grey50'
    ) +
    scale_x +
    scale_y +
    ggtheme

  return(p_manhattan)
}
