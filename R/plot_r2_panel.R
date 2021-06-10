#' Title
#'
#' @param data
#' @param highlight_pos
#' @param xlim
#' @param ylim
#' @param ybreaks
#' @param nlog10p_threshold
#' @param ggtheme
#' @param rasterize
#' @param rasterize.dpi
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom ggrastr rasterize
#'
#' @return
#' @export
#'
plot_r2_panel = function(data,
                         highlight_pos = NULL,
                         xlim = NULL,
                         ylim = c(0, 1),
                         ybreaks = seq(0, 1, by = 0.2),
                         nlog10p_threshold = 1,
                         ggtheme = get_default_theme(),
                         rasterize = FALSE,
                         rasterize.dpi = 300) {

  R = dplyr::select(data, variant, starts_with("gnomad_lead_r2")) %>%
    tidyr::pivot_longer(starts_with("gnomad_lead_r2"), names_to = "pop", values_to = "r2") %>%
    tidyr::drop_na() %>%
    dplyr::mutate(pop = stringr::str_to_upper(stringr::str_remove(pop, "^gnomad_lead_r2_")))

  data = dplyr::select(data, variant, position, nlog10p, lead_variant, cs_id) %>%
    dplyr::left_join(R)

  scale_x = or_missing(!is.null(xlim), coord_cartesian(xlim = xlim))
  scale_y = scale_y_continuous(lim = ylim, breaks = ybreaks, expand = expansion(c(0, 0.1), 0))

  rasterize_f = ifelse(rasterize, function(p) {
    ggrastr::rasterize(p, dpi = rasterize.dpi)
  }, function(p) {
    p
  })

  p_r2 = ggplot() +
    highlight_vline(highlight_pos) +
    geom_point(
      data = dplyr::filter(data, nlog10p > nlog10p_threshold & !(lead_variant | position %in% highlight_pos)),
      aes(x = position, y = r2, color = pop, alpha = !is.na(cs_id))
    ) +
    geom_point(
      data = dplyr::filter(data, lead_variant | position %in% highlight_pos),
      aes(x = position, y = r2, color = pop), shape = 18, size = 3
    ) +
    labs(x = "Position",
         y = expression(italic(r) ^ 2),
         color = "gnomAD") +
    scale_x +
    scale_y +
    scale_color_manual(values = get_gnomad_colors(), na.translate=FALSE) +
    scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.05), guide = FALSE, na.translate=FALSE) +
    ggtheme +
    guides(color = guide_legend(ncol=2))

  return(p_r2)
}
