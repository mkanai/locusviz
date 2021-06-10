#' Title
#'
#' @param data
#' @param lead_variant
#' @param highlight_pos
#' @param title
#' @param xlim
#' @param ylim
#' @param ybreaks
#' @param point.size
#' @param point.size2
#' @param ggtheme
#'
#' @import ggplot2
#' @importFrom ggrastr rasterize
#'
#' @return
#' @export
plot_fm_panel = function(data,
                         highlight_pos = NULL,
                         title = NULL,
                         legend_title = "95% CS",
                         xlim = NULL,
                         ylim = c(0, 1),
                         ybreaks = seq(0, 1, by = 0.2),
                         point.size = 1.5,
                         point.size2 = 3,
                         ggtheme = get_default_theme(),
                         rasterize = FALSE,
                         rasterize.dpi = 300,
                         relevel.cs_id = TRUE) {
  g_fm_title = or_missing(!is.null(title), ggtitle(title))
  scale_x = or_missing(!is.null(xlim), coord_cartesian(xlim = xlim))
  scale_y = scale_y_continuous(lim = ylim,
                               breaks = ybreaks,
                               expand = expansion(c(0, 0.1), 0))

  if (relevel.cs_id) {
    visible_cs_ids = dplyr::filter(data, xlim[1] <= position &
                                     position <= xlim[2]) %>%
      tidyr::drop_na(cs_id) %>%
      dplyr::distinct(cs_id) %>%
      dplyr::arrange(cs_id) %>%
      dplyr::pull(cs_id) %>%
      as.character()

    if (!is.null(highlight_pos)) {
      highlight_cs_ids = dplyr::filter(data, position %in% highlight_pos) %>%
        tidyr::drop_na(cs_id) %>%
        dplyr::arrange(dplyr::desc(pip)) %>%
        dplyr::distinct(cs_id) %>%
        dplyr::pull(cs_id) %>%
        as.character()
      visible_cs_ids = c(highlight_cs_ids,
                         setdiff(visible_cs_ids, highlight_cs_ids))
    }

    if (length(visible_cs_ids) > 0) {
      data = dplyr::mutate(
        data,
        cs_id = forcats::fct_relevel(cs_id, visible_cs_ids),
        cs_id = forcats::lvls_revalue(cs_id, paste0("X", seq_along(
          levels(cs_id)
        )))
      )
    }
  }

  rasterize_f = ifelse(rasterize, function(p) {
    ggrastr::rasterize(p, dpi = rasterize.dpi)
  }, function(p) {
    p
  })

  p_fm = ggplot() +
    highlight_vline(highlight_pos) +
    or_missing(!all(is.na(data$pip)), list(
      # non-CS variants
      rasterize_f(geom_point(
        data = dplyr::filter(data, is.na(cs_id) &
                               !position %in% highlight_pos),
        aes(x = position, y = pip, color = cs_id),
        size = point.size
      )),
      # highlighted variants not in CS
      geom_point(
        data = dplyr::filter(data, !is.na(cs_id) &
                               !position %in% highlight_pos),
        aes(x = position, y = pip, color = cs_id),
        size = point.size
      ),
      # highlighted variants in CS
      geom_point(
        data = dplyr::filter(data, position %in% highlight_pos),
        aes(x = position, y = pip, color = cs_id),
        shape = 18,
        size = point.size2
      )
    )) +
    labs(x = "Position", y = "PIP", color = legend_title) +
    g_fm_title +
    ggtheme +
    scale_x +
    scale_y +
    scale_color_manual(
      values = BuenColors::jdb_palette("corona"),
      na.translate = TRUE,
      na.value = "grey50"
    )

  return(p_fm)
}
