#' Create fine-mapping panel
#'
#' This function creates a panel showing fine-mapping posterior inclusion probabilities
#' (PIPs) for variants in a genomic region, with optional credible set coloring.
#'
#' @param data Data frame containing variant data with columns: position, pip,
#'   and optionally cs_id for credible set membership
#' @param highlight_pos Numeric vector of positions to highlight with larger diamonds
#' @param title Character string for plot title
#' @param legend_title Character string for legend title (default: "95\% CS")
#' @param xlim Numeric vector of length 2 specifying x-axis limits (start, end)
#' @param ylim Numeric vector of length 2 specifying y-axis limits (default: c(0,1))
#' @param ybreaks Numeric vector specifying y-axis break points
#' @param point.size Numeric size for regular variant points (default: 1.5)
#' @param point.size2 Numeric size for highlighted variant points (default: 3)
#' @param background.layers List of additional ggplot2 layers to add as background
#' @param rasterize Logical whether to rasterize the scatter plot (default: FALSE)
#' @param rasterize.dpi Numeric DPI for rasterization (default: 300)
#' @param cs.colors Character vector of colors for credible sets
#' @param relevel.cs_id Logical whether to relevel credible set IDs (default: TRUE)
#'
#' @return A ggplot2 object showing the fine-mapping panel
#'
#' @import ggplot2
#' @importFrom ggrastr rasterize
#' @importFrom dplyr filter distinct arrange pull mutate
#' @importFrom tidyr drop_na
#' @importFrom BuenColors jdb_palette
#'
#' @examples
#' \dontrun{
#' # Basic fine-mapping plot (default theme applied automatically)
#' plot_fm_panel(finemapping_data)
#'
#' # Override theme by adding it on top
#' plot_fm_panel(finemapping_data) + get_default_theme(fontsize = 7)
#'
#' # With custom settings
#' plot_fm_panel(
#'   finemapping_data,
#'   highlight_pos = c(123456, 789012),
#'   xlim = c(1000000, 2000000),
#'   title = "Fine-mapping results",
#'   cs.colors = c("red", "blue", "green")
#' )
#' }
#'
#' @export
plot_fm_panel <- function(data,
                          highlight_pos = NULL,
                          title = NULL,
                          legend_title = "95% CS",
                          xlim = NULL,
                          ylim = c(0, 1),
                          ybreaks = seq(0, 1, by = 0.2),
                          point.size = 1.5,
                          point.size2 = 3,
                          background.layers = NULL,
                          rasterize = FALSE,
                          rasterize.dpi = 300,
                          cs.colors = NULL,
                          relevel.cs_id = TRUE) {
  g_fm_title <- or_missing(!is.null(title), ggtitle(title))
  scale_x <- or_missing(!is.null(xlim), coord_cartesian(xlim = xlim))
  scale_y <- scale_y_continuous(
    limits = ylim,
    breaks = ybreaks,
    expand = expansion(c(0, 0.1), 0)
  )

  if (is.null(cs.colors)) {
    cs.colors <- BuenColors::jdb_palette("corona")
  }

  if (is.null(cs.colors) & relevel.cs_id) {
    visible_cs_ids <- dplyr::filter(data, xlim[1] <= position &
      position <= xlim[2]) %>%
      tidyr::drop_na(cs_id) %>%
      dplyr::distinct(cs_id) %>%
      dplyr::arrange(cs_id) %>%
      dplyr::pull(cs_id) %>%
      as.character()

    if (!is.null(highlight_pos)) {
      highlight_cs_ids <- dplyr::filter(data, position %in% highlight_pos) %>%
        tidyr::drop_na(cs_id) %>%
        dplyr::arrange(dplyr::desc(pip)) %>%
        dplyr::distinct(cs_id) %>%
        dplyr::pull(cs_id) %>%
        as.character()
      visible_cs_ids <- c(
        highlight_cs_ids,
        setdiff(visible_cs_ids, highlight_cs_ids)
      )
    }

    if (length(visible_cs_ids) > 0) {
      data <- dplyr::mutate(
        data,
        cs_id = forcats::fct_relevel(cs_id, visible_cs_ids),
        cs_id = forcats::lvls_revalue(cs_id, paste0("X", seq_along(
          levels(cs_id)
        )))
      )
    }
  }

  if (!is.null(background.layers) & !is.list(background.layers)) {
    background.layers <- list(background.layers)
  }

  rasterize_f <- ifelse(rasterize, function(p) {
    ggrastr::rasterize(p, dpi = rasterize.dpi)
  }, function(p) {
    p
  })

  p_fm <- ggplot() +
    background.layers +
    highlight_vline(highlight_pos) +
    or_missing(!all(is.na(data$pip)), list(
      # non-CS variants
      rasterize_f(geom_point(
        data = dplyr::filter(data, is.na(cs_id) &
          !position %in% highlight_pos),
        aes(x = position, y = pip, color = cs_id),
        size = point.size
      )),
      # non-highlighted variants in CS
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
    get_default_theme() +
    scale_x +
    scale_y +
    scale_color_manual(
      values = cs.colors,
      na.translate = TRUE,
      na.value = "grey50"
    )

  return(p_fm)
}
