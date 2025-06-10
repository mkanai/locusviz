#' Create r² (linkage disequilibrium) panel
#'
#' This function creates a panel showing linkage disequilibrium (r²) values between
#' variants and a lead variant, stratified by population from gnomAD data.
#'
#' @param data Data frame containing variant data with columns: variant, position,
#'   nlog10p, lead_variant, cs_id, and gnomad_lead_r2_* or gnomad_lead_r_* columns
#' @param highlight_pos Numeric vector of positions to highlight with larger diamonds
#' @param xlim Numeric vector of length 2 specifying x-axis limits (start, end)
#' @param ylim Numeric vector of length 2 specifying y-axis limits (default: c(0,1))
#' @param ybreaks Numeric vector specifying y-axis break points
#' @param point.size Numeric size for regular variant points (default: 1.5)
#' @param point.size2 Numeric size for highlighted variant points (default: 3)
#' @param legend.ncol Number of columns for the legend (default: 2)
#' @param nlog10p_threshold Numeric minimum -log10(p) value to display (default: 1)
#' @param ggtheme ggplot2 theme object (default: get_default_theme())
#' @param background.layers List of additional ggplot2 layers to add as background
#' @param rasterize Logical whether to rasterize the scatter plot (default: FALSE)
#' @param rasterize.dpi Numeric DPI for rasterization (default: 300)
#'
#' @return A ggplot2 object showing the r² panel
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom ggrastr rasterize
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom stringr str_detect str_to_upper str_remove
#'
#' @examples
#' \dontrun{
#' # Basic r² plot
#' plot_r2_panel(gwas_data)
#'
#' # With custom settings
#' plot_r2_panel(
#'   gwas_data,
#'   highlight_pos = c(123456, 789012),
#'   xlim = c(1000000, 2000000),
#'   legend.ncol = 3
#' )
#' }
#'
#' @export
#'
plot_r2_panel <- function(data,
                          highlight_pos = NULL,
                          xlim = NULL,
                          ylim = c(0, 1),
                          ybreaks = seq(0, 1, by = 0.2),
                          point.size = 1.5,
                          point.size2 = 3,
                          legend.ncol = 2,
                          nlog10p_threshold = 1,
                          ggtheme = get_default_theme(),
                          background.layers = NULL,
                          rasterize = FALSE,
                          rasterize.dpi = 300) {
  if (any(stringr::str_detect(colnames(data), "^gnomad_lead_r2_"))) {
    input.r <- FALSE
    cols <- "gnomad_lead_r2_"
  } else if (any(stringr::str_detect(colnames(data), "^gnomad_lead_r_"))) {
    input.r <- TRUE
    cols <- "gnomad_lead_r_"
  } else {
    stop("No gnomad_lead_{r,r2}_ columns found.")
  }

  R <- dplyr::select(data, variant, starts_with(cols)) %>%
    tidyr::pivot_longer(starts_with(cols),
      names_to = "pop",
      values_to = "r2"
    ) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(pop = stringr::str_to_upper(stringr::str_remove(pop, cols)))

  if (input.r) {
    R$r2 <- R$r2**2
  }

  data <- dplyr::select(data, variant, position, nlog10p, lead_variant, cs_id) %>%
    dplyr::left_join(R)

  scale_x <- or_missing(!is.null(xlim), coord_cartesian(xlim = xlim))
  scale_y <- scale_y_continuous(
    lim = ylim,
    breaks = ybreaks,
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

  p_r2 <- ggplot() +
    background.layers +
    highlight_vline(highlight_pos) +
    rasterize_f(geom_point(
      data = dplyr::filter(
        data,
        nlog10p > nlog10p_threshold &
          !(lead_variant | position %in% highlight_pos)
      ),
      aes(
        x = position,
        y = r2,
        color = pop,
        alpha = !is.na(cs_id)
      ),
      size = point.size
    )) +
    geom_point(
      data = dplyr::filter(data, lead_variant |
        position %in% highlight_pos),
      aes(x = position, y = r2, color = pop),
      shape = 18,
      size = point.size2
    ) +
    labs(
      x = "Position",
      y = expression(italic(r)^2),
      color = "gnomAD"
    ) +
    scale_x +
    scale_y +
    scale_color_manual(values = get_gnomad_colors(), na.translate = FALSE) +
    scale_alpha_manual(
      values = c("TRUE" = 1, "FALSE" = 0.05),
      guide = FALSE,
      na.translate = FALSE
    ) +
    ggtheme +
    guides(color = guide_legend(ncol = legend.ncol))

  return(p_r2)
}
