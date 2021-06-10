#' Title
#'
#' @param fontsize
#' @param hide.xlab
#' @param hide.ylab
#' @param hide.xtext
#' @param hide.ytext
#' @param hide.xtitle
#' @param hide.ytitle
#'
#' @return
#' @export
get_default_theme = function(fontsize = 8,
                             legend.position = c(1, 1),
                             legend.justification = c(1, 1),
                             hide.xlab = FALSE,
                             hide.ylab = FALSE,
                             hide.xtext = FALSE,
                             hide.ytext = FALSE,
                             hide.xtitle = FALSE,
                             hide.ytitle = FALSE) {
  ggtheme =
    BuenColors::pretty_plot(fontsize = fontsize) +
    BuenColors::L_border() +
    theme(
      plot.background = element_blank(),
      plot.margin = margin(0, 0.1, 0, 0.1, unit = "cm"),
      plot.tag = element_text(face = 'bold'),
      plot.title = element_text(hjust = 4e-3, margin = margin(b = -12), size = fontsize),
      legend.position = legend.position,
      legend.justification = legend.justification,
      legend.title = element_text(margin = margin(0, 0, 0, 0)),
      legend.background = element_blank(),
      legend.key.size = unit(0.2, "cm")
    )

  if (hide.xlab) {
    hide.xtext = hide.xtitle = TRUE
  }
  if (hide.ylab) {
    hide.ytext = hide.ytitle = TRUE
  }

  if (hide.xtext) {
    ggtheme = ggtheme + theme(axis.text.x = element_blank())
  }
  if (hide.ytext) {
    ggtheme = ggtheme + theme(axis.text.y = element_blank())
  }
  if (hide.xtitle) {
    ggtheme = ggtheme + theme(axis.title.x = element_blank())
  }
  if (hide.ytitle) {
    ggtheme = ggtheme + theme(axis.title.y = element_blank())
  }

  return(ggtheme)
}
