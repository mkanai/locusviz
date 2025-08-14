#' Get default ggplot2 theme for locusviz plots
#'
#' This function returns a consistent ggplot2 theme used across all locusviz
#' plotting functions. It provides a clean, publication-ready appearance with
#' customizable options for hiding axis elements.
#'
#' @param fontsize Numeric font size for all text elements (default: 8)
#' @param legend.position Numeric vector for legend position (default: c(1,1) = top-right)
#' @param legend.justification Numeric vector for legend justification (default: c(1,1))
#' @param hide.xlab Logical whether to hide both x-axis text and title (default: FALSE)
#' @param hide.ylab Logical whether to hide both y-axis text and title (default: FALSE)
#' @param hide.xtext Logical whether to hide x-axis text (default: FALSE)
#' @param hide.ytext Logical whether to hide y-axis text (default: FALSE)
#' @param hide.xtitle Logical whether to hide x-axis title (default: FALSE)
#' @param hide.ytitle Logical whether to hide y-axis title (default: FALSE)
#'
#' @return A ggplot2 theme object
#'
#' @import ggplot2
#' @importFrom BuenColors pretty_plot L_border
#'
#' @examples
#' # Get default theme
#' theme_default <- get_default_theme()
#'
#' # Hide x-axis elements for stacked plots
#' theme_no_x <- get_default_theme(hide.xlab = TRUE)
#'
#' # Larger font size
#' theme_large <- get_default_theme(fontsize = 12)
#'
#' @export
get_default_theme <- function(fontsize = 8,
                              legend.position = c(1, 1),
                              legend.justification = c(1, 1),
                              hide.xlab = FALSE,
                              hide.ylab = FALSE,
                              hide.xtext = FALSE,
                              hide.ytext = FALSE,
                              hide.xtitle = FALSE,
                              hide.ytitle = FALSE,
                              angle.xtext = NULL) {
  ggtheme <-
    BuenColors::pretty_plot(fontsize = fontsize) +
    BuenColors::L_border() +
    theme(
      plot.background = element_blank(),
      plot.margin = margin(0, 0.1, 0, 0.1, unit = "cm"),
      plot.tag = element_text(face = "bold"),
      plot.title = element_text(hjust = 4e-3, margin = margin(b = -12), size = fontsize),
      legend.position = legend.position,
      legend.justification = legend.justification,
      legend.title = element_text(margin = margin(0, 0, 0, 0)),
      legend.background = element_blank(),
      legend.key.size = unit(0.2, "cm")
    )

  if (hide.xlab) {
    hide.xtext <- hide.xtitle <- TRUE
  }
  if (hide.ylab) {
    hide.ytext <- hide.ytitle <- TRUE
  }

  if (hide.xtext) {
    ggtheme <- ggtheme + theme(axis.text.x = element_blank())
  }
  if (hide.ytext) {
    ggtheme <- ggtheme + theme(axis.text.y = element_blank())
  }
  if (hide.xtitle) {
    ggtheme <- ggtheme + theme(axis.title.x = element_blank())
  }
  if (hide.ytitle) {
    ggtheme <- ggtheme + theme(axis.title.y = element_blank())
  }
  if (!hide.xtext & !is.null(angle.xtext)) {
    ggtheme <- ggtheme + theme(axis.text.x = element_text(angle = angle.xtext, hjust = 1))
  }

  return(ggtheme)
}
