#' Text labels jittered to avoid horizontal overlap
#'
#' A ggplot2 layer that draws text labels and spreads them horizontally at
#' render time so they do not overlap, optionally drawing a leader line from
#' each label back to its anchor point. Unlike [jitter_labels()], the label
#' footprint is measured against the real panel when the plot is drawn, so the
#' spacing is correct regardless of the output device size. This is a 1-D
#' (horizontal) analogue of `ggrepel::geom_text_repel`.
#'
#' @inheritParams ggplot2::geom_text
#' @param connector Logical; draw a leader line from each label to its anchor
#'   (default `TRUE`).
#' @param connector.colour Colour of the leader line (default `"grey50"`).
#' @param connector.linetype Line type of the leader line (default `"dotted"`).
#' @param connector.size Line width of the leader line (default `0.2`).
#' @param connector.straight Fraction of the connector's vertical span, measured
#'   from the label end, drawn as a straight vertical segment; the remainder
#'   (toward the anchor) is drawn diagonally. This reproduces trackViewer's
#'   leader style, where the diagonal absorbs the horizontal jitter and a short
#'   vertical segment enters the label (default `0.33`). Set to `0` for a single
#'   straight diagonal from anchor to label.
#' @param weight Spacing weight passed to the jitter pass (default `1.2`).
#' @section Aesthetics:
#' `geom_jitter_text()` understands `x`, `y`, `label` (required), plus `yend`
#' (the connector's anchor `y`; defaults to `y`), `colour`, `size`, `angle`,
#' `hjust`, `vjust`, `alpha`, `family`, `fontface`, and `lineheight`.
#' @return A ggplot2 layer.
#' @examples
#' \dontrun{
#' ggplot(df, aes(x = pos, y = 1, label = name)) +
#'   geom_jitter_text(angle = 90)
#' }
#' @import ggplot2
#' @importFrom grid makeContent gTree gList setChildren textGrob segmentsGrob
#'   grobWidth convertWidth unit gpar
#' @export
geom_jitter_text <- function(mapping = NULL, data = NULL, stat = "identity",
                             position = "identity", ...,
                             connector = TRUE,
                             connector.colour = "grey50",
                             connector.linetype = "dotted",
                             connector.size = 0.2,
                             connector.straight = 0.33,
                             weight = 1.2,
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE) {
  layer(
    geom = GeomJitterText, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      connector = connector,
      connector.colour = connector.colour,
      connector.linetype = connector.linetype,
      connector.size = connector.size,
      connector.straight = connector.straight,
      weight = weight,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_jitter_text
#' @format NULL
#' @usage NULL
#' @export
GeomJitterText <- ggproto("GeomJitterText", Geom,
  required_aes = c("x", "y", "label"),
  default_aes = aes(
    yend = NA, colour = "black", size = 3.88, angle = 0,
    hjust = 0.5, vjust = 0.5, alpha = NA, family = "",
    fontface = 1, lineheight = 1.2
  ),
  draw_key = ggplot2::draw_key_text,
  draw_panel = function(data, panel_params, coord,
                        connector = TRUE,
                        connector.colour = "grey50",
                        connector.linetype = "dotted",
                        connector.size = 0.2,
                        connector.straight = 0.33,
                        weight = 1.2,
                        na.rm = FALSE) {
    data <- coord$transform(data, panel_params)
    if (is.null(data$yend) || all(is.na(data$yend))) {
      data$yend <- data$y
    } else {
      data$yend[is.na(data$yend)] <- data$y[is.na(data$yend)]
    }
    grid::gTree(
      jitter_data = data,
      connector = connector,
      connector.colour = connector.colour,
      connector.linetype = connector.linetype,
      connector.size = connector.size,
      connector.straight = connector.straight,
      weight = weight,
      cl = "jitter_text_grob"
    )
  }
)

#' @importFrom grid makeContent
#' @export
makeContent.jitter_text_grob <- function(x) {
  data <- x$jitter_data
  n <- nrow(data)
  if (n == 0) {
    return(grid::setChildren(x, grid::gList()))
  }

  # Measure each label's horizontal footprint against the real panel (npc).
  # grobWidth of a rotated textGrob is its horizontal bounding box, so this
  # works for any angle (string width at 0 deg, cap height near 90 deg).
  label_w <- vapply(seq_len(n), function(i) {
    tg <- grid::textGrob(
      label = data$label[i],
      rot = data$angle[i],
      gp = grid::gpar(
        fontsize = data$size[i] * ggplot2::.pt,
        fontfamily = data$family[i],
        fontface = data$fontface[i],
        lineheight = data$lineheight[i]
      )
    )
    grid::convertWidth(grid::grobWidth(tg), "npc", valueOnly = TRUE)
  }, numeric(1))
  line_w <- max(label_w)

  # Jitter the x positions in npc space (Layer A), preserving input order.
  if (n >= 2 && is.finite(line_w) && line_w > 0) {
    ord <- order(data$x)
    xj_sorted <- .jitter_positions_npc(data$x[ord], line_w, x$weight)
    xj_sorted <- .readjust_positions_npc(xj_sorted, line_w)
    xj <- numeric(n)
    xj[ord] <- xj_sorted
  } else {
    xj <- data$x
  }

  txt <- grid::textGrob(
    label = data$label,
    x = grid::unit(xj, "npc"),
    y = grid::unit(data$y, "npc"),
    rot = data$angle,
    hjust = data$hjust,
    vjust = data$vjust,
    gp = grid::gpar(
      col = scales::alpha(data$colour, data$alpha),
      fontsize = data$size * ggplot2::.pt,
      fontfamily = data$family,
      fontface = data$fontface,
      lineheight = data$lineheight
    )
  )

  if (isTRUE(x$connector)) {
    # trackViewer-style leader line: a diagonal from the anchor up to the
    # label's (jittered) x, absorbing the horizontal shift, then a short
    # straight vertical segment at the label's x entering the label. With no
    # jitter both segments are vertical, giving a single straight line.
    gp <- grid::gpar(
      col = x$connector.colour,
      lty = x$connector.linetype,
      lwd = x$connector.size * ggplot2::.pt
    )
    y_break <- data$y - x$connector.straight * (data$y - data$yend)
    seg_diag <- grid::segmentsGrob(
      x0 = grid::unit(data$x, "npc"),
      y0 = grid::unit(data$yend, "npc"),
      x1 = grid::unit(xj, "npc"),
      y1 = grid::unit(y_break, "npc"),
      gp = gp
    )
    seg_straight <- grid::segmentsGrob(
      x0 = grid::unit(xj, "npc"),
      y0 = grid::unit(y_break, "npc"),
      x1 = grid::unit(xj, "npc"),
      y1 = grid::unit(data$y, "npc"),
      gp = gp
    )
    children <- grid::gList(seg_diag, seg_straight, txt)
  } else {
    children <- grid::gList(txt)
  }
  grid::setChildren(x, children)
}
