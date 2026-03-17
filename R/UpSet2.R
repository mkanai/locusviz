#' Modified UpSet plot
#'
#' This function creates an UpSet plot with enhanced visual customization options.
#' UpSet plots are used to visualize intersections of multiple sets.
#'
#' @param m A matrix or data frame where rows/columns represent sets and values
#'   indicate set membership (1) or not (0)
#' @param set_on_rows Logical indicating if sets are on rows (default: TRUE) or columns
#' @param comb_col Character color or vector of colors for combination markers
#' @param pt_size Grid unit object specifying point size (default: unit(3, "mm"))
#' @param lwd Numeric line width for connections (default: 2)
#' @param bg_col Character color for background (default: "#F0F0F0")
#' @param bg_pt_col Character color for background points (default: "#CCCCCC")
#' @param set_order Character vector specifying order of sets
#' @param comb_order Character vector specifying order of combinations
#' @param top_annotation HeatmapAnnotation object for top annotation
#' @param right_annotation HeatmapAnnotation object for right annotation
#' @param row_names_side Character string: "left" or "right" for row name position
#' @param remove_lines Logical whether to remove connecting lines (default: FALSE)
#' @param ... Additional arguments passed to ComplexHeatmap::UpSet
#'
#' @return An UpSet plot object from ComplexHeatmap
#'
#' @import grid
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' m <- matrix(c(1, 0, 1, 1, 0, 0, 1, 1, 0), nrow = 3)
#' rownames(m) <- c("Set1", "Set2", "Set3")
#'
#' # Basic UpSet plot
#' UpSet2(m)
#'
#' # Customized UpSet plot
#' UpSet2(m, comb_col = "red", pt_size = unit(5, "mm"))
#' }
#'
#' @export
UpSet2 <- function(m,
                   set_on_rows = TRUE,
                   comb_col = "black",
                   pt_size = grid::unit(3, "mm"),
                   lwd = 2,
                   bg_col = "#F0F0F0",
                   bg_pt_col = "#CCCCCC",
                   set_order = NULL,
                   comb_order = NULL,
                   top_annotation = NULL,
                   right_annotation = NULL,
                   row_names_side = "left",
                   remove_lines = FALSE,
                   ...) {
  if (!requireNamespace("ComplexHeatmap", quietly = TRUE)) {
    stop("Package 'ComplexHeatmap' is required for UpSet2(). ",
      "Install it with: BiocManager::install('ComplexHeatmap')",
      call. = FALSE
    )
  }

  m2 <- m

  class(m2) <- "matrix"

  pt_size <- pt_size
  lwd <- lwd

  if (set_on_rows) {
    n_comb <- ncol(m)
    if (length(comb_col == 1)) comb_col <- rep(comb_col, n_comb)

    layer_fun <- function(j, i, x, y, w, h, fill) {
      nr <- round(1 / as.numeric(h[1]))
      nc <- round(1 / as.numeric(w[1]))
      subm <- matrix(pindex(m2, i, j), nrow = nr, byrow = FALSE)
      for (k in seq_len(nr)) {
        if (k %% 2) {
          grid.rect(y = k / nr, height = 1 / nr, just = "top", gp = gpar(fill = bg_col[1], col = NA))
        } else {
          if (length(bg_col) > 1) {
            grid.rect(y = k / nr, height = 1 / nr, just = "top", gp = gpar(fill = bg_col[2], col = NA))
          }
        }
      }
      pidx <- as.logical(pindex(m2, i, j))
      grid.points(x[!pidx], y[!pidx], size = pt_size, pch = 16, gp = gpar(col = bg_pt_col))
      if (!remove_lines) {
        jj <- unique(j)
        for (k in seq_len(nc)) {
          if (sum(subm[, k]) >= 2) {
            i_min <- min(which(subm[, k] > 0))
            i_max <- max(which(subm[, k] > 0))
            grid.lines(c(k - 0.5, k - 0.5) / nc, (nr - c(i_min, i_max) + 0.5) / nr, gp = gpar(col = "black", lwd = lwd))
          }
        }
      }
      grid.points(x[pidx], y[pidx], size = pt_size, pch = 16, gp = gpar(col = comb_col[i[pidx]]))
    }

    ht <- ComplexHeatmap::Heatmap(m2,
      cluster_rows = FALSE, cluster_columns = FALSE, rect_gp = gpar(type = "none"),
      layer_fun = layer_fun, show_heatmap_legend = FALSE,
      top_annotation = top_annotation,
      right_annotation = right_annotation,
      row_names_side = row_names_side,
      row_order = set_order, column_order = comb_order, ...
    )
  } else {
    n_comb <- nrow(m)
    if (length(comb_col == 1)) comb_col <- rep(comb_col, n_comb)

    layer_fun <- function(j, i, x, y, w, h, fill) {
      nr <- round(1 / as.numeric(h[1]))
      nc <- round(1 / as.numeric(w[1]))
      subm <- matrix(pindex(m2, i, j), nrow = nr, byrow = FALSE)
      for (k in seq_len(nc)) {
        if (k %% 2) {
          grid.rect(x = k / nc, width = 1 / nc, just = "right", gp = gpar(fill = "#F0F0F0", col = NA))
        }
      }
      pidx <- as.logical(pindex(m2, i, j))
      grid.points(x[!pidx], y[!pidx], size = pt_size, pch = 16, gp = gpar(col = bg_pt_col))
      if (!remove_lines) {
        ii <- unique(i)
        for (k in seq_len(nr)) {
          if (sum(subm[k, ]) >= 2) {
            i_min <- min(which(subm[k, ] > 0))
            i_max <- max(which(subm[k, ] > 0))
            grid.lines((c(i_min, i_max) - 0.5) / nc, (nr - c(k, k) + 0.5) / nr, gp = gpar(col = "black", lwd = lwd))
          }
        }
      }
      grid.points(x[pidx], y[pidx], size = pt_size, pch = 16, gp = gpar(col = comb_col[j[pidx]]))
    }

    ht <- ComplexHeatmap::Heatmap(m2,
      cluster_rows = FALSE, cluster_columns = FALSE, rect_gp = gpar(type = "none"),
      layer_fun = layer_fun, show_heatmap_legend = FALSE,
      top_annotation = top_annotation,
      right_annotation = right_annotation,
      row_order = comb_order, column_order = set_order, ...
    )
  }
  ht
}
