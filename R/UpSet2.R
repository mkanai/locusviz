#' Modified UpSet plot
#'
#' @param m
#' @param set_on_rows
#' @param comb_col
#' @param pt_size
#' @param lwd
#' @param bg_col
#' @param bg_pt_col
#' @param set_order
#' @param comb_order
#' @param top_annotation
#' @param right_annotation
#' @param row_names_side
#' @param remove_lines
#' @param ...
#'
#' @import grid
#' @importFrom ComplexHeatmap UpSet
#' @export
UpSet2 = function(m,
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
  m2 = m

  class(m2) = "matrix"

  pt_size = pt_size
  lwd = lwd

  if(set_on_rows) {
    n_comb = ncol(m)
    if(length(comb_col == 1)) comb_col = rep(comb_col, n_comb)

    layer_fun = function(j, i, x, y, w, h, fill) {
      nr = round(1/as.numeric(h[1]))
      nc = round(1/as.numeric(w[1]))
      subm = matrix(pindex(m2, i, j), nrow = nr, byrow = FALSE)
      for(k in seq_len(nr)) {
        if(k %% 2) {
          grid.rect(y = k/nr, height = 1/nr, just = "top", gp = gpar(fill = bg_col[1], col = NA))
        } else {
          if(length(bg_col) > 1) {
            grid.rect(y = k/nr, height = 1/nr, just = "top", gp = gpar(fill = bg_col[2], col = NA))
          }
        }
      }
      pidx = as.logical(pindex(m2, i, j))
      grid.points(x[!pidx], y[!pidx], size = pt_size, pch = 16, gp = gpar(col = bg_pt_col))
      if (!remove_lines) {
        jj = unique(j)
        for(k in seq_len(nc)) {
          if(sum(subm[, k]) >= 2) {
            i_min = min(which(subm[, k] > 0))
            i_max = max(which(subm[, k] > 0))
            grid.lines(c(k - 0.5, k - 0.5)/nc, (nr - c(i_min, i_max) + 0.5)/nr, gp = gpar(col = "black", lwd = lwd))
          }
        }
      }
      grid.points(x[pidx], y[pidx], size = pt_size, pch = 16, gp = gpar(col = comb_col[i[pidx]]))
    }

    ht = ComplexHeatmap::Heatmap(m2, cluster_rows = FALSE, cluster_columns = FALSE, rect_gp = gpar(type = "none"),
                                 layer_fun = layer_fun, show_heatmap_legend = FALSE,
                                 top_annotation = top_annotation,
                                 right_annotation = right_annotation,
                                 row_names_side = row_names_side,
                                 row_order = set_order, column_order = comb_order, ...)
  } else {
    n_comb = nrow(m)
    if(length(comb_col == 1)) comb_col = rep(comb_col, n_comb)

    layer_fun = function(j, i, x, y, w, h, fill) {
      nr = round(1/as.numeric(h[1]))
      nc = round(1/as.numeric(w[1]))
      subm = matrix(pindex(m2, i, j), nrow = nr, byrow = FALSE)
      for(k in seq_len(nc)) {
        if(k %% 2) {
          grid.rect(x = k/nc, width = 1/nc, just = "right", gp = gpar(fill = "#F0F0F0", col = NA))
        }
      }
      pidx = as.logical(pindex(m2, i, j))
      grid.points(x[!pidx], y[!pidx], size = pt_size, pch = 16, gp = gpar(col = bg_pt_col))
      if (!remove_lines) {
        ii = unique(i)
        for(k in seq_len(nr)) {
          if(sum(subm[k, ]) >= 2) {
            i_min = min(which(subm[k, ] > 0))
            i_max = max(which(subm[k, ] > 0))
            grid.lines((c(i_min, i_max) - 0.5)/nc, (nr - c(k ,k) + 0.5)/nr, gp = gpar(col = "black", lwd = lwd))
          }
        }
      }
      grid.points(x[pidx], y[pidx], size = pt_size, pch = 16, gp = gpar(col = comb_col[j[pidx]]))
    }

    ht = ComplexHeatmap::Heatmap(m2, cluster_rows = FALSE, cluster_columns = FALSE, rect_gp = gpar(type = "none"),
                                 layer_fun = layer_fun, show_heatmap_legend = FALSE,
                                 top_annotation = top_annotation,
                                 right_annotation = right_annotation,
                                 row_order = comb_order, column_order = set_order, ...)
  }
  ht
}
