# 1-D label jitter in npc space.
#
# Pure-R ports of trackViewer's internal jitterLables()/reAdjustLabels(),
# operating entirely in normalized parent coordinates ([0, 1]) with no grid
# dependency. Both take and return positions in [0, 1].

#' Spread crowded positions apart in npc space
#'
#' Recursively nudges any position whose gap to its neighbour is smaller than
#' `weight * line_w`, spreading each crowded cluster symmetrically about its
#' centre. Faithful port of `trackViewer:::jitterLables` with grid coordinate
#' conversions removed (inputs/outputs are already npc).
#'
#' @param pos Numeric vector of positions in `[0, 1]`, sorted ascending.
#' @param line_w Label footprint as a fraction of the axis (npc).
#' @param weight Spacing weight; recursion decreases it by 0.2 until `< 0.5`.
#' @return Numeric vector of adjusted positions in npc, same length/order as
#'   `pos`.
#' @keywords internal
#' @noRd
.jitter_positions_npc <- function(pos, line_w, weight = 1.2) {
  if (weight < 0.5) {
    return(pos)
  }
  n <- length(pos)
  pos.diff <- diff(c(0, pos, 1))
  idx <- which(pos.diff < weight * line_w)
  if (length(idx) < 1) {
    return(pos)
  }
  if (all(idx %in% c(1, n + 1))) {
    return(pos)
  }
  idx.diff <- diff(c(-1, idx))
  idx.grp <- rle(idx.diff)
  idx.grp$values[idx.grp$values == 1] <- n + seq_len(sum(idx.grp$values == 1))
  idx.grp <- inverse.rle(idx.grp)
  idx.grp.w <- which(idx.grp > n) - 1
  idx.grp.w <- idx.grp.w[idx.grp.w > 0]
  idx.grp[idx.grp.w] <- idx.grp[idx.grp.w + 1]
  idx.grp <- split(idx, idx.grp)
  flag <- as.numeric(names(idx.grp)) > n
  idx.grp.mul <- lapply(idx.grp[flag], function(.ele) c(.ele[1] - 1, .ele))
  idx.grp.sin <- lapply(idx.grp[!flag], function(.ele) {
    lapply(as.list(.ele), function(.e) c(.e - 1, .e))
  })
  idx.grp.sin <- unlist(idx.grp.sin, recursive = FALSE)
  idx.grp <- c(idx.grp.mul, idx.grp.sin)

  adj.pos <- lapply(idx.grp, function(.ele) {
    .ele <- .ele[.ele > 0 & .ele <= n]
    this.pos <- pos[.ele]
    names(this.pos) <- .ele
    if (length(this.pos) %% 2 == 1) {
      center <- ceiling(length(this.pos) / 2)
    } else {
      center <- length(this.pos) / 2 + 0.5
    }
    if (length(this.pos) > 5) {
      this.pos <- this.pos +
        (seq_along(this.pos) - center) * (weight - 0.1) *
          line_w / ceiling(log(length(this.pos), 5))
    } else {
      this.pos <- this.pos +
        (seq_along(this.pos) - center) * (weight - 0.1) * line_w
    }
    this.pos
  })
  names(adj.pos) <- NULL
  adj.pos <- unlist(adj.pos)
  pos[as.numeric(names(adj.pos))] <- adj.pos

  .jitter_positions_npc(pos, line_w = line_w, weight = weight - 0.2)
}

#' Re-balance label positions across bins in npc space
#'
#' Bins `[0, 1]` into `ceiling(1 / line_w)` bins, moves points from crowded bins
#' into adjacent empty ones, then evenly spaces points within each bin. Faithful
#' port of `trackViewer:::reAdjustLabels` with grid conversions removed; results
#' are clamped to `[0, 1]`.
#'
#' @param pos Numeric vector of positions in `[0, 1]`.
#' @param line_w Label footprint as a fraction of the axis (npc).
#' @return Numeric vector of re-balanced positions in npc, sorted ascending.
#' @keywords internal
#' @noRd
.readjust_positions_npc <- function(pos, line_w) {
  pos <- sort(pos)
  bins <- ceiling(1 / line_w)
  pos.bin <- cut(pos, c(-Inf, (0:bins) * line_w, Inf),
    labels = 0:(bins + 1), right = FALSE
  )
  tbl <- table(pos.bin)
  if (all(tbl < 2)) {
    return(pos)
  }
  tbl.len <- length(tbl)
  if (tbl.len < 3) {
    return(pos)
  }
  loops <- 1000
  loop <- 1
  while (any(tbl == 0) && any(tbl > 1) && loop < loops) {
    tbl.bk <- tbl
    for (i in order(tbl.bk, decreasing = TRUE)) {
      if (tbl[i] > 1 && tbl.bk[i] == tbl[i]) {
        if (i == 1) {
          if (tbl[2] < tbl[1]) {
            half <- sum(tbl[1:2]) / 2
            tbl[2] <- ceiling(half)
            tbl[1] <- floor(half)
          }
        } else if (i == tbl.len) {
          if (tbl[tbl.len] > tbl[tbl.len - 1]) {
            half <- sum(tbl[(tbl.len - 1):tbl.len]) / 2
            tbl[tbl.len - 1] <- ceiling(half)
            tbl[tbl.len] <- floor(half)
          }
        } else {
          if (tbl[i - 1] < tbl[i + 1]) {
            half <- sum(tbl[(i - 1):i]) / 2
            tbl[i - 1] <- floor(half)
            tbl[i] <- ceiling(half)
          } else {
            half <- sum(tbl[i:(i + 1)]) / 2
            tbl[i] <- floor(half)
            tbl[i + 1] <- ceiling(half)
          }
        }
      }
    }
    loop <- loop + 1
  }
  coef <- unlist(lapply(tbl, function(.ele) {
    if (.ele == 0) {
      return(0)
    }
    s <- seq(from = 0, to = 1, length.out = .ele + 1)
    (s[-length(s)] + s[-1]) / 2
  }))
  coef <- coef[coef != 0]
  pos <- (rep(as.numeric(names(tbl)), tbl) - 1 + coef) * line_w
  pmin(pmax(pos, 0), 1)
}

#' Jitter labels to avoid horizontal overlap
#'
#' Adjusts a set of x positions so labels drawn at them do not overlap, by
#' spreading crowded positions apart and re-balancing them across the axis. The
#' algorithm runs entirely in npc (`[0, 1]`) space and is deterministic: the
#' label footprint is supplied explicitly via `label.width` rather than measured
#' from the current graphics device. For draw-time-correct spacing measured
#' against the real panel, use [geom_jitter_text()] instead.
#'
#' @param label.pos Numeric vector of label positions in data coordinates.
#' @param xscale Numeric length-2 vector giving the increasing x-axis range
#'   c(min, max). Reversed scales are not supported.
#' @param label.width Label footprint as a fraction of the axis range (npc).
#'   Default `0.025` approximates one line of text on a ~7-inch device.
#' @param weight Spacing weight passed to the jitter pass (default `1.2`).
#' @return Numeric vector of adjusted positions in data coordinates, in the same
#'   order as `label.pos`.
#' @examples
#' positions <- c(100, 105, 110, 115)
#' jitter_labels(positions, xscale = c(0, 1000))
#' @export
jitter_labels <- function(label.pos, xscale, label.width = 0.025,
                          weight = 1.2) {
  stopifnot(length(xscale) == 2, xscale[1] < xscale[2])
  n <- length(label.pos)
  if (n < 2) {
    return(label.pos)
  }
  ord <- order(label.pos)
  pos_npc <- (label.pos[ord] - xscale[1]) / diff(xscale)
  pos_npc <- .jitter_positions_npc(pos_npc, line_w = label.width, weight = weight)
  pos_npc <- .readjust_positions_npc(pos_npc, line_w = label.width)
  out <- numeric(n)
  out[ord] <- pos_npc * diff(xscale) + xscale[1]
  out
}
