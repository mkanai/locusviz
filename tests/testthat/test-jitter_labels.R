test_that(".jitter_positions_npc leaves already-spread points unchanged", {
  pos <- c(0.1, 0.4, 0.7)
  expect_equal(.jitter_positions_npc(pos, line_w = 0.05), pos)
})

test_that(".jitter_positions_npc spreads crowded points apart", {
  pos <- c(0.49, 0.50, 0.51)
  out <- .jitter_positions_npc(pos, line_w = 0.05)
  expect_false(isTRUE(all.equal(out, pos)))
  # the cluster gets wider than it started
  expect_gt(diff(range(out)), diff(range(pos)))
})

test_that(".readjust_positions_npc returns sorted positions within [0, 1]", {
  pos <- c(0.50, 0.49, 0.51, 0.52)
  out <- .readjust_positions_npc(pos, line_w = 0.05)
  expect_equal(out, sort(out))
  expect_true(all(out >= 0 & out <= 1))
})

test_that(".readjust_positions_npc keeps the same number of points", {
  pos <- c(0.50, 0.49, 0.51, 0.52, 0.53)
  out <- .readjust_positions_npc(pos, line_w = 0.05)
  expect_length(out, length(pos))
})

test_that("jitter_labels returns input order and data coordinates", {
  pos <- c(300, 100, 200) # deliberately unsorted
  out <- jitter_labels(pos, xscale = c(0, 1000), label.width = 0.05)
  expect_length(out, 3)
  # rank order of output matches rank order of input
  expect_equal(order(out), order(pos))
  # results stay within the axis
  expect_true(all(out >= 0 & out <= 1000))
})

test_that("jitter_labels is a no-op for fewer than two points", {
  expect_equal(jitter_labels(numeric(0), c(0, 1000)), numeric(0))
  expect_equal(jitter_labels(500, c(0, 1000)), 500)
})

# Optional fidelity check against the upstream trackViewer internals. Skipped
# when trackViewer is not installed (it is only a Suggests dependency).
test_that("npc core reproduces trackViewer jitterLables + reAdjustLabels", {
  skip_if_not_installed("trackViewer")

  coor <- c(120, 121, 122, 500, 900, 901)
  xscale <- c(100, 1000)
  line_w <- 0.05

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::pushViewport(grid::viewport(xscale = xscale))
  on.exit(grid::popViewport(), add = TRUE, after = FALSE)

  ref <- trackViewer:::jitterLables(coor, xscale, line_w)
  ref <- trackViewer:::reAdjustLabels(ref, line_w)
  ref_npc <- (ref - xscale[1]) / diff(xscale)

  pos_npc <- (coor - xscale[1]) / diff(xscale)
  ours <- .jitter_positions_npc(pos_npc, line_w)
  ours <- .readjust_positions_npc(ours, line_w)

  expect_equal(ours, as.numeric(ref_npc), tolerance = 1e-8)
})

test_that(".readjust_positions_npc clamps edge-crowded points into [0, 1]", {
  # A dense cluster hard against the right edge: upstream reAdjustLabels would
  # spread some points past 1.0 (npc); our port clamps them into [0, 1].
  pos <- c(0.95, 0.96, 0.97, 0.98, 0.99, 1.00)
  out <- .readjust_positions_npc(pos, line_w = 0.05)
  expect_true(all(out >= 0 & out <= 1))
  expect_length(out, length(pos))
})
