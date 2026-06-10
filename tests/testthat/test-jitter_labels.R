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
