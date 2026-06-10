test_that("geom_jitter_text builds and renders without error", {
  df <- data.frame(
    x = c(100, 101, 102, 500),
    y = c(1, 1, 1, 1),
    label = c("a", "b", "c", "d")
  )
  p <- ggplot2::ggplot(df) +
    geom_jitter_text(ggplot2::aes(x = x, y = y, label = label),
      angle = 90
    )
  expect_s3_class(ggplot2::ggplotGrob(p), "gtable")

  dev <- grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_silent(print(p))
})

test_that("geom_jitter_text accepts a connector anchor via yend", {
  df <- data.frame(
    x = c(100, 101), y = c(2, 2), yend = c(0.5, 0.5),
    label = c("a", "b")
  )
  p <- ggplot2::ggplot(df) +
    geom_jitter_text(
      ggplot2::aes(x = x, y = y, yend = yend, label = label),
      connector = TRUE
    )
  expect_s3_class(ggplot2::ggplotGrob(p), "gtable")
})

test_that("geom_jitter_text draws a diagonal+straight dogleg connector", {
  df <- data.frame(
    x = c(100, 101, 102), y = c(2, 2, 2), yend = c(0.5, 0.5, 0.5),
    label = c("aaaa", "bbbb", "cccc")
  )
  build_grob <- function(straight) {
    p <- ggplot2::ggplot(df) +
      geom_jitter_text(
        ggplot2::aes(x = x, y = y, yend = yend, label = label),
        angle = 90, connector = TRUE, connector.straight = straight
      )
    grid::makeContent(ggplot2::layer_grob(p)[[1]])
  }

  dev <- grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())

  # With a straight fraction the connector is two segments (diagonal + vertical)
  # plus the text grob; with straight = 0 the diagonal goes all the way so the
  # vertical segment is zero-length but still present (3 children either way).
  g <- build_grob(0.33)
  seg_children <- vapply(
    g$children,
    function(ch) inherits(ch, "segments"),
    logical(1)
  )
  expect_equal(sum(seg_children), 2L)
})
