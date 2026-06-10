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
