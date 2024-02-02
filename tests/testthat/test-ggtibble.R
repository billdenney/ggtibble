test_that("ggtibble", {
  # Test length 1
  v1 <- ggtibble(data.frame(A = 1, B = 2))
  expect_named(v1, expected = c("data_plot", "figure", "caption"))
  expect_length(v1$figure, 1)
  expect_equal(v1$data_plot[[1]], tibble::tibble(A = 1, B = 2))
  expect_equal(v1$caption, "")

  # Test length >1
  v2 <- ggtibble(data.frame(A = 1:2, B = 3:4), outercols = "A")
  expect_named(v2, expected = c("A", "data_plot", "figure", "caption"))
  expect_length(v2$figure, 2)
  expect_equal(
    v2$data_plot,
    list(
      tibble::tibble(B = 3),
      tibble::tibble(B = 4)
    )
  )
  expect_equal(v2$caption, rep("", 2))

  # Test captioning
  v3 <- ggtibble(data.frame(A = 1:2, B = 3:4), outercols = "A", caption = "A is {A}")
  expect_named(v3, expected = c("A", "data_plot", "figure", "caption"))
  expect_length(v3$figure, 2)
  expect_equal(
    v3$data_plot,
    list(
      tibble::tibble(B = 3),
      tibble::tibble(B = 4)
    )
  )
  expect_equal(v3$caption, paste("A is", 1:2))

  # Test labels
  v4 <- ggtibble(data.frame(A = 1:2, B = 3:4), outercols = "A", labs = list(x = "A is {A}"))
  expect_named(v4, expected = c("A", "data_plot", "figure", "caption"))
  expect_length(v4$figure, 2)
  expect_equal(
    v4$data_plot,
    list(
      tibble::tibble(B = 3),
      tibble::tibble(B = 4)
    )
  )
  expect_named(v4$figure[[1]]$labels, "x")
  expect_equal(
    v4$figure[[1]]$labels$x,
    "A is 2"
  )
  expect_equal(v4$caption, rep("", 2))

  # Test math
  expect_error(
    v4 - ggplot2::geom_point(),
    regexp = "- is not defined for ggtibble objects",
    fixed = TRUE
  )
  expect_error(
    +v4,
    regexp = "Unary operations are not defined for ggtibble objects",
    fixed = TRUE
  )
  v5 <- v4 + ggplot2::geom_point()
  expect_equal(nrow(v5), 2)
  expect_equal(v4$figure[[1]]$layers, list())
  expect_s3_class(v5$figure[[1]]$layers[[1]]$geom, "GeomPoint")
})
