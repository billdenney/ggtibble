test_that("labs_glue", {
  # cheat about making a ggtibble object
  d_ggtibble <-
    dplyr::tibble(
      A = c("A", "B"),
      B = c("B", "C")
    )
  class(d_ggtibble) <- c("ggtibble", class(d_ggtibble))
  expect_error(
    labs_glue(d_ggtibble),
    regexp = "length(args) > 0 is not TRUE",
    fixed = TRUE
  )
  expect_equal(
    labs_glue(d_ggtibble, y = "A"),
    list(ggplot2::labs(y = "A"))
  )
  expect_equal(
    labs_glue(d_ggtibble, y = "{A}"),
    list(
      ggplot2::labs(y = "A"),
      ggplot2::labs(y = "B")
    )
  )
  expect_equal(
    labs_glue(d_ggtibble, x = NULL, y = "{A}"),
    list(
      ggplot2::labs(x = character(0), y = "A"),
      ggplot2::labs(x = character(0), y = "B")
    )
  )
})
