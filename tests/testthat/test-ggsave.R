test_that("ggsave", {
  d_plot <-
    data.frame(
      A = rep(c("foo", "bar"), each = 4),
      B = 1:8,
      C = 11:18,
      Bunit = "mg",
      Cunit = "km"
    )
  all_plots <-
    ggtibble(
      d_plot,
      ggplot2::aes(x = B, y = C),
      outercols = c("A", "Bunit", "Cunit"),
      caption = "All the {A}",
      labs = list(x = "B ({Bunit})", y = "C ({Cunit})")
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_line()
  expected_files <- file.path(tempdir(), c("foo.png", "bar.png"))
  expect_equal(
    ggsave(filename = "{A}.png", plot = all_plots, path = tempdir()),
    expected_files
  )
  unlink(expected_files)

  # Enough filenames must be given
  expect_error(
    ggsave(filename = "a.png", plot = all_plots$figure, path = tempdir()),
    regexp = "There must be one `filename` per `plot`",
    fixed = TRUE
  )
  # Filenames must be unique (don't accidentally overwrite anything)
  expect_error(
    ggsave(filename = "{Bunit}.png", plot = all_plots, path = tempdir()),
    regexp = "Each `filename` must be unique",
    fixed = TRUE
  )
})
