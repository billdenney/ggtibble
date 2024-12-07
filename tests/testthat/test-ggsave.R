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
  expected_files <- file.path(tempdir(), c("foo.svg", "bar.svg"))
  expect_equal(
    ggsave(filename = "{A}.svg", plot = all_plots, path = tempdir()),
    expected_files
  )
  unlink(expected_files)
})
