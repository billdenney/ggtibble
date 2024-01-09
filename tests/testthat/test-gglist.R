test_that("new_gglist", {
  expect_error(new_gglist("A"), regexp = "`x` must be a list")
  expect_error(
    new_gglist(list("A")),
    regexp = "the contents of 'x' must be NULL, a 'gg' (ggplot), or a 'labels' object",
    fixed = TRUE
  )
})
