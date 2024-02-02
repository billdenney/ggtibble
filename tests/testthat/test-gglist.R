# gglist creation ####

test_that("gglist", {
  expect_length(
    gglist(list(data.frame(A = 1), data.frame(B = 2))),
    2
  )
})

# gglist math ####

test_that("vec_arith for gglists", {
  g1 <- new_gglist(list(ggplot2::ggplot(environment = emptyenv())))
  # add a single item to a single gglist
  expect_s3_class(
    g1 + ggplot2::geom_point(),
    "gglist"
  )
  # add a single item to a single gglist
  expect_s3_class(
    g1 + ggplot2::labs(x = "foo"),
    "gglist"
  )
  # add a list of items to a single gglist
  expect_s3_class(
    g1 + list(ggplot2::labs(x = "foo")),
    "gglist"
  )
  expect_s3_class(
    g1 +
      list(
        ggplot2::geom_point(),
        ggplot2::labs(x = "foo")
      ),
    "gglist"
  )
  # List addition occurs per gglist element not per list element (like adding a
  # list to a ggplot2 object)
  expect_length(
    g1 +
      list(
        ggplot2::geom_point(),
        ggplot2::labs(x = "foo")
      ),
    1
  )
})

# new_gglist ####

test_that("new_gglist accepted input classes", {
  expect_s3_class(
    new_gglist(list(ggplot2::labs("foo"))),
    "gglist"
  )
  expect_s3_class(
    new_gglist(list(ggplot2::ggplot())),
    "gglist"
  )
  expect_s3_class(
    new_gglist(list(NULL)),
    "gglist"
  )
})

test_that("new_gglist errors", {
  expect_error(new_gglist("A"), regexp = "`x` must be a list")
  expect_error(
    new_gglist(list("A")),
    regexp = "the contents of 'x' must be NULL, a 'gg' (ggplot), or a 'labels' object",
    fixed = TRUE
  )
})
