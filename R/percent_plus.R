#' Use the `%+%` operator from `ggplot2` for `ggtibble` and `gglist` objects
#' @param e1 Either a `ggtibble` or `gglist` object or an object that can use
#'   the default `ggplot2::%+%` function
#' @param e2 A plot component (see ?ggplot2::`%+%`)
#' @export
`%+%` <- function (e1, e2) {
  UseMethod("%+%")
}

#' @export
`%+%.default` <- function(e1, e2) {
  ggplot2::`%+%`(e1, e2)
}

#' @export
`%+%.gglist` <- function(e1, e2) {
  lapply(FUN = "%+%", X = e1, e2)
}

#' @export
`%+%.ggtibble` <- function(e1, e2) {
  e1$figure <- e1$figure %+% e2
  e1
}
