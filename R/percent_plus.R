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
