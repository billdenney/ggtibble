#' Generate ggplot2 labels based on data in a ggtibble
#'
#' @param p The ggtibble object
#' @param ... Named arguments to be used as `ggplot2::labs()` labels where the
#'   value is a glue specification
#' @returns `p` with the labels modified
#' @export
labs_glue <- function(p, ...) {
  stopifnot(inherits(p, "ggtibble"))
  args <- list(...)
  stopifnot(length(args) > 0)
  # all ... arguments must be named
  stopifnot(!is.null(names(args)))
  stopifnot(!any(names(args) %in% ""))
  labs_args <- list()
  for (nm in names(args)) {
    labs_args[[nm]] <- glue::glue_data(p, args[[nm]])
  }
  purrr::pmap(.l = labs_args, .f = ggplot2::labs)
}
