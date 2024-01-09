#' @importFrom dplyr group_vars
#' @export
dplyr::group_vars

#' @importFrom ggplot2 ggplot
#' @export
ggplot2::ggplot

#' Make a tibble where one column is the data to plot, one is the gglist, and
#' one is the caption
#'
#' @param data The data.frame to plot
#' @param ... Passed to subsequent methods (usually passed to `gglist()`)
#' @return A data.frame with a column named "data_plot" with the data to plot, "figure" with the gglist, and "caption" with the captions
#' @export
ggtibble <- function(data, ...) {
  UseMethod("ggtibble")
}

#' @describeIn ggtibble The default method for a data.frame or tibble
#' @inheritParams ggplot2::ggplot
#' @param outercols The columns to have outside the nesting
#' @param captionglue The glue specification for creating the caption
#' @param labs Labels to add via `labs_glue()`
#' @export
ggtibble.data.frame <- function(data, mapping = ggplot2::aes(), ..., outercols = group_vars(data), labs = list(), captionglue = "") {
  if (!tibble::is_tibble(data)) {
    data <- tibble::as_tibble(as.data.frame(data))
  }
  d_plot <- tidyr::nest(.data = data, data_plot = !outercols)
  d_plot$figure <- gglist(data = d_plot$data_plot, mapping = mapping, ...)
  d_plot$caption <- glue::glue_data(d_plot, captionglue)
  class(d_plot) <- c("ggtibble", class(d_plot))
  if (length(labs) > 0) {
    d_plot$figure <- d_plot$figure + do.call(what = labs_glue, append(list(p = d_plot), labs))
  }
  d_plot
}

#' @export
knit_print.ggtibble <- function(x, ...) {
  knit_print(x$figure, ...)
}

#' @export
chooseOpsMethod.ggtibble <- function(x, y, mx, my, cl, reverse) {
  # Always use the ggtibble method (which will usually then pass to the gglist
  # method for the "figure" column)
  TRUE
}

#' @export
Ops.ggtibble <- function(e1, e2) {
  if (nargs() == 1) {
    stop("Unary operations are not defined for ggtibble objects")
  }
  if (.Generic == "+") {
    # Add the gglist object to something else (usually a ggplot2-style object)
    e1$figure <- e1$figure + e2
  } else {
    stop(.Generic, " is not defined for ggtibble objects")
  }
  e1
}
