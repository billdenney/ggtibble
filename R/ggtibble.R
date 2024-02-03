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
#' @param caption The glue specification for creating the caption
#' @param labs Labels to add via `labs_glue()`
#' @returns A `ggtibble` object which is a tibble with columns named "figure"
#'   which is a `gglist` object (a list of ggplots), "data_plot" which is the a
#'   list of data.frames making up the source data used for each individual
#'   plot, "caption" which is the text to use for the plot caption, and all of
#'   the `outercols` used for nesting.
#' @examples
#' d_plot <-
#'   data.frame(
#'     A = rep(c("foo", "bar"), each = 4),
#'     B = 1:8,
#'     C = 11:18,
#'     Bunit = "mg",
#'     Cunit = "km"
#'   )
#' all_plots <-
#'   ggtibble(
#'     d_plot,
#'     ggplot2::aes(x = B, y = C),
#'     outercols = c("A", "Bunit", "Cunit"),
#'     caption = "All the {A}",
#'     labs = list(x = "B ({Bunit})", y = "C ({Cunit})")
#'   ) +
#'   ggplot2::geom_point() +
#'   ggplot2::geom_line()
#' knit_print(all_plots)
#' @export
ggtibble.data.frame <- function(data, mapping = ggplot2::aes(), ..., outercols = group_vars(data), labs = list(), caption = "") {
  if (!tibble::is_tibble(data)) {
    data <- tibble::as_tibble(as.data.frame(data))
  }
  d_plot <- tidyr::nest(.data = data, data_plot = !tidyr::all_of(outercols))
  d_plot$figure <- gglist(data = d_plot$data_plot, mapping = mapping, ...)
  d_plot$caption <- glue::glue_data(d_plot, caption)
  class(d_plot) <- c("ggtibble", class(d_plot))
  if (length(labs) > 0) {
    d_plot$figure <- d_plot$figure + do.call(what = labs_glue, append(list(p = d_plot), labs))
  }
  d_plot
}

#' @describeIn knit_print.gglist Print the plots in a `ggtibble` object
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
