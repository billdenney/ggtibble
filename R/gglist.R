#' Generate a list of ggplots from a list of data.frames
#'
#' @param data A list of data.frames (or similar objects)
#' @inheritParams ggplot2::ggplot
#' @return A list of ggplot2 objects
#' @examples
#' mydata <-
#'   list(
#'     data.frame(x = 1:3, y = 3:1),
#'     data.frame(x = 4:7, y = 7:4)
#'   )
#' gglist(mydata, ggplot2::aes(x = x, y = y)) +
#'   ggplot2::geom_point()
#' @export
gglist <- function(data = NULL, mapping = ggplot2::aes(), ..., environment = parent.frame()) {
  new_gglist(
    lapply(X = data, FUN = ggplot2::ggplot, mapping = mapping, ..., environment = environment)
  )
}

new_gglist <- function(x = list()) {
  if (!inherits(x, "list")) {
    stop("`x` must be a list")
  }
  x_null <- vapply(X = x, FUN = is.null, FUN.VALUE = TRUE)
  x_gg <- vapply(X = x, FUN = inherits, "gg", FUN.VALUE = TRUE)
  x_labels <- vapply(X = x, FUN = inherits, "labels", FUN.VALUE = TRUE)
  if (!all(x_null | x_gg | x_labels)) {
    rlang::abort("the contents of 'x' must be NULL, a 'gg' (ggplot), or a 'labels' object")
  }
  vctrs::new_vctr(x, class = "gglist")
}

vec_ptype_abbr.gglist <- function(x, ...) {
  "gglst"
}

#' @export
format.gglist <- function(x, ...) {
  rep("A ggplot object", length(x))
}

#' @export
print.gglist <- function(x, ...) {
  for (idx in seq_along(x)) {
    print(x[[idx]], ...)
  }
  invisible(x)
}

#' @export
chooseOpsMethod.gglist <- function(x, y, mx, my, cl, reverse) {
  inherits(y, "gg") | inherits(y, "labels") | inherits(y, "list")
}

#' @export
#' @importFrom vctrs vec_arith
vctrs::vec_arith
#' @export
#' @method vec_arith gglist
vec_arith.gglist <- function(op, x, y, ...) {
  UseMethod("vec_arith.gglist", y)
}
#' @export
#' @method vec_arith.gglist gglist
vec_arith.gglist.gglist <- function(op, x, y, ...) {
  stopifnot(op == "+")
  stopifnot(length(y) %in% c(1, length(x)))
  new_gglist(
    mapply(FUN = "+", x, y, ..., SIMPLIFY = FALSE)
  )
}
#' @export
#' @method vec_arith.gglist list
vec_arith.gglist.list <- function(op, x, y, ...) {
  stopifnot(op == "+")
  ret <- x
  for (idx in seq_along(ret)) {
    # Add the entire list to each gglist object
    ret[[idx]] <- ret[[idx]] + y
  }
  new_gglist(ret)
}
#' @export
#' @method vec_arith.gglist gg
vec_arith.gglist.gg <- function(op, x, y, ...) {
  stopifnot(op == "+")
  new_gglist(
    lapply(FUN = "+", X = x, y, ...)
  )
}
#' @export
#' @method vec_arith.gglist labels
vec_arith.gglist.labels <- function(op, x, y, ...) {
  stopifnot(op == "+")
  new_gglist(
    lapply(FUN = "+", X = x, y, ...)
  )
}

#' @importFrom knitr knit_print
#' @export
knitr::knit_print

#' Print a list of plots made by gglist
#'
#' @param x The gglist object
#' @param ... extra arguments to `knit_print()`
#' @param filename A filename with an optional "%d" sprintf pattern for saving
#'   the plots
#' @param fig_suffix Any text to add after the figure (often
#'   "\n\n\\FloatBarrier\n\n")
#' @return The list, invisibly
#' @family knitters
#' @export
knit_print.gglist <- function(x, ..., filename = NULL, fig_suffix = "\n\n") {
  if (!is.null(filename)) {
    if (grepl(x = filename, pattern = "%[0-9]*d")) {
      filename <- sprintf(filename, seq_along(x))
    }
  }
  stopifnot(`\`filename\` must be either NULL or the same length as \`x\`` = is.null(filename) |
    length(filename) == length(x))
  lapply(X = seq_along(x), FUN = function(idx) {
    knitr::knit_print(x = x[[idx]], ..., filename = filename[[idx]], fig_suffix = fig_suffix)
  })
  invisible(x)
}

#' Print a ggplot (usually within knit_print.gglist)
#'
#' @param x The gg object (i.e. a ggplot)
#' @param ... Ignored
#' @param filename A filename saving the plot
#' @param fig_prefix Text to `cat()` before the figure is printed
#' @inheritParams knit_print.gglist
#' @inheritParams ggplot2::ggsave
#' @return The gg object, invisibly
#' @family knitters
#' @export
knit_print.gg <- function(x, ..., fig_prefix, fig_suffix, filename = NULL, width = 6, height = 4, units = "in") {
  cat("\n\n")
  if (!missing(fig_prefix)) {
    cat(fig_prefix)
  }
  print(x, ...)
  if (!is.null(filename)) {
    ggplot2::ggsave(
      filename = filename, plot = x, width = width,
      height = height, units = units
    )
  }
  if (!missing(fig_suffix)) {
    cat(fig_suffix)
  }
  cat("\n\n")
  invisible(x)
}
