#' Extract all expressions to be evaluated by `glue()`
#'
#' @param ... passed to `glue()`
#' @returns A character vector of expressions to be evaluated
#' @examples
#' extract_glue_expr("foo {character(0)} {bar}")
extract_glue_expr <- function(...) {
  ret <- character(0)
  expr_capture <- function(text, envir) {
    ret <<- c(ret, text)
    text
  }
  glue::glue(..., .transformer = expr_capture)
  ret
}
