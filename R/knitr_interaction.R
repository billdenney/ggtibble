# Store the default knitr source hook for when not using ggtibble
default_source_hook <- knitr::knit_hooks$get('source')

knitr_hook_ggtibble <- function(x, options) {

}

knitr_option_ggtibble <- function(options) {
  if (!is.null(options$ggtibble)) {
    stopifnot(inherits(options$ggtibble, "ggtibble"))

    if (is.null(options$fig.cap)) {
      options$fig.cap <- options$ggtibble$caption
    }
  }
}
