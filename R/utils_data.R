
force_reactive <- function(x) {
  if (is.reactive(x)) {
    x()
  } else {
    x
  }
}

filter_var <- function(x, val) {
  if (length(val)) {x %in% val} else {TRUE}
}
