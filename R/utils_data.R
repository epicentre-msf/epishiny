#' @keywords internal
#' @noRd
force_reactive <- function(x) {
  if (is.reactive(x)) {
    x()
  } else {
    x
  }
}

#' @keywords internal
#' @noRd
filter_var <- function(x, val) {
  if (length(val)) {
    x %in% val
  } else {
    TRUE
  }
}

#' @keywords internal
#' @noRd
time_stamp <- function() {
  format(Sys.time(), "%Y-%m-%d_%H:%M:%S")
}
