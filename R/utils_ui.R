
#' @export
use_epishiny <- function() {
  header <- tags$head(
    tags$script(src = "epishiny/js/weekNumber.js"),
    tags$style(".dropdown-menu {z-index: 1000 !important;} .bslib-nav-item {margin: 0 !important;}"),
    shinyjs::useShinyjs(),
    waiter::useWaiter()
  )
  shiny::singleton(header)
}

#' @keywords internal
#' @noRd
epi_pals <- function() {
  x <- list()

  x$pal20 <- c(
    "#4E79A7FF", "#A0CBE8FF", "#F28E2BFF", "#FFBE7DFF", "#59A14FFF",
    "#8CD17DFF", "#B6992DFF", "#F1CE63FF", "#499894FF", "#86BCB6FF",
    "#E15759FF", "#FF9D9AFF", "#79706EFF", "#BAB0ACFF", "#D37295FF",
    "#FABFD2FF", "#B07AA1FF", "#D4A6C8FF", "#9D7660FF", "#D7B5A6FF"
  )

  x$pal10 <- c(
    "#4E79A7FF", "#F28E2BFF", "#E15759FF", "#76B7B2FF", "#59A14FFF",
    "#EDC948FF", "#B07AA1FF", "#FF9DA7FF", "#9C755FFF", "#BAB0ACFF"
  )

  x$dark2 <- c(
    "#4E79A7", "#1B9E77", "#D95F02", "#7570B3", "#E7298A",
    "#66A61E", "#E6AB02", "#A6761D", "#666666"
  )

  x$d310 <- c(
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
    "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"
  )

  x
}

#' @keywords internal
#' @noRd
make_select_filter <- function(var, lab, ns, df_ll, ...) {
  vec <- df_ll[[var]]
  if (is.factor(vec)) {
    choices <- levels(vec)
  } else if (is.character(vec)) {
    choices <- sort(unique(vec))
  } else {
    stop("Grouping variables must be factor or character class")
  }
  shinyWidgets::pickerInput(
    inputId = ns(var),
    label = lab,
    choices = choices,
    selected = NULL,
    options = picker_opts(...),
    multiple = TRUE
  )
}

#' @keywords internal
#' @noRd
picker_opts <- function(actions = TRUE,
                        search = FALSE,
                        none_text = "All",
                        selected_text = "selected",
                        container = "body",
                        ...) {
  shinyWidgets::pickerOptions(
    actionsBox = actions,
    liveSearch = search,
    selectedTextFormat = "count > 2",
    countSelectedText = paste("{0}", selected_text),
    noneSelectedText = none_text,
    container = container,
    ...
  )
}

#' Format break labels
#'
#' @param breaks numeric vector of breaks
#' @param lab_accuracy accuracy of labels, passed to [`scales::number`]
#' @param replace_Inf if `Inf` is your final break, replace with a + sign in the label?
#'
#' @keywords internal
#' @noRd
label_breaks <- function(breaks, lab_accuracy = .1, replace_Inf = TRUE) {
  labs <- sprintf(
    "%s-%s",
    frmt_num(breaks[1:length(breaks) - 1], accuracy = lab_accuracy),
    frmt_num(breaks[2:length(breaks)] - 1, accuracy = lab_accuracy)
  )
  if (replace_Inf){
    labs <- gsub("-Inf", "+", labs)
  }
  return(labs)
}

#' Format numbers is units when large
#'
#' @param x a number to format
#' @param accuracy accuracy of labels, passed to [`scales::number`]
#'
#' @keywords internal
#' @noRd
frmt_num <- function(x, accuracy = .1) {
  n <- scales::number(x, accuracy = accuracy, scale_cut = scales::cut_short_scale())
  n <- stringr::str_remove(n, "\\.0+(?=[a-zA-Z])")
  n <- stringr::str_remove(n, "\\.0+$")
  n
}
