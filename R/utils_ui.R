.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    "epishiny",
    system.file("assets", package = "epishiny")
  )
}

#' @export
use_epishiny <- function() {
  header <- tags$head(
    tags$script(src = "epishiny/js/weekNumber.js"),
    shinyjs::useShinyjs()
  )
  shiny::singleton(header)
}

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

  x
}

google_font <- function(font) {
  font <- stringr::str_replace_all(font, " ", "+")
  glue::glue("https://fonts.googleapis.com/css2?family={font}:wght@300;400;700&display=swap")
}

div_reactive <- function(...) {
  tags$div(class = "reactive-width", ...)
}

div_inline <- function(..., margin_right = TRUE) {
  display <- "display: inline-block;"
  margin <- ifelse(margin_right, "margin-right: 10px;", "")
  tags$div(style = paste(display, margin), ...)
}

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
#' @param breaks
#' @param lab_accuracy accuracy of labels, passed to [`scales::number`]
#' @param replace_Inf if `Inf` is your final break, replace with a + sign in the label?
#'
#' @export
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
#' @export
frmt_num <- function(x, accuracy = .1) {
  n <- scales::number(x, accuracy = accuracy, scale_cut = scales::cut_short_scale())
  n <- stringr::str_remove(n, "\\.0+(?=[a-zA-Z])")
  n <- stringr::str_remove(n, "\\.0+$")
  n
}

tab_box_custom <- function(
    ...,
    id = NULL,
    selected = NULL,
    title = NULL,
    footer = NULL,
    inputs = NULL,
    width = 6,
    height = NULL,
    side = c("left", "right")
) {
  side <- match.arg(side)
  content <- shiny::tabsetPanel(..., id = id, selected = selected)
  content$attribs$class <- "nav-tabs-custom"
  if (!is.null(height)) {
    content <- tagAppendAttributes(
      content,
      style = paste0("height: ", validateCssUnit(height))
    )
  }
  if (side == "right") {
    content$children[[1]] <- tagAppendAttributes(
      content$children[[1]],
      class = "pull-right"
    )
  }
  if (!is.null(title)) {
    if (side == "left") {
      titleClass <- "pull-right"
    } else {
      titleClass <- "pull-left"
    }
    content$children[[1]] <- htmltools::tagAppendChild(
      content$children[[1]],
      tags$li(class = paste("box-header", titleClass), h3(class = "box-title", style = "display: inline-block; margin-right: 15px;", title))
    )
  }
  if (!is.null(inputs)) {
    if (side == "left") {
      titleClass <- "pull-right"
    } else {
      titleClass <- "pull-left"
    }
    content$children[[1]] <- htmltools::tagAppendChild(
      content$children[[1]],
      tags$li(class = titleClass, purrr::map(inputs, div_inline))
    )
  }
  div(class = paste0("col-sm-", width), content)
}

box_custom <- function(
    ...,
    title = NULL,
    inputs = NULL,
    input_right = NULL,
    footer = NULL,
    status = NULL,
    solidHeader = FALSE,
    background = NULL,
    width = 6,
    height = NULL,
    collapsible = FALSE,
    collapsed = FALSE,
    headerId = NULL
) {
  boxClass <- "box"
  if (solidHeader || !is.null(background)) {
    boxClass <- paste(boxClass, "box-solid")
  }
  if (!is.null(status)) {
    shinydashboard:::validateStatus(status)
    boxClass <- paste0(boxClass, " box-", status)
  }
  if (collapsible && collapsed) {
    boxClass <- paste(boxClass, "collapsed-box")
  }
  if (!is.null(background)) {
    shinydashboard:::validateColor(background)
    boxClass <- paste0(boxClass, " bg-", background)
  }
  style <- NULL
  if (!is.null(height)) {
    style <- paste0("height: ", shiny::validateCssUnit(height))
  }
  titleTag <- NULL
  if (!is.null(title)) {
    titleTag <- h3(class = "box-title", style = "display: inline-block; margin-right: 15px;", title)
  }
  collapseTag <- NULL
  if (collapsible) {
    buttonStatus <- status %OR% "default"
    collapseIcon <- if (collapsed) {
      "plus"
    } else {
      "minus"
    }
    collapseTag <- div(
      class = "box-tools pull-right",
      tags$button(class = paste0("btn btn-box-tool"), `data-widget` = "collapse", shiny::icon(collapseIcon))
    )
  }
  input_rightTag <- NULL
  if (!is.null(input_right)) {
    input_rightTag <- div(
      class = "box-tools pull-right",
      input_right
    )
  }
  headerTag <- NULL
  if (!is.null(titleTag) || !is.null(collapseTag)) {
    headerTag <- div(id = headerId, class = "box-header", titleTag, inputs, input_rightTag, collapseTag)
  }
  div(class = if (!is.null(width)) {
    paste0("col-sm-", width)
  }, div(class = boxClass, style = if (!is.null(style)) {
    style
  }, headerTag, div(class = "box-body", ...), if (!is.null(footer)) {
    div(class = "box-footer", footer)
  }))
}

box_w_inputs <- function(..., title, inputs, input_right = NULL,
                         width = 6, height = NULL, footer = NULL, headerId = NULL) {
  box_custom(
    width = width, height = height, solidHeader = TRUE, footer = footer,
    title = title, inputs = purrr::map(inputs, div_inline), input_right = input_right,
    headerId = headerId,
    ...
  )
}

box_w_dropdown_inputs <- function(..., title, inputs = NULL, input_right = NULL,
                                  width = 6, height = NULL, footer = NULL,
                                  btn_lab = "options", btn_id = NULL) {
  box_custom(
    width = width, height = height, solidHeader = TRUE, footer = footer,
    title = title, inputs = tags$div(
      id = btn_id, style = "display: inline-block;",
      shinyWidgets::dropdownButton(
        size = "sm", label = btn_lab, icon = shiny::icon("sliders"),
        inline = FALSE, width = 300, circle = FALSE, margin = 10,
        inputs
      )
    ),
    ...
  )
}

valueBoxSpark <- function(value, title, sparkobj = NULL, subtitle, info = NULL,
                          icon = NULL, color = "aqua", width = 3, href = NULL){

  shinydashboard:::validateColor(color)

  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")

  info_icon <- tags$small(
    tippy::tippy(tags$i(
      class = "fa fa-info-circle fa-lg",
      # title = info,
      # `data-toggle` = "tooltip",
      style = "color: rgba(255, 255, 255, 0.75);"
    ), info, placement = "bottom"),
    # bs3 pull-right
    # bs4 float-right
    class = "pull-right float-right"
  )

  boxContent <- div(
    class = paste0("small-box bg-", color),
    # bs3 icon-large
    # bs4 icon
    if (!is.null(icon)) div(class = "icon-large  icon", icon),
    div(
      class = "inner",
      tags$small(title),
      if (!is.null(info)) info_icon,
      h3(value),
      if (!is.null(sparkobj)) sparkobj,
      p(subtitle)
    )
  )

  if (!is.null(href))
    boxContent <- a(href = href, boxContent)

  div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    boxContent
  )
}
