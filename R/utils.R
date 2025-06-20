#' @noRd
use_epishiny <- function() {
  header <- shiny::tags$head(
    tags$script(src = "epishiny/js/weekNumber.js"),
    tags$style(".dropdown-menu {z-index: 1000 !important;} .bslib-nav-item {margin: 0 !important;}"),
    shinyjs::useShinyjs(),
    waiter::useWaiter()
  )
  shiny::singleton(header)
}

#' Pipe operator re-export
#' @importFrom magrittr %>%
#' @noRd
magrittr::`%>%`

#' Assignment pipe operator re-export
#' @importFrom magrittr %<>%
#' @noRd
magrittr::`%<>%`

#' Definition operator re-export
#' @importFrom rlang :=
#' @noRd
rlang::`:=`

#' Force evaluation of reactive expressions
#'
#' @description
#' Evaluates reactive expressions if the input is reactive, otherwise returns the input as-is
#'
#' @param x A reactive expression or regular value
#' @return The evaluated reactive expression or the original value
#' @noRd
force_reactive <- function(x) {
  if (shiny::is.reactive(x)) {
    x()
  } else {
    x
  }
}

#' Filter vector based on value membership
#'
#' @description
#' Creates a logical vector indicating which elements of x are in val
#'
#' @param x Vector to filter
#' @param val Values to match against
#' @return Logical vector indicating membership
#' @noRd
filter_var <- function(x, val) {
  if (length(val)) {
    x %in% val
  } else {
    TRUE
  }
}

#' Generate timestamp string
#'
#' @description
#' Creates a formatted timestamp string in "YYYY-MM-DD_HHMMSS" format
#'
#' @return Character string containing formatted current timestamp
#' @noRd
time_stamp <- function() {
  format(Sys.time(), "%Y-%m-%d_%H%M%S")
}

#' Generate epidemiological color palettes
#'
#' @description
#' Returns a list containing predefined color palettes for epidemiological visualizations
#'
#' @return A list containing color palettes:
#'   \item{pal20}{A 20-color palette suitable for categorical data}
#' @noRd
epi_pals <- function() {
  x <- list()

  x$pal20 <- c(
    "#4E79A7FF",
    "#A0CBE8FF",
    "#F28E2BFF",
    "#FFBE7DFF",
    "#59A14FFF",
    "#8CD17DFF",
    "#B6992DFF",
    "#F1CE63FF",
    "#499894FF",
    "#86BCB6FF",
    "#E15759FF",
    "#FF9D9AFF",
    "#79706EFF",
    "#BAB0ACFF",
    "#D37295FF",
    "#FABFD2FF",
    "#B07AA1FF",
    "#D4A6C8FF",
    "#9D7660FF",
    "#D7B5A6FF"
  )

  x$pal10 <- c(
    "#4E79A7FF",
    "#F28E2BFF",
    "#E15759FF",
    "#76B7B2FF",
    "#59A14FFF",
    "#EDC948FF",
    "#B07AA1FF",
    "#FF9DA7FF",
    "#9C755FFF",
    "#BAB0ACFF"
  )

  x$dark2 <- c(
    "#4E79A7",
    "#1B9E77",
    "#D95F02",
    "#7570B3",
    "#E7298A",
    "#66A61E",
    "#E6AB02",
    "#A6761D",
    "#666666"
  )

  x$d310 <- c(
    "#1f77b4",
    "#ff7f0e",
    "#2ca02c",
    "#d62728",
    "#9467bd",
    "#8c564b",
    "#e377c2",
    "#7f7f7f",
    "#bcbd22",
    "#17becf"
  )

  x$vibrant <- c(
    "#0077BB",
    "#33BBEE",
    "#009988",
    "#EE7733",
    "#CC3311",
    "#EE3377"
  )

  x$muted <- c(
    "#332288",
    "#88CCEE",
    "#44AA99",
    "#117733",
    "#999933",
    "#DDCC77",
    "#CC6677",
    "#882255",
    "#AA4499"
  )

  x$aurora <- c("#BF616A", "#D08770", "#EBCB8B", "#A3BE8C", "#B48EAD")

  x$frost <- c("#5E81AC", "#81A1C1", "#88C0D0", "#8FBCBB")

  x
}

prepare_palette <- function(
  n,
  missing_data = FALSE,
  na_colour = "#666666",
  pal = epi_pals()$aurora,
  alpha = 0.8
) {
  if (!length(pal)) {
    pal <- epi_pals()$aurora
  }
  n_pal <- length(pal)
  n_non_na <- ifelse(missing_data, n - 1, n)
  pal <- if (n_non_na > n_pal && n_non_na < 10) {
    grDevices::colorRampPalette(epi_pals()$aurora)(n_non_na)
  } else if (n_non_na > n_pal) {
    epi_pals()$pal20
  } else {
    pal
  }
  if (missing_data) {
    pal[n] <- na_colour
  }
  scales::alpha(pal, alpha)
}

#' @noRd
choro_pals <- function() {
  list(
    diverging = c(
      "BrBG",
      "PiYG",
      "PRGn",
      "PuOr",
      "RdBu",
      "RdGy",
      "RdYlBu",
      "RdYlGn",
      "Spectral"
    ),
    sequential = c(
      "Blues",
      "BuGn",
      "BuPu",
      "GnBu",
      "Greens",
      "Greys",
      "Oranges",
      "OrRd",
      "PuBu",
      "PuBuGn",
      "PuRd",
      "Purples",
      "RdPu",
      "Reds",
      "YlGn",
      "YlGnBu",
      "YlOrBr",
      "YlOrRd"
    ),
    viridis = c(
      "viridis",
      "magma",
      "inferno",
      "plasma"
    )
  )
}

#' @noRd
choro_breaks <- function() {
  c(
    "fixed",
    "sd",
    "equal",
    "pretty",
    "quantile",
    "kmeans",
    "hclust",
    "bclust",
    "fisher",
    "jenks",
    "dpih",
    "q6",
    "Q6",
    "geom",
    "arith",
    "em",
    "msd",
    "ckmeans"
  )
}

#' @noRd
setup_group_filter <- function(var, lab, ns, ...) {
  if (is.null(lab)) {
    lab <- var
  }
  # shinyWidgets::pickerInput(
  #   inputId = ns(var),
  #   label = lab,
  #   choices = NULL,
  #   selected = NULL,
  #   options = picker_opts(...),
  #   multiple = TRUE
  # )
  shiny::selectizeInput(
    inputId = ns(var),
    label = lab,
    choices = NULL,
    selected = NULL,
    multiple = TRUE,
    options = list(placeholder = "All", plugins = "remove_button")
  )
}

#' @noRd
update_group_filter <- function(session, var, df) {
  vec <- df[[var]]
  if (is.factor(vec)) {
    choices <- levels(droplevels(vec))
  } else if (is.character(vec)) {
    choices <- sort(unique(vec))
  } else {
    stop("Grouping variables must be factor or character class")
  }
  # shinyWidgets::updatePickerInput(session, var, choices = choices)
  shiny::updateSelectizeInput(session, var, choices = choices)
}

#' @noRd
make_select_filter <- function(var, lab, ns, df, ...) {
  vec <- df[[var]]
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

#' @noRd
picker_opts <- function(actions = TRUE, search = FALSE, none_text = "All", selected_text = "selected", container = "body", ...) {
  shinyWidgets::pickerOptions(
    actionsBox = actions,
    liveSearch = search,
    selectedTextFormat = "count > 1",
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
#' @noRd
label_breaks <- function(breaks, lab_accuracy = .1, replace_Inf = TRUE) {
  labs <- sprintf(
    "%s-%s",
    frmt_num(breaks[1:length(breaks) - 1], accuracy = lab_accuracy),
    frmt_num(breaks[2:length(breaks)] - 1, accuracy = lab_accuracy)
  )
  if (replace_Inf) {
    labs <- gsub("-Inf", "+", labs)
  }
  return(labs)
}

#' Format numbers with scale units when large
#'
#' @param x a number to format
#' @param accuracy accuracy of labels, passed to [`scales::number`]
#'
#' @noRd
frmt_num <- function(x, accuracy = .1) {
  n <- scales::number(x, accuracy = accuracy, scale_cut = scales::cut_short_scale())
  n <- stringr::str_remove(n, "\\.0+(?=[a-zA-Z])")
  n <- stringr::str_remove(n, "\\.0+$")
  n
}

#' @noRd
hc_week_labels <- function(week_letter = getOption("epishiny.week.letter", "W")) {
  highcharter::JS(
    glue::glue(
      "function () {
         var date = new Date(this.value);
         var year = date.getWeekYear();
         var week = date.getWeek();
         return year + '-{{week_letter}}' + week;
     }",
      .open = "{{",
      .close = "}}"
    )
  )
}


#' @noRd
my_hc_export <- function(
  hc,
  title,
  subtitle,
  credits,
  caption,
  colors,
  width = 900,
  height = 450,
  dl_buttons = c("downloadPNG", "downloadJPEG", "downloadSVG", "separator", "downloadCSV", "downloadXLS"),
  dl_text = "Download",
  filename = "EPI-FIG-"
) {
  set_hc_val <- function(first, second) {
    if (!missing(first)) {
      out <- first
    } else if (!is.null(second)) {
      out <- second
    } else {
      out <- NULL
    }
    return(out)
  }

  title <- set_hc_val(title, hc$x$hc_opts$title$text)
  subtitle <- set_hc_val(subtitle, hc$x$hc_opts$subtitle$text)
  colors <- set_hc_val(colors, hc$x$hc_opts$colors)
  credits <- set_hc_val(credits, hc$x$hc_opts$credits$text)
  show_credits <- ifelse(length(credits), TRUE, FALSE)
  show_caption <- ifelse(length(caption), TRUE, FALSE)

  legend_title <- stringr::str_remove(hc$x$hc_opts$legend$title$text, "\\(click to filter\\)")

  highcharter::hc_exporting(
    hc,
    enabled = TRUE,
    sourceWidth = width,
    sourceHeight = height,
    buttons = list(contextButton = list(menuItems = dl_buttons, text = dl_text)),
    filename = paste0(filename, time_stamp()),
    csv = list(dateFormat = "%d/%m/%Y"),
    tableCaption = "",
    useMultiLevelHeaders = FALSE,
    formAttributes = list(target = "_blank"),
    chartOptions = list(
      title = list(text = title),
      subtitle = list(text = subtitle),
      credits = list(enabled = show_credits, text = credits),
      caption = list(enabled = show_caption, text = caption),
      colors = colors,
      legend = list(title = list(text = legend_title)),
      xAxis = list(plotBands = list()), # remove plotbands
      rangeSelector = list(enabled = FALSE),
      navigator = list(enabled = FALSE),
      # plotOptions = list(series = list(dataLabels = list(enabled = TRUE, format="{point.y:,.0f}"))),
      chart = list(backgroundColor = "#fff")
    )
  )
}

#' @noRd
leaf_basemap <- function(
  bbox,
  baseGroups = c("Light", "OSM", "OSM HOT"),
  overlayGroups = c("Boundaries"),
  miniMap = TRUE
) {
  lf <- leaflet::leaflet() %>%
    leaflet::fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]]) %>%
    leaflet::addMapPane(name = "choropleth", zIndex = 310) %>%
    leaflet::addMapPane(name = "place_labels", zIndex = 320) %>%
    leaflet::addMapPane(name = "circles", zIndex = 410) %>%
    leaflet::addMapPane(name = "boundaries", zIndex = 420) %>%
    leaflet::addMapPane(name = "geo_highlight", zIndex = 430) %>%
    leaflet::addProviderTiles("CartoDB.PositronNoLabels", group = "Light") %>%
    leaflet::addProviderTiles(
      "CartoDB.PositronOnlyLabels",
      group = "Light",
      options = leaflet::leafletOptions(pane = "place_labels")
    ) %>%
    leaflet::addProviderTiles("OpenStreetMap", group = "OSM") %>%
    leaflet::addProviderTiles("OpenStreetMap.HOT", group = "OSM HOT") %>%
    leaflet::addScaleBar(
      position = "bottomright",
      options = leaflet::scaleBarOptions(imperial = FALSE)
    ) %>%
    leaflet::addLayersControl(
      baseGroups = baseGroups,
      overlayGroups = overlayGroups,
      position = "topleft"
    )

  if (miniMap) {
    lf <- lf %>% leaflet::addMiniMap(toggleDisplay = TRUE, position = "bottomleft")
  }

  return(lf)
}

#' Generate HTML Tooltip for Leaf Nodes
#'
#' This function creates an HTML tooltip for leaf nodes in a Shiny application.
#' The tooltip displays information about the name, number of patients, population,
#' and attack rate, if available.
#'
#' @param df A data frame containing the data.
#' @param name_col A string specifying the column name for the names (default is "name").
#' @param n_col A string specifying the column name for the counts (default is "total").
#' @param n_lab A string specifying the label for the counts (default is "N patients").
#' @param pop_col A string specifying the column name for the population (default is NULL).
#' @param pop_lab A string specifying the label for the population (default is "Population").
#' @param ar_col A string specifying the column name for the attack rate (default is NULL).
#' @param ar_lab A string specifying the label for the attack rate (default is "Attack rate").
#'
#' @return A list of HTML elements to be used as tooltips in a Shiny application.
#' @importFrom scales number
#' @importFrom glue glue
#' @importFrom purrr map
#' @importFrom shiny HTML
#' @noRd
#'
#' @examples
#' df <- data.frame(
#'   name = c("Location A", "Location B"),
#'   total = c(100, NA),
#'   population = c(1000, 2000),
#'   attack_rate = c(10, NA)
#' )
#' make_leaf_tooltip(df, pop_col = "population", ar_col = "attack_rate")
make_leaf_tooltip <- function(
  df,
  name_col = "name",
  n_col = "total",
  n_lab = "N patients",
  pop_col = NULL,
  pop_lab = "Population",
  ar_col = NULL,
  ar_lab = "Attack rate"
) {
  counts <- ifelse(is.na(df[[n_col]]), "No data", scales::number(df[[n_col]], accuracy = 1))
  if (all(!is.null(pop_col), !is.null(ar_col))) {
    pop <- ifelse(is.na(df[[pop_col]]), "No data", scales::number(df[[pop_col]], accuracy = 1))
    ar <- ifelse(is.na(df[[ar_col]]), "No data", scales::number(df[[ar_col]], accuracy = .1))
    glue::glue(
      "<b>{df[[name_col]]}</b><br>
       {n_lab}: <b>{counts}</b><br>
       {pop_lab}: <b>{pop}</b><br>
       {ar_lab}: <b>{ar}</b> / 100 000<br>"
    ) %>%
      purrr::map(shiny::HTML)
  } else {
    glue::glue(
      "<b>{df[[name_col]]}</b><br>
       {n_lab}: <b>{counts}</b><br>"
    ) %>%
      purrr::map(shiny::HTML)
  }
}

#' Get Label for Selected Choice
#'
#' This function retrieves the label for a selected choice from a list of choices.
#'
#' @param selected The selected choice for which the label is to be retrieved.
#' @param choices A named vector of choices from which the label is to be retrieved.
#' @param .default The default label to be used if the selected choice is not found in the choices. Defaults to the value of the "epishiny.count.label" option, or "N" if the option is not set.
#'
#' @return The label corresponding to the selected choice if found in the choices, otherwise the default label.
#'
#' @examples
#' choices <- c(a = "apple", b = "banana", c = "cherry")
#' get_label("b", choices) # Returns "banana"
#' get_label("d", choices) # Returns "N" (default)
#'
#' @noRd
get_label <- function(selected, choices, .default = getOption("epishiny.count.label", "N")) {
  if (length(choices)) {
    lab <- choices[choices == selected]
    ifelse(rlang::is_named(lab), names(lab), lab)
  } else {
    .default
  }
}

#' @noRd
format_filter_info <- function(fi = NULL, tf = NULL, pf = NULL) {
  if (length(tf)) {
    if (length(fi)) {
      # since we already have a period filter value from the date input, replace it with bar click period
      fi <- stringr::str_replace(
        fi,
        "\\d{2}/[A-Za-z]{3}/\\d{2} - \\d{2}/[A-Za-z]{3}/\\d{2}",
        tf$lab
      )
    } else {
      fi <- paste("<b>Filters applied</b></br>Period:", tf$lab)
    }
  }
  if (length(pf)) {
    pf_lab <- glue::glue("{pf$level_name}: {pf$region_name}")
    if (length(fi)) {
      fi <- glue::glue("{fi}</br>{pf_lab}")
    } else {
      fi <- paste0("<b>Filters applied</b></br>", pf_lab)
    }
  }
  fi
}
