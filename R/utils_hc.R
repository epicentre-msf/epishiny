

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Setting default highcharter options")

  fntfmly <- '"Roboto Mono",-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,"Helvetica Neue",Arial,sans-serif,"Apple Color Emoji","Segoe UI Emoji","Segoe UI Symbol";'

  hc_opts <- getOption("highcharter.chart")

  hc_opts$colors <- scales::alpha(epi_pals()$d310, .8)

  hc_opts$plotOptions$column <- list(
    zIndex = 2,
    stacking = "normal",
    groupPadding = 0.05,
    pointPadding = 0.05,
    borderWidth = 1,
    borderColor = "white"
  )

  hc_opts$plotOptions$bar <- list(
    zIndex = 2,
    stacking = "normal",
    groupPadding = 0.05,
    pointPadding = 0.05,
    borderWidth = 0.05
  )

  hc_opts$plotOptions$line <- list(
    zIndex = 10,
    marker = list(enabled = TRUE)
  )

  hc_opts$credits <- list(
    enabled = FALSE,
    href = "",
    style = list(fontFamily = fntfmly, fontSize = "10px", fontStyle = "italic", cursor = "default")
  )

  hc_opts$caption <- list(
    enabled = FALSE,
    href = "",
    style = list(fontFamily = fntfmly, fontSize = "10px", cursor = "default")
  )

  hc_opts$exporting <- list(enabled = FALSE)

  options(
    highcharter.chart = hc_opts,
    highcharter.theme = highcharter::hc_theme_smpl(
      chart = list(style = list(fontFamily = fntfmly)),
      title = list(style = list(fontFamily = fntfmly)),
      subtitle = list(style = list(fontFamily = fntfmly))
    )
  )
}

hc_week_labels <- function() {
  htmlwidgets::JS(
    "function () {
         var date = new Date(this.value);
         var year = date.getWeekYear();
         var week = date.getWeek();
         return year + '-W' + week;
     }"
  )
}

# hc_download_btns <- c(
#   "viewFullscreen",
#   "separator",
#   "downloadPNG",
#   "downloadJPEG",
#   "downloadSVG",
#   "separator",
#   "downloadCSV",
#   "downloadXLS"
# )

#' @export
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
  show_credits <- ifelse(is.null(credits), FALSE, TRUE)
  show_caption <- ifelse(is.null(caption), FALSE, TRUE)

  legend_title <- stringr::str_remove(hc$x$hc_opts$legend$title$text, "\\(click to filter\\)")

  highcharter::hc_exporting(
    hc,
    enabled = TRUE,
    sourceWidth = width,
    sourceHeight = height,
    buttons = list(contextButton = list(menuItems = dl_buttons, text = dl_text)),
    filename = paste0(filename, Sys.Date()),
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

# #' @export
# my_hc_export <- function(
#   hc,
#   title,
#   subtitle,
#   caption,
#   width = 600,
#   height = 350,
#   dl_buttons = c("viewFullscreen", "separator", "downloadPNG", "downloadJPEG", "downloadSVG", "separator", "downloadCSV", "downloadXLS"),
#   palette = c("steelblue", "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666"),
#   dl_text = "",
#   filename = "EPI-FIGURE-"
# ) {
#   #title <- ifelse(is.null(hc$x$hc_opts$title$text), title, hc$x$hc_opts$title$text)
#   #subtitle <- ifelse(is.null(hc$x$hc_opts$subtitle$text), subtitle, hc$x$hc_opts$subtitle$text)
#   title <- ifelse(missing(title), hc$x$hc_opts$title$text, title)
#   subtitle <- ifelse(missing(subtitle), hc$x$hc_opts$subtitle$text, subtitle)
#   legend_title <- stringr::str_remove(hc$x$hc_opts$legend$title$text, "\\(Click to hide\\)")
#   credits <- ifelse(is.null(hc$x$hc_opts$credits$text), "Graphique: Epicentre", hc$x$hc_opts$credits$text)
#   # credits <- paste0("Graphique: Epicentre | Source: MDO")
#
#   highcharter::hc_exporting(
#     hc,
#     enabled = TRUE,
#     sourceWidth = width,
#     sourceHeight = height,
#     buttons = list(contextButton = list(menuItems = dl_buttons, text = dl_text)),
#     filename = paste0(filename, Sys.Date()),
#     csv = list(dateFormat = "%d/%m/%Y"),
#     tableCaption = "",
#     useMultiLevelHeaders = FALSE,
#     formAttributes = list(target = "_blank"),
#     chartOptions = list(
#       title = list(text = title),
#       subtitle = list(text = subtitle),
#       credits = list(enabled = TRUE, text = credits),
#       legend = list(title = list(text = legend_title)),
#       xAxis = list(plotBands = list()), # remove plotbands
#       rangeSelector = list(enabled = FALSE),
#       navigator = list(enabled = FALSE),
#       colors = palette,
#       # plotOptions = list(series = list(dataLabels = list(enabled = TRUE, format="{point.y:,.0f}"))),
#       chart = list(
#         backgroundColor = "#fff"
#         # events = list(
#         #   load = JS(paste0("function () {
#         #     this.renderer.image(
#         #     'https://epicentre.msf.org/sites/default/files/logo_Epicentre_1.png',", width - 176, ", -5, 176, 45)
#         #     .add();
#         #   }
#         #   "))
#         # )
#       )
#     )
#   )
# }
