#' @keywords internal
#' @noRd
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

#' @keywords internal
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
  show_credits <- ifelse(length(credits), FALSE, TRUE)
  show_caption <- ifelse(length(caption), FALSE, TRUE)

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
