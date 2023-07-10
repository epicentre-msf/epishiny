.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    "epishiny",
    system.file("assets", package = "epishiny")
  )
}

.onAttach <- function(libname, pkgname) {
  # set epishiny default options =============
  options(
    epishiny.na.label = "(Missing)",
    epishiny.week.initial = "W",
    epishiny.week.start = 1
  )

  # default highcharter theming options =============
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