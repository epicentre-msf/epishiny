#' Launch a single epishiny module as a standalone shiny app
#'
#' Use this function to quickly launch any of the 3 epishiny interactive
#' visualisation modules (time, place, person) independently, allowing for
#' incorporation into exploratory data analysis workflows in R.
#'
#' @param module name of the module to launch. Current options are
#'  'epicurve', 'map' or 'pyramid'
#' @param ... other named arguments passed to the relevant module
#'  UI and Server functions. See each module's documenation for details
#'  of the arguments required.
#'
#' @return No return value, a shiny app is launched.
#' @example inst/examples/docs/launch-module.R
#' @export
launch_module <- function(module = c("time", "place", "person"), ...) {

  # library(highcharter)
  # # default highcharter theming options =============
  # fntfmly <- '"Roboto Mono",-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,"Helvetica Neue",Arial,sans-serif,"Apple Color Emoji","Segoe UI Emoji","Segoe UI Symbol";'
  #
  # hc_opts <- getOption("highcharter.chart")
  #
  # hc_opts$colors <- scales::alpha(epishiny:::epi_pals()$d310, .8)
  #
  # hc_opts$plotOptions$column <- list(
  #   zIndex = 2,
  #   stacking = "normal",
  #   groupPadding = 0.05,
  #   pointPadding = 0.05,
  #   borderWidth = 1,
  #   borderColor = "white"
  # )
  #
  # hc_opts$plotOptions$bar <- list(
  #   zIndex = 2,
  #   stacking = "normal",
  #   groupPadding = 0.05,
  #   pointPadding = 0.05,
  #   borderWidth = 0.05
  # )
  #
  # hc_opts$plotOptions$line <- list(
  #   zIndex = 10,
  #   marker = list(enabled = TRUE)
  # )
  #
  # hc_opts$credits <- list(
  #   enabled = FALSE,
  #   href = "",
  #   style = list(fontFamily = fntfmly, fontSize = "10px", fontStyle = "italic", cursor = "default")
  # )
  #
  # hc_opts$caption <- list(
  #   enabled = FALSE,
  #   href = "",
  #   style = list(fontFamily = fntfmly, fontSize = "10px", cursor = "default")
  # )
  #
  # hc_opts$exporting <- list(enabled = FALSE)
  #
  # options(
  #   highcharter.chart = hc_opts,
  #   highcharter.theme = highcharter::hc_theme_smpl(
  #     chart = list(style = list(fontFamily = fntfmly)),
  #     title = list(style = list(fontFamily = fntfmly)),
  #     subtitle = list(style = list(fontFamily = fntfmly))
  #   )
  # )

  module <- match.arg(module, c("time", "place", "person"), several.ok = FALSE)
  mod_ui <- paste0(module, "_ui")
  mod_server <- paste0(module, "_server")
  # prepare arguments for ui and server
  args = tibble::lst(
    id = "epimod",
    full_screen = FALSE,
    ...
  )
  ui_args <- match.arg(
    names(args),
    names(as.list(args(mod_ui))),
    several.ok = TRUE
  )
  server_args <- match.arg(
    names(args),
    names(as.list(args(mod_server))),
    several.ok = TRUE
  )
  ui <- bslib::page_fillable(
    use_epishiny(),
    do.call(mod_ui, args[ui_args]),
    waiter::waiter_preloader(html = waiter::spin_3())
  )
  server <- function(input, output, session) {
    do.call(mod_server, args[server_args])
  }

  slb <- getOption("shiny.launch.browser")
  on.exit(options(shiny.launch.browser = slb))
  if (
    # Make sure that {rstudioapi} is available
    requireNamespace("rstudioapi", quietly = TRUE) &&
    # Returns TRUE if RStudio is running
    rstudioapi::hasFun("viewer")
  ) {
    options(shiny.launch.browser = .rs.invokeShinyPaneViewer)
  }

  shiny::shinyApp(ui, server)
}

#' Launch epishiny demo dashboard
#'
#' See an example of the type of dashboard you can build
#' using `epishiny` modules within a `bslib` UI.
#'
#' @return No return value, a shiny app is launched.
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'   library(epishiny)
#'   launch_demo_dashboard()
#' }
#' @export
launch_demo_dashboard <- function() {
  app_dir <- system.file("examples", "demo", package = "epishiny")
  if (app_dir == "") {
    stop("Could not find example directory. Try re-installing `epishiny`.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
