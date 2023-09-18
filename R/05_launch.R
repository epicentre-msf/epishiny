#' Launch a single epishiny module as a standalone shiny app
#'
#' Use this function to quickly launch any of the 3 epishiny interactive
#' visualisation modules (time, place, person) independently, allowing for
#' incorporation into exploratory data analysis workflows in R.
#'
#' @param module name of the module to launch. Current options are
#'  'time', 'place' or 'person'
#' @param ... other named arguments passed to the relevant module
#'  UI and Server functions. See each module's documenation for details
#'  of the arguments required.
#'
#' @return No return value, a shiny app is launched.
#' @example inst/examples/docs/launch-module.R
#' @export
launch_module <- function(module = c("time", "place", "person"), ...) {

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
    # Check if inside Rstudio
    Sys.getenv("RSTUDIO") == "1" &&
    # Make sure that {rstudioapi} is available
    requireNamespace("rstudioapi", quietly = TRUE) &&
    # Returns TRUE if RStudio is running
    rstudioapi::hasFun("viewer")
  ) {
    options(shiny.launch.browser = .rs.invokeShinyPaneViewer)
  } else {
    options(shiny.launch.browser = TRUE)
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
