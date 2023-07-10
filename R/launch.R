
#' @export
launch_module <- function(module = c("epicurve", "map", "pyramid"), ...) {
    module <- match.arg(module, c("epicurve", "map", "pyramid"), several.ok = FALSE)
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
      tags$head(use_epishiny()),
      do.call(mod_ui, args[ui_args]),
      waiter::waiter_preloader(html = waiter::spin_3())
    )
    server <- function(input, output, session) {
      do.call(mod_server, args[server_args])
    }
    shiny::shinyApp(ui, server, options = list("launch.browser" = TRUE))
}
