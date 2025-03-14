#' Delay Module UI
#'
#' Creates the UI components for the delay visualization module, including
#' distribution of delays, and mean/median time between key events.
#'
#' @rdname delay
#'
#' @param id Module ID, must be the same in both the UI and server function to link the two.
#' @param title Title of the module (default: "Delays").
#' @param icon A Bootstrap icon for the module header (default: hourglass-split).
#' @param opts_btn_lab Label for the options button (default: "Options").
#' @param full_screen Logical, whether to allow fullscreen mode (default: TRUE).
#'
#' @return A Shiny UI object containing navigation panels for a bar chart and timeline visualization.
#'
#' @importFrom shiny NS selectInput sliderInput checkboxInput conditionalPanel radioButtons actionButton updateSelectInput
#' @importFrom bslib navset_card_tab nav_panel card_body popover
#' @importFrom highcharter highchartOutput
#' @importFrom bsicons bs_icon
#' @importFrom shinyWidgets checkboxGroupButtons
#'
#' @export
delay_ui <- function(
  id,
  title = "Delays",
  icon = bsicons::bs_icon("hourglass-split"),
  opts_btn_lab = "Options",
  full_screen = TRUE
) {
  ns <- shiny::NS(id)

  pkg_deps <- c("highcharter", "bslib", "shinyWidgets")
  if (!rlang::is_installed(pkg_deps)) {
    rlang::check_installed(pkg_deps, reason = "to use the delay module.")
  }

  bslib::navset_card_tab(
    wrapper = function(...) {
      bslib::card_body(..., padding = 0, class = "delay-container")
    },
    full_screen = full_screen,
    id = ns("tabs"),

    title = tags$div(
      class = "d-flex justify-content-start align-items-center",
      tags$span(icon, title, class = "pe-2"),
      bslib::popover(
        trigger = actionButton(
          ns("dropdown"),
          icon = shiny::icon("sliders"),
          label = opts_btn_lab,
          class = "btn-sm btn-light"
        ),
        selectInput(
          ns("group_var"),
          label = "Select grouping variable",
          choices = NULL
        ),
        sliderInput(
          ns("co_value"),
          label = "Maximum delay",
          min = 0,
          max = 30,
          value = 30,
          step = 1
        ),
        div(
          id = ns("bar_inputs"),
          selectInput(
            ns("delays"),
            label = "Select delay",
            choices = NULL
          ),
          shinyWidgets::checkboxGroupButtons(
            inputId = ns("show_stat"),
            size = "xs",
            label = "Select statistics to display:",
            choices = c("Mean" = "mean", "Median" = "median"),
            selected = c("mean", "median")
          ),
          checkboxInput(
            ns("fit_dist"),
            label = "Fit Distribution",
            value = TRUE
          ),
          conditionalPanel(
            condition = sprintf("input['%s']", ns("fit_dist")),
            selectInput(
              ns("which_dist"),
              label = "Select Distribution",
              choices = c("gamma", "weibull", "lnorm"),
              selected = "gamma",
              multiple = TRUE
            )
          )
        ),

        div(
          id = ns("timeline_inputs"),
          selectInput(
            ns("dates"),
            label = "Select events",
            choices = NULL,
            multiple = TRUE
          ),

          radioButtons(
            inputId = ns("mean_median"),
            label = "Select Statistic",
            choices = c("Mean" = "mean", "Median" = "median"),
            selected = "mean",
            inline = TRUE
          ),
          checkboxInput(
            ns("display_lab"),
            label = "Display labels",
            value = TRUE
          )
        )
      )
    ),

    bslib::nav_panel(
      title = shiny::icon("bar-chart"),
      value = "bar_nav",
      highcharter::highchartOutput(ns("delay_barchart"), height = "100%")
    ),

    bslib::nav_panel(
      title = shiny::icon("clock"),
      value = "timeline_nav",
      highcharter::highchartOutput(ns("delay_timeline"))
    )
  )
}

#' Delay Module Server
#'
#' Manages the server-side logic for the delay visualization module, handling
#' user interactions, data transformations, and plotting.
#'
#' @param id  Module ID, must be the same in both the UI and server function to link the two.
#' @param linelist Linelist containing the event data.
#' @param date_vars A vector of date-related variable names for event selection, in the chronological order. If named vector then names are used in selection/display
#' @param group_vars A vector of categorical variables available for grouping.
#'
#' @return A Shiny module server function that generates reactive plots and updates UI elements.
#'
#' @importFrom shiny moduleServer observeEvent updateSelectInput reactive req renderHighchart
#' @importFrom highcharter renderHighchart
#' @importFrom shinyjs toggle
#'
#' @export
delay_server <- function(id, linelist, date_vars, group_vars) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      observe({
        shinyjs::toggle("bar_inputs", condition = input$tabs == "bar_nav")
      })

      observe({
        shinyjs::toggle(
          "timeline_inputs",
          condition = input$tabs == "timeline_nav"
        )
      })

      # names of all delays
      delay_choices <- reactive({
        delay_df() |> select(contains("__")) |> names()
      })

      # Update delay choices
      observeEvent(delay_df(), {
        updateSelectInput(
          session,
          "delays",
          choices = delay_choices()
        )
      })

      # Update group variables choices
      observeEvent(group_vars, {
        updateSelectInput(
          session,
          "group_var",
          choices = c("None" = "none", group_vars),
          selected = "none"
        )
      })

      group_var <- reactive({
        if (input$group_var == "none") NULL else input$group_var
      })

      # calculate delays
      delay_df <- reactive({
        req(input$group_var)
        get_delay_df(linelist, date_vars, group_var = group_var())
      })

      # Plot barchart
      output$delay_barchart <- renderHighchart({
        req(input$delays)

        plot_delay_bar(
          delay_df(),
          co_value = input$co_value,
          input$delays,
          fit_dist = input$fit_dist,
          which_dist = input$which_dist,
          group_var = group_var(),
          show_stat = input$show_stat
        )
      })

      # update dates choices
      observeEvent(date_vars, {
        updateSelectInput(
          session,
          "dates",
          choices = date_vars,
          selected = date_vars
        )
      })

      # Reactive to map the selected value to the corresponding label
      selected_label <- reactive({
        names(date_vars[date_vars %in% input$dates]) # Map selected value to its label
      })

      choice_dates <- reactive(setNames(input$dates, selected_label()))

      # Plot Timeline
      output$delay_timeline <- renderHighchart({
        plot_delay_timeline(
          delay_df = delay_df(),
          statistic = input$mean_median,
          date_var_seq = choice_dates(),
          co_value = input$co_value,
          display_lab = input$display_lab,
          group_var = group_var()
        )
      })
    }
  )
}
