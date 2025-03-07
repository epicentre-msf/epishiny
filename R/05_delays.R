# UI ----------------------------------------------------------------------
delay_ui <- function(
  id,
  title = "Delays",
  icon = bsicons::bs_icon("clock-history"),
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
      tags$span(icon, title, class = "pe-2")
    ),

    # Tab for Barchart
    bslib::nav_panel(
      title = shiny::icon("bar-chart"),
      tags$div(
        class = "d-flex justify-content-between align-items-center",
        selectInput(
          ns("delays"),
          label = "Select delay",
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

      highcharter::highchartOutput(ns("delay_barchart"), height = "100%")
    ),

    # Tab for Timeline
    bslib::nav_panel(
      title = shiny::icon("clock"),
      tags$div(
        class = "d-flex justify-content-between align-items-center",

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
        sliderInput(
          ns("co_value"),
          label = "Maximum delay",
          min = 0,
          max = 30,
          value = 30,
          step = 1
        ),
        checkboxInput(
          ns("display_lab"),
          label = "Display labels",
          value = TRUE
        ),
      ),
      highcharter::highchartOutput(ns("delay_timeline"))
    )
  )
}

# SERVER ------------------------------------------------------------------
# Define Server
delay_server <- function(id, linelist, date_vars) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # calculate delays
      delay_df <- reactive({
        get_delay_df(linelist, date_vars)
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

      # Plot barchart
      output$delay_barchart <- renderHighchart({
        req(input$delays)

        plot_delay_bar(
          delay_df(),
          co_value = input$co_value,
          input$delays,
          fit_dist = input$fit_dist,
          which_dist = input$which_dist,
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
          group_var = NULL,
          color_pal = c("#FFEDA0", "#FEB24C", "#F03B20"),
          display_lab = input$display_lab
        )
      })
    }
  )
}
