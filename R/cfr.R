#' CFR ui
#' @rdname cfr
#' @param id The id.
#'
#' @return
#' @export
#'
#' @examples
cfr_ui <- function(id, full_screen = TRUE) {
  # parameter tabs
  parameter_tabs <- tabsetPanel(
    id = NS(id, "params"),
    type = "hidden",
    tabPanel(
      "Normal",
      numericInput(NS(id, "mean"), "mean", value = 1),
      numericInput(NS(id, "sd"), "standard deviation", min = 0, value = 1)
    ),
    tabPanel(
      "Gamma",
      numericInput(NS(id, "shape"), "shape", value = 5),
      numericInput(NS(id, "scale"), "scale", value = 1)
    ),
    tabPanel(
      "Log-normal",
      numericInput(NS(id, "meanlog"), "meanlog", value = 1, min = 0),
      numericInput(NS(id, "sdlog"), "sdlog", value = 1, min = 0)
    )
  )

  # cfr options panel
  cfr_options_tabs <- tabsetPanel(
    id = NS(id, "cfr_options"),
    type = "hidden",
    tabPanel(
      "rolling",
      numericInput(
        NS(id, "poisson_threshold"), "poisson_threshold",
        value = 100, min = 1
      )
    ),
    tabPanel(
      "time_varying",
      numericInput(
        NS(id, "burn_in"), "burn_in",
        value = 7, min = 0
      ),
      numericInput(
        NS(id, "smoothing_window"), "smoothing_window",
        value = 1, min = 1
      )
    )
  )

  tagList(
    use_epishiny(),
    bslib::card(
      full_screen = full_screen,
      bslib::card_header(
        class = "d-flex justify-content-start align-items-center",
        tags$span(bsicons::bs_icon("bar-chart-line-fill"), "CFR", class = "pe-2"),

        # options button and dropdown menu
        bslib::popover(
          title = tags$span(shiny::icon("sliders"), "Options"),
          trigger = actionButton(
            NS(id, "dropdown"),
            icon = shiny::icon("sliders"),
            label = "Options",
            class = "btn-sm pe-2 me-2"
          ),
          selectInput(
            NS(id, "type"), "Estimate type",
            choices = list(
              Rolling = "rolling", `Time-varying` = "time_varying"
            )
          ),
          cfr_options_tabs,
          checkboxInput(
            NS(id, "correct_delays"), "Correct for delays?"
          ),
          conditionalPanel(
            ns = NS(id),
            condition = "input.correct_delays",
            selectInput(
              NS(id, "dist"), "Distribution",
              choices = c("Normal", "Gamma", "Log-normal")
            ),
            parameter_tabs
          )
        )
      ),
      bslib::card_body(
        class = "d-flex justify-content-start align-items-center",
        padding = 0,
        tableOutput(NS(id, "cfr_overall")),
        highcharter::highchartOutput(NS(id, "cfr_plot")),
        conditionalPanel(
          ns = NS(id),
          "input.correct_delays",
          div(
            style = "display:flex;",
            plotly::plotlyOutput(NS(id, "plot_pmf"), width = "25vw", height = "25vw"),
            plotly::plotlyOutput(NS(id, "plot_cdf"), width = "25vw", height = "25vw")
          )
        )
      )
    )
  )
}

#' CFR server
#'
#' @rdname cfr
#' @param id The id.
#' @param df The data.
#'
#' @return
#' @export
#'
#' @examples
cfr_server <- function(id, df) {
  moduleServer(
    id, function(input, output, session) {
      observeEvent(input$type, {
        updateTabsetPanel(inputId = "cfr_options", selected = input$type)
      })

      # update distribution parameters per user choice
      observeEvent(input$dist, {
        updateTabsetPanel(inputId = "params", selected = input$dist)
      })

      pmf_fn <- reactive(
        switch(input$dist,
          Normal = dnorm,
          Gamma = dgamma,
          `Log-normal` = dlnorm
        )
      )
      cdf_fn <- reactive(
        switch(input$dist,
          Normal = pnorm,
          Gamma = pgamma,
          `Log-normal` = plnorm
        )
      )
      args <- reactive(
        switch(input$dist,
          Normal = list(
            mean = input$mean, sd = input$sd
          ),
          Gamma = list(
            shape = input$shape, rate = 1 / input$scale
          ),
          `Log-normal` = list(
            meanlog = input$meanlog, sdlog = input$sdlog
          )
        )
      )

      # create PMF plot
      plot_pmf <- reactive(
        ggplot() +
          stat_function(
            fun = pmf_fn(),
            args = args(),
            fill = "steelblue",
            geom = "area", colour = NA
          ) +
          labs(
            x = "Days after symptom onset",
            y = "Prob. density (death)"
          ) +
          theme_classic() +
          xlim(0, 21) + # assume 3 weeks
          coord_cartesian(expand = FALSE)
      )

      # create CDF plot
      plot_cdf <- reactive(
        ggplot() +
          stat_function(
            fun = cdf_fn(),
            args = args(),
            fill = "steelblue",
            geom = "area", colour = NA
          ) +
          labs(
            x = "Days after symptom onset",
            y = "Cumulative density (death)"
          ) +
          theme_classic() +
          xlim(0, 21) + # assume 3 weeks
          ylim(0, 1) +
          coord_cartesian(expand = FALSE)
      )

      # pass to output ui
      output$plot_pmf <- renderPlotly(ggplotly(plot_pmf()))
      output$plot_cdf <- renderPlotly(ggplotly(plot_cdf()))

      # prepare ddens
      ddens <- reactive(
        switch(input$correct_delays,
          `TRUE` = function(x) {
            do.call(pmf_fn(), c(list(x = x), args()))
          },
          `FALSE` = NULL
        )
      )

      # prepare data by forcing it to reactive
      df_mod <- reactive({
        force_reactive(df)
      })

      # estimate CFR
      cfr_estimate <- reactive(
        switch(input$type,
          rolling = cfr::cfr_rolling(
            data = df,
            delay_density = ddens(),
            poisson_threshold = input$poisson_threshold
          ),
          time_varying = cfr::cfr_time_varying(
            data = df,
            delay_density = ddens(),
            burn_in = input$burn_in,
            smoothing_window = input$smoothing_window
          )
        )
      )

      # create plot
      cfr_plot_hc <- reactive(
        highcharter::hchart(
          cfr_estimate(),
          type = "line",
          highcharter::hcaes(date, severity_mean),
          id = "cfr_plot",
          name = "CFR estimate", color = "red"
        ) %>%
          highcharter::hc_add_series(
            data = cfr_estimate()
          ) %>%
          highcharter::hc_tooltip(shared = TRUE)
      )

      # pass plot and df to output ui
      output$cfr_plot <- highcharter::renderHighchart(cfr_plot_hc())

      # get static overall estimate
      cfr_overall <- reactive(
        cfr::cfr_static(
          data = df,
          delay_density = ddens(),
          poisson_threshold = 100 # NOTE: fixed
        )
      )

      output$cfr_overall <- renderTable(cfr_overall())
    }
  )
}

cfr_app <- function() {
  ui <- fluidPage(
    cfr_ui("cfr")
  )
  server <- function(input, output, session) {
    cfr_server("cfr")
  }
  shinyApp(ui, server)
}
