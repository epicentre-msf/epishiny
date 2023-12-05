cfr_ui <- function(id) {
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
    sidebarLayout(
      sidebarPanel(
        useShinyjs(),
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
      ),
      mainPanel(
        tableOutput(NS(id, "cfr_overall")),
        plotlyOutput(NS(id, "cfr_plot")),
        conditionalPanel(
          ns = NS(id),
          "input.correct_delays",
          div(
            style = "display:flex;",
            plotlyOutput(NS(id, "plot_pmf"), width = "25vw", height = "25vw"),
            plotlyOutput(NS(id, "plot_cdf"), width = "25vw", height = "25vw")
          )
        )
      )
    )
  )
}

cfr_server <- function(id) {
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
      # estimate CFR
      cfr_estimate <- reactive(
        switch(input$type,
          rolling = cfr::cfr_rolling(
            data = cfr::ebola1976,
            delay_density = ddens(),
            poisson_threshold = input$poisson_threshold
          ),
          time_varying = cfr::cfr_time_varying(
            data = cfr::ebola1976,
            delay_density = ddens(),
            burn_in = input$burn_in,
            smoothing_window = input$smoothing_window
          )
        )
      )

      # create plot
      cfr_plot <- reactive(
        ggplot(cfr_estimate()) +
          geom_ribbon(
            aes(
              date,
              ymin = severity_low, ymax = severity_high
            ),
            fill = alpha("pink", 0.2)
          ) +
          geom_line(
            aes(date, severity_mean),
            colour = "darkred"
          ) +
          scale_x_date(
            date_labels = "%b-%Y"
          ) +
          scale_y_continuous(
            labels = scales::label_percent()
          ) +
          labs(
            x = NULL,
            y = "CFR (%)"
          ) +
          theme_classic()
      )

      # pass plot and df to output ui
      output$cfr_plot <- renderPlotly(ggplotly(cfr_plot()))

      # get static overall estimate
      cfr_overall <- reactive(
        cfr::cfr_static(
          data = cfr::ebola1976,
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
