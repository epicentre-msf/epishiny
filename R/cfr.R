#' CFR module
#'
#' @description Visualise the rolling or time-varying disease severity in the
#' form of the case fatality risk, while optionally correcting for delays in
#' reporting outcomes (deaths).
#'
#' @name cfr
#' @rdname cfr
#' @inheritParams time_ui
#'
#' @param df A `<data.frame>` of daily cases and deaths to be passed to
#' `launch_module()`.
#' Must include columns suitable for conversion to
#' `date`, `cases`, and `deaths`, which specify the
#' daily cases and deaths reported during the outbreak.
#' The CFR module currently only supports aggregated incidence data and does
#' not support grouping variables.
#' Dates must be a continuous series and no values may be missing or `NA`.
#' See `cfr::cfr_rolling()` or `cfr::cfr_time_varying()` for more details on
#' the CFR functions.
#' @param date_var A string for the date column; defaults to `"date"`.
#' @param cases_var A string for the column of cases reported; defaults to
#' `"cases"`.
#' @param deaths_var A string for the column of deaths reported; defaults to
#' `"deaths"`.
#'
#' @return Creates a Shiny module to be launched by `launch_module()`.
#' @import shiny
#' @importFrom dplyr .data
#' @export
cfr_ui <- function(id,
                   title = "CFR estimate",
                   icon = bsicons::bs_icon("graph-up"),
                   opts_btn_lab = "options",
                   full_screen = TRUE) {
  # parameter tabs
  parameter_tabs <- tabsetPanel(
    id = NS(id, "params"),
    type = "hidden",
    tabPanel(
      "normal",
      numericInput(NS(id, "mean"), "mean", value = 1),
      numericInput(NS(id, "sd"), "standard deviation", min = 0, value = 1)
    ),
    tabPanel(
      "gamma",
      numericInput(NS(id, "shape"), "shape", value = 5),
      numericInput(NS(id, "scale"), "scale", value = 1)
    ),
    tabPanel(
      "lognormal",
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
        # title
        tags$span(icon, title, class = "pe-2"),
        # options button and dropdown menu
        bslib::popover(
          title = tags$span(icon("sliders"), opts_btn_lab),
          trigger = actionButton(
            NS(id, "dropdown"),
            icon = icon("sliders"),
            label = opts_btn_lab,
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
              choices = list(
                Normal = "normal",
                Gamma = "gamma",
                `Log-normal` = "lognormal"
              )
            ),
            parameter_tabs
          ),
          actionButton(
            inputId = NS(id, "go"),
            label = "Get estimate",
            icon = icon("refresh"),
            class = "btn-sm btn-primary",
            width = "100%"
          )
        )
      ),
      bslib::card_body(
        padding = 0,
        highcharter::highchartOutput(NS(id, "cfr_plot"))
      )
    )
  )
}

#' CFR server
#'
#' @name cfr
#' @rdname cfr
#' @export
cfr_server <- function(id,
                       df,
                       date_var = "date",
                       cases_var = "cases",
                       deaths_var = "deaths") {
  moduleServer(
    id, function(input, output, session) {
      # rename df columns and select columns
      df <- dplyr::select(
        df,
        date = date_var,
        cases = cases_var,
        deaths = deaths_var
      )

      # updaet panel show per CFR choice
      observeEvent(input$type, {
        updateTabsetPanel(inputId = "cfr_options", selected = input$type)
      })

      # update distribution parameters per user choice
      observeEvent(input$dist, {
        updateTabsetPanel(inputId = "params", selected = input$dist)
      })

      # prepare PMF functions
      pmf_fn <- reactive(
        switch(input$dist,
          normal = stats::dnorm,
          gamma = stats::dgamma,
          lognormal = stats::dlnorm
        )
      )

      # distribution arguments
      args <- reactive(
        switch(input$dist,
          normal = list(
            mean = input$mean, sd = input$sd
          ),
          gamma = list(
            shape = input$shape, rate = 1 / input$scale
          ),
          lognormal = list(
            meanlog = input$meanlog, sdlog = input$sdlog
          )
        )
      )

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
      cfr_estimate <- bindEvent(
        x = reactive(
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
        ),
        input$go,
        ignoreNULL = FALSE
      )

      # get static estimate for plot title
      cfr_estimate_static <- bindEvent(
        x = reactive(
          cfr::cfr_static(
            data = df,
            delay_density = ddens(),
            poisson_threshold = input$poisson_threshold
          )
        ),
        input$go,
        ignoreNULL = FALSE
      )

      # get flag for whether correction is applied
      correction_applied <- bindEvent(
        x = reactive(
          !is.null(ddens())
        ),
        input$go,
        ignoreNULL = FALSE
      )

      # plot line label
      estimate_type <- reactive(
        switch(input$type,
          rolling = "Rolling",
          time_varying = "Time-varying"
        )
      )
      estimate_label <- reactive(glue::glue("{estimate_type()} CFR estimate"))

      # create plot
      cfr_plot_hc <-
        bindEvent(
          x = reactive(
            highcharter::highchart() %>%
              highcharter::hc_add_series(
                type = "arearange",
                data = cfr_estimate(),
                highcharter::hcaes(
                  .data$date,
                  low = .data$severity_low,
                  high = .data$severity_high
                ),
                name = "95% confidence interval",
                color = "pink",
              ) %>%
              highcharter::hc_add_series(
                type = "line",
                data = cfr_estimate(),
                highcharter::hcaes(.data$date, .data$severity_mean),
                name = estimate_label(),
                color = "darkred"
              ) %>%
              highcharter::hc_chart(zoomType = "xy") %>%
              highcharter::hc_xAxis(type = "datetime") %>%
              highcharter::hc_yAxis_multiples(
                list(title = list(text = ""), max = 1),
                list(title = list(text = ""), opposite = TRUE, linkedTo = 0)
              ) %>%
              highcharter::hc_plotOptions(
                line = list(marker = list(enabled = FALSE))
              ) %>%
              highcharter::hc_tooltip(
                shared = TRUE, sort = TRUE, valueDecimals = 3
              ) %>%
              highcharter::hc_title(
                text = sprintf(
                  "Overall (static) CFR estimate: %.3f, 95%% CI: %.3f — %.3f",
                  cfr_estimate_static()$severity_mean,
                  cfr_estimate_static()$severity_low,
                  cfr_estimate_static()$severity_high
                ),
                margin = 20,
                align = "left"
              ) %>%
              highcharter::hc_subtitle(
                text = sprintf(
                  "Delay correction %s",
                  ifelse(correction_applied(), "applied", "not applied")
                )
              ) %>%
              my_hc_export()
          ),
          cfr_estimate()
        )

      # pass plot to output ui
      output$cfr_plot <- highcharter::renderHighchart(cfr_plot_hc())
    }
  )
}
