
#' @export
epicurve_ui <- function(
    id,
    date_vars,
    group_vars,
    title = "Time",
    opts_btn_lab = "options",
    date_lab = "Date axis",
    date_int_lab = "Date interval",
    day_week_month_labs = c("Day", "Week", "Month"),
    groups_lab = "Group data by",
    ratio_line_lab = NULL,
    full_screen = TRUE
) {
  ns <- NS(id)
  bslib::card(
    full_screen = full_screen,
    bslib::card_header(
      class = "d-flex justify-content-start align-items-center",

      tags$span(shiny::icon("chart-column"), title, class = "pe-2"),

      shinyWidgets::dropMenu(
        actionButton(
          ns("dropdown"),
          icon = icon("sliders"),
          label = opts_btn_lab,
          class = "btn-sm"
        ),
        options = shinyWidgets::dropMenuOptions(flip = TRUE),
        selectInput(
          ns("date"),
          label = date_lab,
          choices = date_vars,
          multiple = FALSE,
          selectize = FALSE,
          width = 200
        ),
        tags$br(),
        shinyWidgets::radioGroupButtons(
          ns("date_interval"),
          label = date_int_lab,
          size = "sm",
          status = "outline-dark",
          choices = purrr::set_names(
            c("day", "week", "month"),
            day_week_month_labs
          ),
          selected = "week"
        ),
        tags$br(),
        selectInput(
          ns("group"),
          label = groups_lab,
          choices = group_vars,
          multiple = FALSE,
          selectize = FALSE,
          width = 200
        ),
        tags$br(),
        if (!is.null(ratio_line_lab)) {
          shiny::checkboxInput(
            ns("show_ratio_line"),
            ratio_line_lab,
            value = FALSE,
            width = "100%"
          )
        }
      )
    ),
    bslib::card_body(
      padding = 0,
      min_height = 300,
      # echarts4r::echarts4rOutput(ns("epicurve"), height = "100%")
      highcharter::highchartOutput(ns("chart"))
    )
  )
}

#' @export
epicurve_server <- function(
    id,
    df_ll,
    date_vars,
    group_vars,
    y_lab = "Patients",
    ratio_var = NULL,
    ratio_lab = NULL,
    ratio_numer = NULL,
    ratio_denom = NULL,
    week_start = 1,
    filter_info = shiny::reactiveVal()
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      df_mod <- reactive({
        force_reactive(df_ll)
      })

      # filter_info <- reactive({
      #   force_reactive(filter_info)
      # })

      # variables and labels etc
      rv <- reactiveValues()

      observe({
        date <- input$date
        rv$date <- date
        rv$date_sym <- rlang::sym(date)
        group <- input$group
        rv$group <- group
        rv$group_sym <- rlang::sym(group)
        rv$missing_dates <- sum(is.na(df_mod()[[input$date]]))
      })

      df_curve <- reactive({
        date <- rlang::sym(input$date)
        group <- rlang::sym(input$group)

        df <- df_mod() %>%
          dplyr::mutate(!!date := lubridate::floor_date(
            lubridate::as_date(!!date),
            unit = input$date_interval,
            week_start = week_start
          )) %>%
          dplyr::count(!!date, !!group) %>%
          tidyr::drop_na()

        if (!is.null(ratio_var)) {
          if (is.null(ratio_numer) || is.null(ratio_denom)) {
            stop("both `ratio_numer` and `ratio_denom` must be supplied when `ratio_var` is supplied")
          }

          df_rate <- df_mod() %>%
            dplyr::mutate(!!date := lubridate::floor_date(
              lubridate::as_date(!!date),
              input$date_interval,
              week_start = week_start
            )) %>%
            dplyr::group_by(!!date) %>%
            dplyr::summarise(
              n = sum(.data[[ratio_var]] %in% ratio_numer),
              N = sum(.data[[ratio_var]] %in% ratio_denom),
              rate = (n / N) * 100,
              .groups = "drop"
            ) %>%
            dplyr::select(!!date, rate)

          df <- df %>% dplyr::left_join(df_rate, by = input$date)
        }

        return(df)
      })

      output$chart <- highcharter::renderHighchart({

        df <- df_curve()
        date <- isolate(rv$date)
        group <- isolate(rv$group)
        date_sym <- isolate(rv$date_sym)
        group_sym <- isolate(rv$group_sym)
        missing_dates <- isolate(rv$missing_dates)

        date_lab <- ifelse(
          is.null(names(date_vars[date_vars == date])),
          date,
          names(date_vars[date_vars == date])
        )

        group_lab <- ifelse(
          is.null(names(group_vars[group_vars == group])),
          group,
          names(group_vars[group_vars == group])
        )

        text_legend <- glue::glue(
          '{group_lab}<br/><span style="font-size: 9px; color: #666; font-weight: normal">(click to filter)</span>'
        )

        shiny::validate(shiny::need(nrow(df) > 0, "No data to display"))

        hc <-
          highcharter::hchart(df, "column", highcharter::hcaes(!!date_sym, n, group = !!group_sym)) %>%
          highcharter::hc_add_event_point(event = "click") %>%
          highcharter::hc_title(text = NULL) %>%
          highcharter::hc_chart(zoomType = "x", alignTicks = TRUE) %>%
          highcharter::hc_xAxis(
            title = list(text = date_lab),
            allowDecimals = FALSE,
            crosshair = TRUE
          ) %>%
          highcharter::hc_yAxis_multiples(
            list(
              title = list(text = y_lab),
              allowDecimals = FALSE
            ),
            list(
              title = list(text = ""),
              labels = list(enabled = TRUE),
              opposite = TRUE,
              gridLineWidth = 0
            )
          ) %>%
          highcharter::hc_tooltip(shared = TRUE) %>%
          highcharter::hc_legend(
            title = list(text = text_legend),
            layout = "vertical",
            align = "right",
            verticalAlign = "top",
            x = -10,
            y = 40,
            itemStyle = list(textOverflow = "ellipsis", width = 150)
          ) %>%
          my_hc_export(caption = isolate(filter_info()))

        if (isolate(input$date_interval == "week")) {
          hc <- hc %>%
            highcharter::hc_xAxis(
              title = list(text = date_lab),
              allowDecimals = FALSE,
              crosshair = TRUE,
              labels = list(
                formatter = hc_week_labels()
              )
            )
        }

        if (isolate(isTruthy(input$show_ratio_line))) {
          hc <- hc %>%
            highcharter::hc_yAxis_multiples(
              list(
                title = list(text = y_lab),
                allowDecimals = FALSE
              ),
              list(
                title = list(text = ratio_lab),
                labels = list(enabled = TRUE, format = "{value}%"),
                gridLineWidth = 1,
                opposite = TRUE
              )
            ) %>%
            highcharter::hc_add_series(
              data = df,
              "line",
              highcharter::hcaes(x =!!date, y = rate),
              id = "ratio_line",
              name = ratio_lab,
              yAxis = 1,
              zIndex = 10,
              color = "black",
              tooltip = list(valueDecimals = 1, valueSuffix = "%")
            )
        }

        if (missing_dates > 0) {
          hc <- hc %>%
            highcharter::hc_credits(
              enabled = TRUE,
              text = glue::glue("Missing {tolower(date_lab)} for {scales::number(missing_dates)} {tolower(y_lab)}")
            )
        }

        hc
      })

      shiny::observe({
        if (isTruthy(input$show_ratio_line)) {

          df_line <- df_curve()

          highcharter::highchartProxy(ns("chart")) %>%
            highcharter::hcpxy_remove_series(id = "ratio_line") %>%
            highcharter::hcpxy_update(
              yAxis = list(
                list(
                  title = list(text = y_lab),
                  allowDecimals = FALSE
                ),
                list(
                  title = list(text = ratio_lab),
                  labels = list(enabled = TRUE, format = "{value}%"),
                  gridLineWidth = 1,
                  opposite = TRUE
                )
              )
            ) %>%
            highcharter::hcpxy_add_series(
              data = df_line,
              "line",
              highcharter::hcaes(x = !!rv$date_sym, y = rate),
              id = "ratio_line",
              name = ratio_lab,
              yAxis = 1,
              zIndex = 10,
              color = "black",
              tooltip = list(valueDecimals = 1, valueSuffix = "%")
            )
        } else {
          highcharter::highchartProxy(ns("chart")) %>%
            highcharter::hcpxy_remove_series(id = "ratio_line") %>%
            highcharter::hcpxy_update(
              yAxis = list(
                list(
                  title = list(text = y_lab),
                  allowDecimals = FALSE
                ),
                list(
                  title = list(text = ""),
                  labels = list(enabled = FALSE),
                  gridLineWidth = 0,
                  opposite = TRUE
                )
              )
            )
        }
      }) %>% shiny::bindEvent(input$show_ratio_line, ignoreInit = TRUE)

      # return chart click input values
      shiny::reactive({
        input$chart_click
      })

      # output$chart <- echarts4r::renderEcharts4r({
      #
      #   df_curve <- df_curve()
      #   date <- isolate(input$date)
      #   group <- isolate(input$group)
      #   date_sym <- rlang::sym(date)
      #   group_sym <- rlang::sym(group)
      #   missing_dates <- isolate(missing_dates())
      #
      #   date_lab <- ifelse(
      #     is.null(names(date_vars[date_vars == date])),
      #     date,
      #     names(date_vars[date_vars == date])
      #   )
      #
      #   group_lab <- ifelse(
      #     is.null(names(group_vars[group_vars == group])),
      #     group,
      #     names(group_vars[group_vars == group])
      #   )
      #
      #   df_curve %>%
      #     mutate(!!group_sym := forcats::fct_explicit_na(!!group_sym)) %>%
      #     group_by(!!group_sym) %>%
      #     echarts4r::e_charts_(date, dispose = FALSE) %>%
      #     echarts4r::e_bar_("n", stack = "group") %>%
      #     echarts4r::e_axis_labels(x = date_lab, y = "Cases") %>%
      #     echarts4r::e_tooltip(trigger = "axis") %>%
      #     echarts4r::e_x_axis(type = "category") %>%
      #     echarts4r::e_grid(
      #       left = '2%',
      #       top = '10%',
      #       right = '10%',
      #       bottom = '10%',
      #       containLabel = FALSE
      #     )
      #
      # })
      #
      # observeEvent(input$label_week, {
      #   if (input$label_week) {
      #     echarts4r::echarts4rProxy("epicurve") %>%
      #       echarts4r::e_x_axis(type = "category", formatter = weekLabels())
      #   } else {
      #     echarts4r::echarts4rProxy("epicurve") %>%
      #       echarts4r::e_x_axis(type = "category")
      #   }
      # })

    }
  )
}

