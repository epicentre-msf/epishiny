#' Time module
#'
#' Visualise data over time with an interactive 'epicurve'.
#'
#' @rdname time
#'
#' @param id Module id. Must be the same in both the UI and server function to link the two.
#' @param date_vars named character vector of date variables for the date axis input. Names are used as variable labels.
#' @param group_vars named character vector of categorical variables for the data grouping input. Names are used as variable labels.
#' @param title header title for the card.
#' @param opts_btn_lab text label for the dropdown menu button.
#' @param date_lab text label for the date variable input.
#' @param date_int_lab text label for the date interval input.
#' @param day_week_month_labs character vector with text labels for day, week and month, respectively.
#' @param groups_lab text label for the grouping variable input.
#' @param n_lab The label for the raw count variable.
#' @param ratio_line_lab text label for the ratio line input. If not supplied the input is not included.
#' @param full_screen Add button to card to with the option to enter full screen mode?
#'
#' @return the module server function returns any point click event data of the highchart.
#'   see [highcharter::hc_add_event_point] for details.
#' @import shiny
#' @export

time_ui <- function(
    id,
    date_vars,
    group_vars,
    title = "Time",
    opts_btn_lab = "options",
    date_lab = "Date axis",
    date_int_lab = "Date interval",
    day_week_month_labs = c("Day", "Week", "Month"),
    groups_lab = "Group data by",
    n_lab = "N patients",
    ratio_line_lab = NULL,
    full_screen = TRUE
) {
  ns <- NS(id)
  tagList(
    use_epishiny(),
    bslib::card(
      # min_height = 300,
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
            choices = c(purrr::set_names("n", n_lab), group_vars),
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

        # class = "d-flex align-items-center gap-1",
        # tags$span(
        #   shiny::icon("chart-column"),
        #   title,
        #   bslib::tooltip(
        #     bsicons::bs_icon("info-circle"),
        #     "Patient origin/residence",
        #     placement = "right"
        #   ),
        #   class = "pe-2"
        # ),
        # bslib::popover(
        #   trigger = bsicons::bs_icon("gear"),
        #   title = opts_btn_lab,

        # )
      ),
      bslib::card_body(
        padding = 0,
        # min_height = 300,
        # echarts4r::echarts4rOutput(ns("epicurve"), height = "100%")
        highcharter::highchartOutput(ns("chart"))
      )
    )
  )
}




#' @param df_ll Data frame or tibble of patient level linelist data. Can be either a shiny reactive or static dataset.
#' @param y_lab The label for y-axis of chart.
#' @param n_lab The label for the raw count variable.
#' @param ratio_var Character string of variable name to use for ratio calculation.
#' @param ratio_lab The label to describe the computed ratio i.e. 'CFR' for case fatality ratio.
#' @param ratio_numer Value(s) in `ratio_var` to be used for the ratio numerator i.e. 'Death'.
#' @param ratio_denom Values in `ratio_var` to be used for the ratio denominator i.e. `c('Death', 'Recovery')`.
#' @param filter_info If contained within an app using [filter_server()], supply the `filter_info` element
#'   returned by that function here as a shiny reactive to add filter information to chart exports.
#'
#' @rdname time
#'
#' @importFrom dplyr .data
#' @export
time_server <- function(
    id,
    df_ll,
    date_vars,
    group_vars,
    y_lab = "Patients",
    n_lab = "N patients",
    ratio_var = NULL,
    ratio_lab = NULL,
    ratio_numer = NULL,
    ratio_denom = NULL,
    filter_info = shiny::reactiveVal()
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      df_mod <- reactive({
        force_reactive(df_ll)
      })

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
            week_start = getOption("epishiny.week.start", 1)
          ))

        if (group == "n") {
          df <- df %>%
            dplyr::count(!!date) %>%
            tidyr::drop_na()
        } else {
          df <- df %>%
            dplyr::count(!!date, !!group) %>%
            tidyr::drop_na()
        }

        if (!is.null(ratio_var)) {
          if (is.null(ratio_numer) || is.null(ratio_denom)) {
            stop("both `ratio_numer` and `ratio_denom` must be supplied when `ratio_var` is supplied")
          }

          df_ratio <- df_mod() %>%
            dplyr::mutate(!!date := lubridate::floor_date(
              lubridate::as_date(!!date),
              input$date_interval,
              week_start = getOption("epishiny.week.start", 1)
            )) %>%
            dplyr::group_by(!!date) %>%
            dplyr::summarise(
              n = sum(.data[[ratio_var]] %in% ratio_numer),
              N = sum(.data[[ratio_var]] %in% ratio_denom),
              ratio = (n / N) * 100,
              .groups = "drop"
            ) %>%
            dplyr::select(!!date, ratio)

          df <- df %>% dplyr::left_join(df_ratio, by = input$date)
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

        shiny::validate(shiny::need(nrow(df) > 0, "No data to display"))

        date_lab <- ifelse(
          is.null(names(date_vars[date_vars == date])),
          date,
          names(date_vars[date_vars == date])
        )

        if (group == "n") {
          hc <- highcharter::hchart(df, "column", highcharter::hcaes(!!date_sym, n), name = n_lab)
        } else {
          group_lab <- ifelse(
            is.null(names(group_vars[group_vars == group])),
            group,
            names(group_vars[group_vars == group])
          )

          text_legend <- glue::glue(
            '{group_lab}<br/><span style="font-size: 9px; color: #666; font-weight: normal">(click to filter)</span>'
          )

          hc <-
            highcharter::hchart(df, "column", highcharter::hcaes(!!date_sym, n, group = !!group_sym)) %>%
            highcharter::hc_legend(
              title = list(text = text_legend),
              layout = "vertical",
              align = "right",
              verticalAlign = "top",
              x = -10,
              y = 40,
              itemStyle = list(textOverflow = "ellipsis", width = 150)
            )
        }

        hc <- hc %>%
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
              highcharter::hcaes(x =!!date, y = ratio),
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
              highcharter::hcaes(x = !!rv$date_sym, y = ratio),
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

    }
  )
}

