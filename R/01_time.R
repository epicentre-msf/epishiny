#' Time module
#'
#' Visualise data over time with an interactive 'epicurve'.
#'
#' @rdname time
#'
#' @param id Module id. Must be the same in both the UI and server function to link the two.
#' @param date_vars named character vector of date variables for the date axis input. Names are used as variable labels.
#' @param group_vars Character vector of categorical variable names. If provided, a select input will appear
#'  in the options dropdown allowing for data groups to be visualised as stacked bars on the epicurve.
#'  If named, names are used as variable labels.
#' @param title header title for the card.
#' @param icon The icon to display next to the title.
#' @param opts_btn_lab text label for the dropdown menu button.
#' @param date_lab text label for the date variable input.
#' @param date_int_lab text label for the date interval input.
#' @param day_week_month_labs character vector with text labels for day, week and month, respectively.
#' @param groups_lab text label for the grouping variable input.
#' @param bar_stacking_lab text label for bar stacking option.
#' @param cumul_data_lab text label for cumulative data option.
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
    group_vars = NULL,
    title = "Time",
    icon = bsicons::bs_icon("bar-chart-line-fill"),
    opts_btn_lab = "options",
    date_lab = "Date axis",
    date_int_lab = "Date interval",
    day_week_month_labs = c("Day", "Week", "Month"),
    groups_lab = "Group data by",
    bar_stacking_lab = "Bar stacking",
    cumul_data_lab = "Show cumulative data?",
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

        tags$span(icon, title, class = "pe-2"),

        # options button and dropdown menu
        bslib::popover(
          title =  tags$span(shiny::icon("sliders"), opts_btn_lab),
          trigger = actionButton(
            ns("dropdown"),
            icon = shiny::icon("sliders"),
            label = opts_btn_lab,
            class = "btn-sm pe-2 me-2"
          ),
          selectInput(
            ns("date"),
            label = date_lab,
            choices = date_vars,
            multiple = FALSE,
            selectize = FALSE,
            width = 200
          ),
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
          selectInput(
            ns("group"),
            label = groups_lab,
            choices = c(purrr::set_names("n", n_lab), group_vars),
            multiple = FALSE,
            selectize = FALSE,
            width = 200
          ),
          shinyWidgets::radioGroupButtons(
            ns("bar_stacking"),
            label = bar_stacking_lab,
            size = "sm",
            status = "outline-dark",
            choices = c("Count" = "normal", "Percent" = "percent"),
            selected = "normal"
          ),
          shiny::checkboxInput(
            ns("cumulative"),
            cumul_data_lab,
            value = FALSE,
            width = "100%"
          ),
          # only show ratio line option if label provided
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
    group_vars = NULL,
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

      if (is.null(group_vars)) {
        shinyjs::hide("group")
      }

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

        if (input$group == "n") {
          df <- df %>%
            dplyr::count(!!date) %>%
            tidyr::drop_na() %>% 
            dplyr::arrange(!!date) %>% 
            dplyr::mutate(n_c = cumsum(.data$n))
        } else {
          df <- df %>%
            dplyr::count(!!date, !!group) %>%
            tidyr::drop_na() %>% 
            dplyr::group_by(!!group) %>% 
            dplyr::arrange(!!date) %>%
            dplyr::mutate(n_c = cumsum(.data$n)) %>% 
            dplyr::ungroup()
        }

        if (!is.null(ratio_var)) {
          if (is.null(ratio_numer) || is.null(ratio_denom)) {
            stop("both `ratio_numer` and `ratio_denom` must be supplied when `ratio_var` is supplied")
          }

          df_ratio <- df_mod() %>%
            dplyr::mutate(!!date := lubridate::floor_date(
              lubridate::as_date(!!date),
              unit = input$date_interval,
              week_start = getOption("epishiny.week.start", 1)
            )) %>%
            dplyr::group_by(!!date) %>%
            dplyr::summarise(
              n1 = sum(.data[[ratio_var]] %in% ratio_numer),
              N = sum(.data[[ratio_var]] %in% ratio_denom),
              ratio = (.data$n1 / .data$N) * 100,
              .groups = "drop"
            ) %>%
            dplyr::arrange(!!date) %>% 
            dplyr::mutate(ratio_c = cumsum(.data$n1) / cumsum(.data$N) * 100) %>% 
            dplyr::select(!!date, .data$ratio, .data$ratio_c)

          df <- df %>% dplyr::left_join(df_ratio, by = input$date)
        }

        return(df)
      })

      output$chart <- highcharter::renderHighchart({
        req(df_curve())
        df <- df_curve()
        date <- isolate(rv$date)
        group <- isolate(rv$group)
        date_sym <- isolate(rv$date_sym)
        group_sym <- isolate(rv$group_sym)
        missing_dates <- isolate(rv$missing_dates)
        n_var <- dplyr::if_else(
          isolate(input$cumulative), "n_c", "n"
        )

        shiny::validate(shiny::need(nrow(df) > 0, "No data to display"))

        date_lab <- ifelse(
          is.null(names(date_vars[date_vars == date])),
          date,
          names(date_vars[date_vars == date])
        )

        if (group == "n") {
          hc <- highcharter::hchart(
            df,
            "column",
            highcharter::hcaes(!!date_sym, !!rlang::sym(n_var)),
            id = "n_bars",
            name = n_lab
          )
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
            highcharter::hchart(df, "column", highcharter::hcaes(!!date_sym, !!n_var, group = !!group_sym)) %>%
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

        stacked_tooltip <- '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y} ({point.percentage:.1f}%)</b><br/>'

        hc <- hc %>%
          highcharter::hc_add_event_point(event = "click") %>%
          highcharter::hc_title(text = NULL) %>%
          highcharter::hc_chart(zoomType = "x", alignTicks = TRUE) %>%
          highcharter::hc_plotOptions(
            column = list(stacking = isolate(input$bar_stacking))
          ) %>% 
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
          highcharter::hc_tooltip(shared = TRUE, pointFormat = stacked_tooltip) %>%
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
          r_var <- ifelse(
            isolate(input$cumulative),
            rlang::sym("ratio_c"),
            rlang::sym("ratio")
          )
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
              highcharter::hcaes(x =!!date, y = !!r_var),
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
      }) %>% bindEvent(df_curve())

      # update bar stacking type
      observe({
        highcharter::highchartProxy(ns("chart")) %>%
          highcharter::hcpxy_update(
            plotOptions = list(column = list(stacking = input$bar_stacking))
          )
      }) %>% bindEvent(input$bar_stacking, ignoreInit = TRUE)

      # update data between cumul/non-cumul
      shiny::observe({
        df <- df_curve()
        date <- isolate(rv$date)
        group <- isolate(rv$group)
        date_sym <- isolate(rv$date_sym)
        group_sym <- isolate(rv$group_sym)

        if (input$cumulative) {
          n_var <- "n_c"
          r_var <- "ratio_c"
        } else {
          n_var <- "n"
          r_var <- "ratio"
        }

        if (group == "n") {
          highcharter::highchartProxy(ns("chart")) %>%
            highcharter::hcpxy_update_series(
              id = "n_bars",
              data = df[[n_var]]
            )
        } else {
          highcharter::highchartProxy(ns("chart")) %>%
            highcharter::hcpxy_set_data(
              type = "column",
              data = df,
              mapping = highcharter::hcaes(!!date_sym, !!rlang::sym(n_var), group = !!group_sym),
              redraw = TRUE
            )
        }

        if (isTruthy(input$show_ratio_line)) {
          highcharter::highchartProxy(ns("chart")) %>%
            highcharter::hcpxy_update_series(
              id = "ratio_line",
              data = df[[r_var]]
            )
        }
      }) %>% shiny::bindEvent(input$cumulative, ignoreInit = TRUE)

      shiny::observe({
        if (isTruthy(input$show_ratio_line)) {

          df_line <- df_curve()
          r_var <- ifelse(
            input$cumulative, 
            rlang::sym("ratio_c"), 
            rlang::sym("ratio")
          )

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
              highcharter::hcaes(x = !!rv$date_sym, y = !!r_var),
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

