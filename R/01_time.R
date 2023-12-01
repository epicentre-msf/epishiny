#' Time module
#'
#' Visualise data over time with an interactive 'epicurve'.
#'
#' @rdname time
#'
#' @param id Module id. Must be the same in both the UI and server function to link the two.
#' @param date_vars Character vector of date variable(s) for the date axis. If named, names are used as variable labels.
#' @param count_vars If data is aggregated, variable name(s) of count variable(s) in data. If more than one variable provided,
#'  a select input will appear in the options dropdown. If named, names are used as variable labels.
#' @param group_vars Character vector of categorical variable names. If provided, a select input will appear
#'  in the options dropdown allowing for data groups to be visualised as stacked bars on the epicurve.
#'  If named, names are used as variable labels.
#' @param title Header title for the card.
#' @param icon The icon to display next to the title.
#' @param opts_btn_lab text label for the dropdown menu button.
#' @param date_lab text label for the date variable input.
#' @param date_int_lab text label for the date interval input.
#' @param date_intervals Character vector with choices for date aggregation intervals passed to the `unit`
#'  argument of [lubridate::floor_date]. If named, names are used as labels. Default is c('day', 'week', 'year').
#' @param count_vars_lab text label for the aggregate count variables input.
#' @param groups_lab text label for the grouping variable input.
#' @param no_grouping_lab text label for the no grouping option in the grouping input.
#' @param bar_stacking_lab text label for bar stacking option.
#' @param cumul_data_lab text label for cumulative data option.
#' @param ratio_line_lab text label for the ratio line input. This input will only be visable if 
#'  `show_ratio` is TRUE in [time_server]
#' @param full_screen Add button to card to with the option to enter full screen mode?
#'
#' @return the module server function returns any point click event data of the highchart.
#'   see [highcharter::hc_add_event_point] for details.
#' @import shiny
#' @export

time_ui <- function(
    id,
    date_vars,
    count_vars = NULL,
    group_vars = NULL,
    title = "Time",
    icon = bsicons::bs_icon("bar-chart-line-fill"),
    opts_btn_lab = "options",
    date_lab = "Date axis",
    date_int_lab = "Date interval",
    date_intervals = c("Day" = "day", "Week" = "week", "Month" = "month"),
    count_vars_lab = "Indicator",
    groups_lab = "Group data by",
    no_grouping_lab = "No grouping",
    bar_stacking_lab = "Bar stacking",
    cumul_data_lab = "Show cumulative data?",
    ratio_line_lab = "Show ratio line?",
    full_screen = TRUE
) {
  ns <- NS(id)

  # check deps are installed
  # pkg_deps <- c("highcharter", "lubridate")
  # if (!rlang::is_installed(pkg_deps)) {
  #   rlang::check_installed(pkg_deps, reason = "to use the epishiny time module.")
  # }

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
            choices = date_intervals,
            selected = ifelse("week" %in% date_intervals, "week", date_intervals[1])
          ),
          selectInput(
            ns("count_var"),
            label = count_vars_lab,
            choices = count_vars,
            multiple = FALSE,
            selectize = FALSE,
            width = 200
          ),
          selectInput(
            ns("group"),
            label = groups_lab,
            choices = c(purrr::set_names("n", no_grouping_lab), group_vars),
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
          shiny::checkboxInput(
            ns("show_ratio_line"),
            ratio_line_lab,
            value = FALSE,
            width = "100%"
          )
        )
      ),
      bslib::card_body(
        padding = 0,
        highcharter::highchartOutput(ns("chart"))
      )
    )
  )
}




#' @param df Data frame or tibble of patient level or aggregated data. Can be either a shiny reactive or static dataset.
#' @param show_ratio Display a ratio line on the epicurve?
#' @param ratio_var For patient level data, character string of variable name to use for ratio calculation.
#' @param ratio_lab The label to describe the computed ratio i.e. 'CFR' for case fatality ratio.
#' @param ratio_numer For patient level data, Value(s) in `ratio_var` to be used for the ratio numerator i.e. 'Death'.
#'  For aggregated data, character string of numeric count column to use of ratio numerator i.e. 'deaths'.
#' @param ratio_denom For patient level data, values in `ratio_var` to be used for the ratio denominator i.e. `c('Death', 'Recovery')`.
#'  For aggregated data, character string of numeric count column to use of ratio denominator i.e. 'cases'.
#' @param filter_info If contained within an app using [filter_server()], supply the `filter_info` element
#'   returned by that function here as a shiny reactive to add filter information to chart exports.
#'
#' @rdname time
#'
#' @importFrom dplyr .data
#' @export
time_server <- function(
    id,
    df,
    date_vars,
    count_vars = NULL,
    group_vars = NULL,
    show_ratio = FALSE,
    ratio_var = NULL,
    ratio_lab = NULL,
    ratio_numer = NULL,
    ratio_denom = NULL,
    place_filter = shiny::reactiveVal(),
    filter_info = shiny::reactiveVal()
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      if (is.null(group_vars)) {
        shinyjs::hide("group")
      }

      if (length(date_vars) < 2) {
        shinyjs::hide("date")
      }

      if (length(count_vars) < 2) {
        shinyjs::hide("count_var")
      }

      if (!show_ratio) {
        shinyjs::hide("show_ratio_line")
      }

      observe({
        cond <- input$group != "n"
        if (!cond) {
          shinyWidgets::updateRadioGroupButtons(session, "bar_stacking", selected = "normal")
        }
        shinyjs::toggle("bar_stacking", condition = cond, anim = TRUE)
      })

      df_mod <- reactive({
        df_out <- force_reactive(df)
        pf <- place_filter()
        if (pf$region_select != "all") {
          df_out <- df_out %>% dplyr::filter(.data[[pf$geo_col]] == pf$region_select)
        }
        df_out
      })

      # variables and labels etc
      rv <- reactiveValues()

      observe({
        date <- input$date
        rv$date <- date
        rv$date_sym <- rlang::sym(date)
        rv$count_var <- input$count_var
        group <- input$group
        rv$group <- group
        rv$group_sym <- rlang::sym(group)
        rv$missing_dates <- sum(is.na(df_mod()[[input$date]]))
        count_var <- input$count_var
        n_lab <- get_label(count_var, count_vars)
        rv$n_lab <- n_lab
      })

      df_curve <- reactive({
        date <- rlang::sym(input$date)
        group <- rlang::sym(input$group)

        # aggregate dates based on input
        df_interval <- df_mod() %>%
          dplyr::mutate(!!date := lubridate::floor_date(
            lubridate::as_date(!!date),
            unit = input$date_interval,
            week_start = getOption("epishiny.week.start", 1)
          ))

        # is the data pre-aggregated
        is_agg <- as.logical(length(count_vars))
        # is a data grouping variable supplied
        is_grouped <- input$group != "n"

        df_time <- get_time_df(
          df = df_interval,
          is_agg = is_agg,
          is_grouped = is_grouped,
          date_var = input$date,
          count_var = input$count_var,
          group_var = input$group
        )

        if (show_ratio) {
          df_ratio <- get_ratio_df(
            df = df_interval,
            date_var = input$date,
            is_agg = is_agg,
            ratio_var = ratio_var,
            ratio_lab = ratio_lab,
            ratio_numer = ratio_numer,
            ratio_denom = ratio_denom
          )
          df_time <- df_time %>% dplyr::left_join(df_ratio, by = input$date)
        }

        return(df_time)
      })

      output$chart <- highcharter::renderHighchart({
        req(df_curve())
        df <- df_curve()
        date <- isolate(rv$date)
        group <- isolate(rv$group)
        date_sym <- isolate(rv$date_sym)
        group_sym <- isolate(rv$group_sym)
        missing_dates <- isolate(rv$missing_dates)
        y_lab <- isolate(rv$n_lab)
        n_var <- dplyr::if_else(
          isolate(input$cumulative), "n_c", "n"
        )

        shiny::validate(shiny::need(nrow(df) > 0, "No data to display"))

        date_lab <- get_label(date, date_vars)

        if (group == "n") {
          hc <- highcharter::hchart(
            df,
            "column",
            highcharter::hcaes(!!date_sym, !!rlang::sym(n_var)),
            id = "n_bars",
            name = rv$n_lab
          ) %>% 
          highcharter::hc_tooltip(shared = TRUE)
        } else {
          group_lab <- get_label(group, group_vars)

          text_legend <- glue::glue(
            '{group_lab}<br/><span style="font-size: 9px; color: #666; font-weight: normal">(click to filter)</span>'
          )

          stacked_tooltip <- '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y} ({point.percentage:.1f}%)</b><br/>'

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
            ) %>% 
            highcharter::hc_tooltip(shared = TRUE, pointFormat = stacked_tooltip)
        }

        click_js <- highcharter::JS(
          glue::glue('function(event) {
            Shiny.setInputValue("{{ns("chart_click")}}", {x: event.point.x, y: event.point.y}, {priority: "event"});
          }', .open = "{{", .close = "}}")
        )

        hc <- hc %>%
          # highcharter::hc_add_event_point(event = "click") %>%
          highcharter::hc_title(text = NULL) %>%
          highcharter::hc_chart(zoomType = "x", alignTicks = TRUE) %>%
          highcharter::hc_plotOptions(
            column = list(stacking = isolate(input$bar_stacking)),
            series = list(cursor = "pointer", stickyTracking = FALSE, events = list(click = click_js))
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
          # highcharter::hc_tooltip(shared = TRUE, pointFormat = stacked_tooltip) %>%
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
                  title = list(text = rv$n_lab),
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
                  title = list(text = rv$n_lab),
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


      bar_click <- reactiveVal(NULL)

      observe({
        new_click <- input$chart_click$x
        if (identical(bar_click(), new_click)) {
          bar_click(NULL)
        } else {
          bar_click(new_click)
        }
      }) %>% bindEvent(input$chart_click)

      # reset bar click if date interval is changed
      observe({
        bar_click(NULL)
      }) %>% bindEvent(input$date_interval)

      # format chart click input values for filtering
      time_filter <- shiny::reactive({
        bc <- bar_click()
        if (length(bc)) {
          date_var <- input$date
          interval <- input$date_interval
          from <- lubridate::as_datetime(bc / 1000, origin = "1970-01-01")
          # this only makes sense if the interval is a full day or more
          to <- from + lubridate::period(1, input$date_interval) - lubridate::period(1, "day")
          list(
            date_var = date_var,
            from = from,
            to = to,
            interval = interval
          )
        } else {
          return(NULL)
        }
      })

      # show clicked period on chart
      observe({
        tf <- time_filter()
        if (length(tf)) {
          # browser()
          if (tf$interval == "day") {
            pad <- lubridate::period(12, "hours")
          } else {
            pad <- lubridate::as.period(lubridate::interval(tf$from, tf$to))$day / 2
            pad <- lubridate::period(ceiling(pad), "day")
          }
          date_from <- tf$from - pad
          date_to <- tf$from + pad
          highcharter::highchartProxy(ns("chart")) %>%
            highcharter::hcpxy_update(
              xAxis = list(plotBands = list(
                list(
                  color = "#D3D3D350",
                  zIndex = 1,
                  from = highcharter::datetime_to_timestamp(date_from),
                  to = highcharter::datetime_to_timestamp(date_to)
                  # label = list(text = "filter")
                )
              ))
            )
        } else {
            highcharter::highchartProxy(ns("chart")) %>%
              highcharter::hcpxy_update(xAxis = list(plotBands = list()))
        }
      }) %>% bindEvent(time_filter(), ignoreNULL = FALSE)

      # return to main app
      return(reactive(time_filter()))

    }
  )
}

#' @noRd
get_time_df <- function(
    df,
    is_agg,
    is_grouped,
    date_var,
    count_var,
    group_var
) {
  if (is_agg && !length(count_var)) {
    cli::cli_abort(c(
      "x" = "You must supply a `count_var` variable name if data is aggregated"
    ))
  }
  if (!is_grouped) {
    if (is_agg) {
      df <- df %>%
        dplyr::count(.data[[date_var]], wt = .data[[count_var]])
    } else {
      df <- df %>% dplyr::count(.data[[date_var]])
    }
    df <- df %>%
      tidyr::drop_na() %>%
      dplyr::arrange(.data[[date_var]]) %>%
      dplyr::mutate(n_c = cumsum(.data$n))
  } else {
    if (is_agg) {
      df <- df %>%
        dplyr::count(.data[[date_var]], .data[[group_var]], wt = .data[[count_var]])
    } else {
      df <- df %>% dplyr::count(.data[[date_var]], .data[[group_var]])
    }
    df <- df %>%
      tidyr::drop_na() %>%
      dplyr::group_by(.data[[group_var]]) %>%
      dplyr::arrange(.data[[date_var]]) %>%
      dplyr::mutate(n_c = cumsum(.data$n)) %>%
      dplyr::ungroup()
  }
  return(df)
}

#' @noRd 
get_ratio_df <- function(
  df,
  date_var,
  is_agg,
  ratio_var,
  ratio_lab,
  ratio_numer,
  ratio_denom
) {
  if (!is_agg) {
    # method for patient level data
    if (any(is.null(ratio_var), is.null(ratio_numer), is.null(ratio_denom), is.null(ratio_lab))) {
      cli::cli_abort(c(
        "x" = "to calculate a ratio from patient level data, the following must be provided:",
        "*" = "`ratio_var`: character string of variable name to use for ratio calculation i.e. 'outcome'",
        "*" = "`ratio_lab`: the axis label to be used for the ratio line i.e. 'CFR'",
        "*" = "`ratio_numer`: character vector of ratio calculation numerator levels(s) found in `ratio_var` i.e. 'death'",
        "*" = "`ratio_denom`: character vector of ratio calculation denominator levels(s) found in `ratio_var` i.e. c('death', 'recovery')",
        "i" = "See ?epishiny::time for details."
      ))
    }

    df_ratio <- df %>%
      dplyr::group_by(.data[[date_var]]) %>%
      dplyr::summarise(
        n1 = sum(.data[[ratio_var]] %in% ratio_numer),
        N = sum(.data[[ratio_var]] %in% unique(c(ratio_numer, ratio_denom))),
        ratio = (.data$n1 / .data$N) * 100,
        .groups = "drop"
      ) %>%
      dplyr::arrange(.data[[date_var]]) %>%
      dplyr::mutate(ratio_c = cumsum(.data$n1) / cumsum(.data$N) * 100) %>%
      dplyr::select(.data[[date_var]], .data$ratio, .data$ratio_c)

  } else {
    # method for aggregated data
    if (any(is.null(ratio_numer), is.null(ratio_denom), is.null(ratio_lab))) {
      cli::cli_abort(c(
        "x" = "to calculate a ratio from aggregated data, the following must be provided in `time_server` module:",
        "*" = "`ratio_numer`: character string variable name of numeric column in data to use for the ratio numerator i.e. 'deaths'",
        "*" = "`ratio_denom`: character string variable name of numeric column in data to use for the ratio denominator i.e. 'cases'",
        "*" = "`ratio_lab`: the axis label to be used for the ratio line i.e. 'CFR'",
        "i" = "See ?epishiny::time for details."
      ))
    }

    df_ratio <- df %>%
      dplyr::group_by(.data[[date_var]]) %>%
      dplyr::summarise(
        n1 = sum(.data[[ratio_numer]], na.rm = TRUE),
        N = sum(.data[[ratio_denom]], na.rm = TRUE),
        ratio = (.data$n1 / .data$N) * 100,
        .groups = "drop"
      ) %>%
      dplyr::arrange(.data[[date_var]]) %>%
      dplyr::mutate(ratio_c = cumsum(.data$n1) / cumsum(.data$N) * 100) %>%
      dplyr::select(.data[[date_var]], .data$ratio, .data$ratio_c)

  }
  return(df_ratio)
}