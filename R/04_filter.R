
#' Filter module
#'
#' Filter linelist data using a sidebar with shiny inputs.
#'
#' @rdname filter
#'
#' @param id Module id. Must be the same in both the UI and server function to link the two.
#' @param date_range A vector containing the minimum and maximum dates for the date range input.
#' @param title The title of the sidebar.
#' @param date_filters_lab The label for the date filters accordion panel.
#' @param period_lab The label for the date range input.
#' @param missing_dates_lab The label for the include missing dates checkbox.
#' @param group_filters_lab The label for the group filters accordion panel.
#' @param filter_btn_lab The label for the filter data button.
#' @param reset_btn_lab The label for the reset filters button.
#'
#' @return A [bslib::sidebar] UI element with date filters, group filters, and action buttons.
#'
#' @import shiny
#' @export
#' @example inst/examples/docs/app.R
filter_ui <- function(
    id,
    date_range,
    title = "Data filters",
    date_filters_lab = "Date filters",
    period_lab = "Period",
    missing_dates_lab = "Include patients with missing dates?",
    group_filters_lab = "Group filters",
    filter_btn_lab = "Filter data",
    reset_btn_lab = "Reset filters"
) {
  ns <- NS(id)

  bslib::sidebar(
    id = ns("sb"),
    title = title,
    bg = "white",

    bslib::accordion(
      open = FALSE,
      bslib::accordion_panel(
        date_filters_lab,
        dateRangeInput(
          inputId = ns("date"),
          label = period_lab,
          min = date_range[1],
          max = date_range[2],
          start = date_range[1],
          end = date_range[2],
          weekstart = 1,
          format = "d/m/yy"
        ),
        # actionButton(ns("days_all"), "Période complète", class = "btn-sm"),
        # actionButton(ns("days_14"), "14 jours", class = "btn-sm"),
        # actionButton(ns("days_7"), "7 jours", class = "btn-sm"),
        # actionButton(ns("days_1"), "Dernier jour", class = "btn-sm"),
        div(style = "padding-top: 10px;", shiny::checkboxInput(
          inputId = ns("include_date_na"),
          label = missing_dates_lab,
          value = TRUE
        ))
      ),
      bslib::accordion_panel(
        group_filters_lab,
        shiny::uiOutput(ns("group_filters"))
      )
    ),

    bslib::layout_columns(
      col_widths = 12,
      actionButton(
        ns("go"),
        filter_btn_lab,
        icon = icon("filter"),
        class = "btn-primary btn-sm"
      ),
      actionButton(
        ns("reset"),
        reset_btn_lab,
        icon = icon("arrows-rotate"),
        class = "btn-light btn-sm"
      ),
      uiOutput(ns("filter_info"))
    )
  )
}


#' @param df Data frame or tibble of patient level or aggregated data. Can be either a shiny reactive or static dataset.
#' @param date_var The name of the date variable in the data frame to be filtered on.
#' @param group_vars named character vector of categorical variables for the data grouping input. Names are used as variable labels.
#' @param na_label The label to use for missing values in group variables.
#'
#' @return The server function returns both the filtered data and a formatted text string with filter information
#'   named `df` and `filter_info` respectively in a reactive list. These should be passed as arguments
#'   of the same name in the time, place and person modules wrapped in a [`shiny::reactive()`]
#'
#'
#' @rdname filter
#' @export
filter_server <- function(
    id,
    df,
    date_var,
    group_vars,
    na_label = getOption("epishiny.na.label", "(Missing)")
) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # ==========================================================================
      # DYNAMIC INPUTS
      # ==========================================================================

      output$group_filters <- shiny::renderUI({
        purrr::map2(group_vars, names(group_vars), make_select_filter, ns, df_mod())
      }) %>% shiny::bindEvent(df_mod())

      # ==========================================================================
      # DATA
      # ==========================================================================

      rv <- reactiveValues(
        df = NULL,
        filter_info = NULL
      )

      df_mod <- reactive({
        force_reactive(df) %>%
          dplyr::mutate(dplyr::across(
            unname(group_vars),
            \(x) forcats::fct_na_value_to_level(x, level = na_label)
          ))
      })

      observe({
        rv$df <- df_mod()
      })

      # reset sidebar inputs when button clicked
      observeEvent(input$reset, {
        shinyjs::reset("sb")
        shinyjs::delay(500, shinyjs::click("go"))
      })

      # observe({
      #   date_range <- range(df_mod()[[date_var]], na.rm = TRUE)
      #   shiny::updateDateRangeInput(
      #     session,
      #     "date",
      #     min = date_range[1],
      #     max = date_range[2],
      #     start = date_range[1],
      #     end = date_range[2]
      #   )
      # }) %>% shiny::bindEvent(df_mod(), input$reset)

      # ==========================================================================
      # OBSERVERS
      # ==========================================================================

      # observeEvent(input$days_all, {
      #   date_range <- range(df_data[[date_var]], na.rm = TRUE)
      #   updateSliderInput(
      #     session,
      #     "date",
      #     value = c(date_range[1], date_range[2]),
      #     timeFormat = "%d %b",
      #     step = 1
      #   )
      # })
      #
      # observeEvent(input$days_14, {
      #   date_range <- range(df_data[[date_var]], na.rm = TRUE)
      #   date_range[1] <- date_range[2] - 13
      #   updateSliderInput(
      #     session,
      #     "date",
      #     value = c(date_range[1], date_range[2]),
      #     timeFormat = "%d %b",
      #     step = 1
      #   )
      # })
      #
      # observeEvent(input$days_7, {
      #   date_range <- range(df_data[[date_var]], na.rm = TRUE)
      #   date_range[1] <- date_range[2] - 6
      #   updateSliderInput(
      #     session,
      #     "date",
      #     value = c(date_range[1], date_range[2]),
      #     timeFormat = "%d %b",
      #     step = 1
      #   )
      # })
      #
      # observeEvent(input$days_1, {
      #   date_range <- range(df_data[[date_var]], na.rm = TRUE)
      #   date_range[1] <- date_range[2]
      #   updateSliderInput(
      #     session,
      #     "date",
      #     value = c(date_range[1], date_range[2]),
      #     timeFormat = "%d %b",
      #     step = 1
      #   )
      # })

      # ==========================================================================
      # FILTER DATA
      # ==========================================================================

      observe({
        # first filter dates
        date_range <- input$date

        df_out <- df_mod() %>%
          dplyr::filter(
            ((.data[[date_var]] >= as.Date(date_range[1]) & .data[[date_var]] <= as.Date(date_range[2]))
             | is.na(.data[[date_var]]))
          )

        if (!isolate(input$include_date_na)) {
          df_out <- df_out %>% dplyr::filter(!is.na(.data[[date_var]]))
        }

        # then filter over all picker inputs
        each_var <- purrr::map(group_vars, ~ filter_var(df_out[[.x]], input[[.x]]))
        selected <- purrr::reduce(each_var, ~ .x & .y)
        df_out <- df_out %>% dplyr::filter(selected)

        rv$df <- df_out
      }) %>% shiny::bindEvent(input$go, ignoreNULL = TRUE, ignoreInit = TRUE)

      # ==========================================================================
      # FILTER INFORMATION TEXT OUTPUT
      # ==========================================================================
      observe({
        date_filters <- glue::glue(
          "Period: {format(input$date[1], '%d/%b/%y')} - {format(input$date[2], '%d/%b/%y')}"
        )

        group_filters <- purrr::map2(unname(group_vars), names(group_vars), ~{
          if (length(input[[.x]])) {
            glue::glue("{.y}: {glue::glue_collapse(input[[.x]], sep = ', ')}")
          }
        }) %>% purrr::compact() %>% purrr::list_simplify()

        fi_out <- glue::glue("<b>Filters applied</b></br>{date_filters}")

        if (!is.null(group_filters)) {
          fi_out <- glue::glue("{fi_out} </br> {glue::glue_collapse(group_filters, sep = '</br>')}")
        }

        rv$filter_info <- fi_out
      })%>% shiny::bindEvent(input$go, ignoreNULL = FALSE, ignoreInit = FALSE)

      output$filter_info <- renderUI({
        shiny::helpText(shiny::HTML(rv$filter_info))
      })

      # return data to main app ===========================
      shiny::reactive({
        shiny::reactiveValuesToList(rv)
      })

    }
  )
}
