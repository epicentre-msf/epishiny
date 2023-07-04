
#' @export
filterUI <- function(id, date_range, date_label = "Period") {
  ns <- NS(id)

  bslib::sidebar(
    id = ns("sb"),
    title = "Filter module",
    bg = "white",

    bslib::accordion(
      open = FALSE,
      bslib::accordion_panel(
        "Date filters",
        dateRangeInput(
          inputId = ns("date"),
          label = date_label,
          min = date_range[1],
          max = date_range[2],
          start = date_range[1],
          end = date_range[2],
          weekstart = 1,
          format = "d/m/yy"
          # value = c(date_range[1], date_range[2]),
          # timeFormat = "%d %b",
          # step = 1
        ),
        # actionButton(ns("days_all"), "Période complète", class = "btn-sm"),
        # actionButton(ns("days_14"), "14 jours", class = "btn-sm"),
        # actionButton(ns("days_7"), "7 jours", class = "btn-sm"),
        # actionButton(ns("days_1"), "Dernier jour", class = "btn-sm"),
        div(style = "padding-top: 10px;", shiny::checkboxInput(
          inputId = ns("include_date_na"),
          label = "Include patients with missing dates?",
          value = TRUE
        ))
      ),
      bslib::accordion_panel(
        "Group filters",
        shiny::uiOutput(ns("group_filters"))
        # purrr::map2(group_vars, names(group_vars), make_select_filter, ns, df_linelist)
      )
    ),

    layout_column_wrap(
      width = 1,
      actionButton(
        ns("go"),
        "Filter data",
        icon = icon("filter"),
        class = "btn-primary btn-sm",
        style = "color: #fff;"
      ),
      actionButton(
        ns("reset"),
        "Reset filters",
        icon = icon("arrows-rotate"),
        class = "btn-info btn-sm",
        style = "color: #fff;"
      )
    )

  )
}

#' @export
filterServer <- function(id, df_ll, date_var, group_vars) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      observeEvent(input$reset, { shinyjs::reset("sb") })

      # ==========================================================================
      # DATA
      # ==========================================================================

      df_mod <- reactive({
        force_reactive(df_ll)
      })

      data_out <- reactiveVal()

      observe({
        data_out(df_mod())
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

      output$group_filters <- shiny::renderUI({
        purrr::map2(group_vars, names(group_vars), make_select_filter, ns, df_mod())
      }) %>% shiny::bindEvent(df_mod())

      # ==========================================================================
      # PUSHBAR SETUP
      # ==========================================================================
      # pushbar::setup_pushbar()
      # observeEvent(pb_init(), {
      #   if (pb_init() == 1) {
      #     pushbar::pushbar_open(id = ns("filters"))
      #   } else if (input$filters_pushbar_opened) {
      #     pushbar::pushbar_close()
      #   } else {
      #     pushbar::pushbar_open(id = ns("filters"))
      #   }
      # })
      # observeEvent(input$go, { pushbar::pushbar_close() })
      # observeEvent(input$reset, { shinyjs::reset("resetable_filters") })
      # observeEvent(input$close, { pushbar::pushbar_close() })

      # ==========================================================================
      # OBSERVERS
      # ==========================================================================


      observeEvent(input$days_all, {
        date_range <- range(df_data[[date_var]], na.rm = TRUE)
        updateSliderInput(
          session,
          "date",
          value = c(date_range[1], date_range[2]),
          timeFormat = "%d %b",
          step = 1
        )
      })

      observeEvent(input$days_14, {
        date_range <- range(df_data[[date_var]], na.rm = TRUE)
        date_range[1] <- date_range[2] - 13
        updateSliderInput(
          session,
          "date",
          value = c(date_range[1], date_range[2]),
          timeFormat = "%d %b",
          step = 1
        )
      })

      observeEvent(input$days_7, {
        date_range <- range(df_data[[date_var]], na.rm = TRUE)
        date_range[1] <- date_range[2] - 6
        updateSliderInput(
          session,
          "date",
          value = c(date_range[1], date_range[2]),
          timeFormat = "%d %b",
          step = 1
        )
      })

      observeEvent(input$days_1, {
        date_range <- range(df_data[[date_var]], na.rm = TRUE)
        date_range[1] <- date_range[2]
        updateSliderInput(
          session,
          "date",
          value = c(date_range[1], date_range[2]),
          timeFormat = "%d %b",
          step = 1
        )
      })

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

        data_out(df_out)
      }) %>% shiny::bindEvent(input$go, ignoreNULL = TRUE, ignoreInit = TRUE)

      # data_out <- reactive({
      #   input$go
      #
      #   # first filter dates
      #   date_range <- input$date
      #
      #   df_out <- df_mod() %>%
      #     dplyr::filter(
      #       ((.data[[date_var]] >= as.Date(date_range[1]) & .data[[date_var]] <= as.Date(date_range[2]))
      #        | is.na(.data[[date_var]]))
      #     )
      #
      #   if (!isolate(input$include_date_na)) {
      #     df_out <- df_out %>% dplyr::filter(!is.na(.data[[date_var]]))
      #   }
      #
      #   # then filter over all picker inputs
      #   each_var <- purrr::map(group_vars, ~ filter_var(df_out[[.x]], input[[.x]]))
      #   selected <- purrr::reduce(each_var, ~ .x & .y)
      #   df_out <- df_out %>% dplyr::filter(selected)
      #
      #   return(df_out)
      # }) %>%
      #   # shiny::bindCache(input) %>%
      #   shiny::bindEvent(input$go, ignoreNULL = FALSE)

      # return to main app ===========================
      return(data_out)

    }
  )
}

#' @export
filterPushbarUI <- function(id, date_range, group_vars) {
  ns <- NS(id)
  tagList(
    pushbar::pushbar_deps(),
    pushbar::pushbar(
      id = ns("filters"),
      style = "background:#fbfbfb;margin-top:50px;padding:20px;width:360px;z-index:9999;",
      div(
        class = "sidebar-inputs",
        sliderInput(
          inputId = ns("date"),
          label = "Date d'admission",
          min = date_range[1],
          max = date_range[2],
          value = c(date_range[1], date_range[2]),
          timeFormat = "%d %b",
          step = 1
        ),
        actionButton(ns("days_all"), "Période complète", class = "btn-sm"),
        actionButton(ns("days_14"), "14 jours", class = "btn-sm"),
        actionButton(ns("days_7"), "7 jours", class = "btn-sm"),
        actionButton(ns("days_1"), "Dernier jour", class = "btn-sm"),
        div(style = "padding-top: 10px;", shiny::checkboxInput(
          inputId = ns("include_date_na"),
          label = "Inclure les cas avec des dates manquantes ?",
          value = TRUE
        )),
        div(
          id = ns("resetable_filters"),
          purrr::map2(group_vars, names(group_vars), make_select_filter, ns),
          actionButton(ns("go"), "Update Data", icon = icon("database"), class = "btn-primary btn-sm", style = "color: #fff;"),
          actionButton(ns("reset"), "Reset Inputs", icon = icon("arrows-rotate"), class = "btn-info btn-sm", style = "color: #fff;"),
          actionButton(ns("close"), "Close", icon = icon("circle-xmark"), class = "btn-danger btn-sm", style = "color: #fff;")
        )
      )
    )
  )
}
