
#' Person module
#'
#' Visualise age and sex demographics in a population pyramid chart and summary table.
#'
#' @rdname person
#'
#' @param id Module id. Must be the same in both the UI and server function to link the two.
#' @param count_vars If data is aggregated, variable name(s) of count variable(s) in data. If more than one variable provided,
#'  a select input will appear in the options dropdown. If named, names are used as variable labels.
#' @param title The title for the card.
#' @param icon The icon to display next to the title.
#' @param opts_btn_lab The label for the options button.
#' @param count_vars_lab text label for the aggregate count variables input.
#' @param full_screen Add button to card to with the option to enter full screen mode?
#'
#' @return A [bslib::navset_card_tab] UI element with chart and table tabs.
#' @export
#' @example inst/examples/docs/app.R
person_ui <- function(
    id,
    count_vars = NULL,
    title = "Person",
    icon = bsicons::bs_icon("people-fill"),
    opts_btn_lab = "options",
    count_vars_lab = "Indicator",
    full_screen = TRUE
) { 
  ns <- shiny::NS(id)

  bslib::navset_card_tab(
    wrapper = function(...) {bslib::card_body(..., padding = 0, class = "person-container")}, 
    full_screen = full_screen,
    id = ns("tabs"),

    title = tags$div(
      class = "d-flex justify-content-start align-items-center",
      tags$span(icon, title, class = "pe-2"),
      bslib::popover(
        title = tags$span(shiny::icon("sliders"), opts_btn_lab),
        trigger = actionButton(
          ns("dropdown"),
          icon = shiny::icon("sliders"),
          label = opts_btn_lab,
          class = "btn-sm"
        ),
        selectInput(
          ns("count_var"),
          label = count_vars_lab,
          choices = count_vars,
          multiple = FALSE,
          selectize = FALSE,
          width = 200
        )
      )
    ),

    bslib::nav_panel(
      title = shiny::icon("chart-bar"),
      highcharter::highchartOutput(ns("as_pyramid"))
    ),
    bslib::nav_panel(
      title = bsicons::bs_icon("table"),
      tags$div(
        id = ns("as_tbl_container"),
        style = "min-height: 300px;",
        gt::gt_output(ns("as_tbl"))
      )
    )
  )

}

#' @param df Data frame or tibble of patient level or aggregated data. Can be either a shiny reactive or static dataset.
#' @param age_var The name of a numeric age variable in the data. 
#'  If ages have already been binned into groups, use `age_group_var` instead.
#' @param age_group_var The name of a character/factor variable in the data with age groups. 
#'  If specified, `age_var` is ignored.
#' @param sex_var The name of the sex variable in the data.
#' @param male_level The level representing males in the sex variable.
#' @param female_level The level representing females in the sex variable.
#' @param age_breaks A numeric vector specifying age breaks for age groups.
#' @param age_labels Labels corresponding to the age breaks.
#' @param age_var_lab The label for the age variable.
#' @param age_group_lab The label for the age group variable.
#' @param n_lab The label for the raw count variable.
#' @param colours Vector of 2 colours to represent male and female, respectively.
#' @param filter_info If contained within an app using [filter_server()], supply the `filter_info` element
#'   returned by that function here as a shiny reactive to add filter information to chart exports.
#'
#' @rdname person
#'
#' @export
person_server <- function(
    id,
    df,
    sex_var,
    male_level,
    female_level,
    age_group_var = NULL,
    age_var = NULL,
    count_vars = NULL,
    age_breaks = c(0, 5, 18, 25, 35, 50, Inf),
    age_labels = c("<5", "5-17", "18-24", "25-34", "35-49", "50+"),
    age_var_lab = "Age (years)",
    age_group_lab = "Age group",
    n_lab = "N patients",
    colours = c("#f15f36", "#19a0aa"),
    filter_info = shiny::reactiveVal()
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      if (length(count_vars) < 2) {
        shinyjs::hide("dropdown")
        shinyjs::hide("count_var")
      }

      # loading spinner for summary table
      w_tbl <- waiter::Waiter$new(
        id = ns("as_tbl_container"),
        html = waiter::spin_3(),
        color = waiter::transparent(alpha = 0)
      )

      df_mod <- reactive({
        df <- force_reactive(df) 
        df$sex <- df[[sex_var]]
        # ensure sex var is factor
        if (!is.factor(df$sex)) {
          df$sex <- forcats::fct_na_value_to_level(
            df$sex,
            getOption("epishiny.na.label", "(Missing)")
          )
        }
        # bin ages if we're working with a numeric variable
        if (is.null(age_group_var)) {
          if (is.null(age_var)) {
            cli::cli_abort("if {.arg age_group_var} if not provided then {.arg age_var} must be.", call = NULL)
          }
          if (!is.numeric(df[[age_var]])) {
            cli::cli_abort("{.arg age_group} must be numeric. Use {.arg age_group} instead if ages have already been binned into groups.", call = NULL)
          }
          df <- df %>% bin_ages(age_var, age_breaks, age_labels)
        } else {
          # ensure age_group is a factor
          df$age_group <- df[[age_group_var]]
          if (!is.factor(df$age_group)) {
            cli::cli_warn(
              "{.arg age_group_var} should be a factor. Coercing to factor but levels may not be in the desired order.",
              call = "person_server()"
            )
            df$age_group <- forcats::fct_na_value_to_level(
              df$age_group,
              getOption("epishiny.na.label", "(Missing)")
            )
          }
        }
        df
      })

      output$as_pyramid <- highcharter::renderHighchart({
        shiny::validate(shiny::need(nrow(df_mod()) > 0, "No data to display"))

        # prepare data for pyramid chart
        hc_dat <- get_as_df(
          df = df_mod(),
          count_var = input$count_var,
          age_group_var = age_group_var,
          age_var = age_var,
          sex_var = sex_var,
          male_level = male_level,
          female_level = female_level,
          age_breaks = age_breaks,
          age_labels = age_labels
        )

        # build the chart
        hc_as_pyramid(
          df_age_sex = hc_dat$df_age_sex,
          missing_age = hc_dat$missing_age,
          missing_sex = hc_dat$missing_sex,
          colours = colours,
          ylab = age_group_lab,
          value_name = get_label(input$count_var, count_vars),
          filter_info = filter_info()
        )
      })

      output$as_tbl <- gt::render_gt({
        # show loading spinner
        shiny::validate(shiny::need(nrow(df_mod()) > 0, "No data to display"))
        w_tbl$show()
        on.exit(w_tbl$hide())

        df_gt <- df_mod()

        if (length(count_vars)) {
          df_gt <- df_gt %>% 
            dplyr::select(.data$sex, .data$age_group, .data[[input$count_var]]) %>% 
            tidyr::uncount(.data[[input$count_var]])
        } 

        df_gt %>%
          dplyr::select(.data$sex, .data$age_group) %>% 
          gtsummary::tbl_summary(
            by = "sex",
            label  = list(
              # age_var ~ age_var_lab,
              "age_group" ~ age_group_lab
            ),
            missing_text = getOption("epishiny.na.label", "(Missing)"),
            # type = list(age_var ~ "continuous2"),
            # digits = list(age_var ~ c(2, 0, 0, 0, 0, 0)),
            # statistic = gtsummary::all_continuous() ~ c("{mean}",
            #                                             "{median} ({p25}, {p75})",
            #                                             "{min}, {max}")
          ) %>%
          gtsummary::modify_header(
            gtsummary::all_stat_cols() ~ "**{level}**, N = {n} ({gtsummary::style_percent(p, digits = 1)}%)"
          ) %>%
          gtsummary::add_overall() %>%
          gtsummary::italicize_levels() %>%
          gtsummary::modify_footnote(update = gtsummary::everything() ~ NA) %>%
          gtsummary::bold_labels() %>%
          gtsummary::as_gt() 
      })

    }
  )
}

#' @noRd
hc_as_pyramid <- function(
    df_age_sex,
    missing_age,
    missing_sex,
    value_name = "Patients",
    value_digit = 0,
    value_unit = "",
    title = NULL,
    xlab = value_name,
    ylab = "Age group",
    colours = c("#f15f36", "#19a0aa"),
    filter_info = NULL
) {

  max_value <- max(abs(df_age_sex$n))
  x_levels <- levels(df_age_sex$age_group)
  x_levels <- x_levels[x_levels != getOption("epishiny.ns.label", "(Missing)")]
  xaxis <- list(categories = x_levels, reversed = FALSE, title = list(text = ylab))

  series <- df_age_sex %>%
    dplyr::group_by(.data$sex) %>%
    dplyr::arrange(.data$age_group) %>% 
    dplyr::do(data = .data$n) %>%
    dplyr::ungroup() %>%
    dplyr::rename(name = .data$sex) %>%
    highcharter::list_parse()

  hc_out <- highcharter::highchart() %>%
    highcharter::hc_chart(type = "bar") %>%
    highcharter::hc_add_series_list(series) %>%
    highcharter::hc_plotOptions(
      series = list(stacking = "normal"),
      bar = list(groupPadding = 0.05, pointPadding = 0.05, borderWidth = 0.05)
    ) %>%
    highcharter::hc_yAxis(
      title = list(text = xlab),
      labels = list(
        formatter = highcharter::JS("function(){ return Math.abs(this.value); }")
      ),
      min = -max_value,
      max = max_value,
      allowDecimals = FALSE
    ) %>%
    highcharter::hc_xAxis(
      xaxis,
      purrr::list_modify(xaxis, opposite = TRUE, linkedTo = 0)
    ) %>%
    highcharter::hc_colors(colours) %>%
    highcharter::hc_tooltip(
      shared = FALSE,
      formatter = highcharter::JS(
        sprintf(
          "function () { return '<b>' + this.series.name + ', age ' + this.point.category + '</b><br/>' + '%s: ' + Highcharts.numberFormat(Math.abs(this.point.y), %s)+'%s';}",
          value_name,
          value_digit,
          value_unit
        )
      )
    ) %>%
    highcharter::hc_legend(
      enabled = TRUE,
      reversed = TRUE,
      verticalAlign = "top",
      align = "center"
    ) %>%
    highcharter::hc_title(text = NULL)

  if (sum(missing_age, missing_sex) > 0) {
    hc_out <- hc_out %>%
      highcharter::hc_credits(
        enabled = TRUE,
        text = glue::glue("Missing data: Age ({scales::number(missing_age)}), Sex ({scales::number(missing_sex)} missing/other)")
      )
  }

  hc_out %>% my_hc_export(caption = filter_info, width = 700)
}

#' @noRd 
get_as_df <- function(
  df,
  sex_var,
  male_level,
  female_level,
  age_group_var,
  age_var,
  age_breaks = c(0, 5, 18, 25, 35, 50, Inf),
  age_labels = c("<5", "5-17", "18-24", "25-34", "35-49", "50+"),
  count_var = NULL
) {

  # get missing data numbers depending on whether data is pre-aggregated or not
  if (length(count_var)) {
    missing_sex <- df %>% 
      dplyr::filter(!.data$sex %in% c(male_level, female_level) | is.na(.data$sex)) %>% 
      dplyr::pull(.data[[count_var]]) %>% 
      sum()
    missing_age <- df %>% 
      dplyr::filter(is.na(.data$age_group) | .data$age_group == getOption("epishiny.na.label", "(Missing)")) %>% 
      dplyr::pull(.data[[count_var]]) %>%
      sum()
  } else {
    missing_sex <- nrow(dplyr::filter(df, !.data$sex %in% c(male_level, female_level) | is.na(.data$sex)))
    missing_age <- sum(is.na(df$age_group) | df$age_group == getOption("epishiny.na.label", "(Missing)"))
  }
  
  df_age_sex <- df %>%
    dplyr::filter(.data$sex %in% c(male_level, female_level)) %>%
    dplyr::mutate(!!rlang::sym("sex") := droplevels(.data$sex))
  # if data is pre-aggregated add the count_var weight to the count function
  if (length(count_var)) {
    df_age_sex <- df_age_sex %>%
      dplyr::count(.data$sex, .data$age_group, wt = .data[[count_var]])
  } else {
    df_age_sex <- df_age_sex %>%
      dplyr::count(.data$sex, .data$age_group)
  }
  df_age_sex <- df_age_sex %>%
    dplyr::mutate(n = dplyr::if_else(.data$sex == male_level, -.data$n, .data$n)) %>%
    tidyr::complete(.data$sex, .data$age_group, fill = list(n = 0)) %>%
    dplyr::filter(!is.na(.data$sex), !is.na(.data$age_group)) %>%
    dplyr::arrange(.data$sex, .data$age_group)
  
  tibble::lst(df_age_sex, missing_age, missing_sex)
}


#' @noRd
bin_ages <- function(
    df,
    age_var,
    age_breaks = c(0, 5, 18, 25, 35, 50, Inf),
    age_labs = c("<5", "5-17", "18-24", "25-34", "35-49", "50+")
) {
  dplyr::mutate(
    df,
    age_group = cut(
      .data[[age_var]],
      breaks = age_breaks,
      labels = age_labs,
      include.lowest = TRUE,
      right = FALSE
    )
  )
}
