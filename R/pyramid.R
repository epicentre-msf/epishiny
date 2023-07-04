
#' @export
pyramidUI <- function(id, title = "Person", icon = "users") {
  ns <- shiny::NS(id)

  bslib::navset_card_tab(
    wrapper = \(...) {bslib::card_body(..., padding = 0, min_height = 300)},
    full_screen = TRUE,

    title = tags$div(
      class = "d-flex justify-content-start align-items-center",
      tags$span(shiny::icon(icon), title, class = "pe-2"),
      shinyWidgets::dropMenu(
        actionButton(ns("dropdown"), icon = shiny::icon("sliders"), label = "options", class = "btn-sm")
      )
    ),

    bslib::nav_panel(
      title = shiny::icon("chart-column"),
      highcharter::highchartOutput(ns("as_pyramid"))
    )
    # bslib::nav_panel(
    #   title = shiny::icon("table"),
    #   gt::gt_output(ns("as_tbl"))
    # )
  )

}

#' @export
pyramidServer <- function(
    id,
    df_data,
    age_var,
    sex_var,
    male_level = "m",
    female_level = "f",
    ...
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      df <- reactive({
        if (shiny::is.reactive(df_data)) {
          df_data()
        } else {
          df_data
        }
      })

      output$as_pyramid <- highcharter::renderHighchart({
        hc_as_pyramid(
          df_data = df(),
          age_var,
          sex_var,
          male_level,
          female_level,
          ...
        )
      })

      output$as_tbl <- gt::render_gt({
        df() %>%
          select(sex_var, age_var) %>%
          bin_ages(age_var) %>%
          gtsummary::tbl_summary(
            by = sex_var,
            label  = list(
              age_var ~ "Age (years)",
              "age_group" ~ "Age Group"
            ),
            # type = gtsummary::all_continuous() ~ "continuous2",
            type = list(age_var ~ "continuous2"),
            digits = list(age_var ~ c(2, 0, 0, 0, 0, 0)),
            statistic = gtsummary::all_continuous() ~ c("{mean}",
                                                        "{median} ({p25}, {p75})",
                                                        "{min}, {max}"),
            missing_text = "Missing"
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


hc_as_pyramid <- function(
    df_data,
    age_var,
    sex_var,
    male_level = "m",
    female_level = "f",
    age_breaks = c(0, 5, 18, 25, 35, 50, Inf),
    age_labels = c("<5", "5-17", "18-24", "25-34", "35-49", "50+"),
    value_name = "Patients",
    value_digit = 0,
    value_unit = "",
    title = NULL,
    xlab = value_name,
    ylab = "Age group",
    colours = c("#f15f36", "#19a0aa")
) {

  missing_sex <- sum(is.na(df_data[[age_var]]))
  missing_age <- sum(is.na(df_data[[sex_var]]))

  df_age_sex <- df_data %>%
    dplyr::filter(.data[[sex_var]] %in% c(male_level, female_level)) %>%
    dplyr::mutate(age_group = cut(
      .data[[age_var]],
      breaks = age_breaks,
      labels = age_labels,
      include.lowest = TRUE,
      right = FALSE
    )) %>%
    dplyr::count(.data[[sex_var]], age_group) %>%
    dplyr::mutate(n = dplyr::if_else(.data[[sex_var]] == male_level, -n, n)) %>%
    tidyr::complete(.data[[sex_var]], age_group, fill = list(n = 0)) %>%
    dplyr::filter(!is.na(.data[[sex_var]]), !is.na(age_group)) %>%
    dplyr::arrange(.data[[sex_var]], age_group)

  max_value <- max(abs(df_age_sex$n))
  x_levels <- levels(df_age_sex$age_group)
  x_levels <- x_levels[x_levels != "(Unknown)"]
  xaxis <- list(categories = x_levels, reversed = FALSE, title = list(text = ylab))

  series <- df_age_sex %>%
    dplyr::group_by(.data[[sex_var]]) %>%
    dplyr::arrange(age_group) %>%
    dplyr::do(data = .$n) %>%
    dplyr::ungroup() %>%
    dplyr::rename(name = .data[[sex_var]]) %>%
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
      rlist::list.merge(xaxis, list(opposite = TRUE, linkedTo = 0))
    ) %>%
    highcharter::hc_colors(colours) %>%
    highcharter::hc_tooltip(
      shared = FALSE,
      formatter = highcharter::JS(sprintf("function () { return '<b>' + this.series.name + ', age ' + this.point.category + 'y</b><br/>' + '%s: ' + Highcharts.numberFormat(Math.abs(this.point.y), %s)+'%s';}", value_name, value_digit, value_unit))
    ) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = TRUE, verticalAlign = "top", align = "center") %>%
    highcharter::hc_title(text = NULL) %>%
    my_hc_export(title = "Age Pyramid", width = 700)

  if (sum(missing_age, missing_sex) > 0) {
    hc_out <- hc_out %>%
      highcharter::hc_credits(enabled = TRUE, text = glue::glue("Missing data: Age ({scales::number(missing_age)}), Sex ({scales::number(missing_sex)})"))
  }

  hc_out
}

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

if (0) {
  library(tidyverse)

  df_linelist <- tibble::tibble(outbreaks::ebola_sim_clean$linelist) %>%
    dplyr::mutate(age = runif(n(), 0, 60) %>% round(0))

  hc_as_pyramid(df_linelist, age_var = "age", sex_var = "gender")
}

