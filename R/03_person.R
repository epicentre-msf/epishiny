
#' Person module
#'
#' Visualise age and sex demographics in a population pyramid chart and summary table.
#'
#' @rdname person
#'
#' @param id Module id. Must be the same in both the UI and server function to link the two.
#' @param title The title for the card.
#' @param icon The icon to display next to the title.
#' @param opts_btn_lab The label for the options button.
#' @param full_screen Add button to card to with the option to enter full screen mode?
#'
#' @return A [bslib::navset_card_tab] UI element with chart and table tabs.
#' @export
#' @example inst/examples/docs/app.R
person_ui <- function(
    id,
    title = "Person",
    icon = "users",
    opts_btn_lab = "options",
    full_screen = TRUE
) {
  ns <- shiny::NS(id)

  bslib::navset_card_tab(
    wrapper = \(...) {bslib::card_body(..., padding = 0, class = "person-container")}, # min_height = 300,
    full_screen = full_screen,
    id = ns("tabs"),

    title = tags$div(
      class = "d-flex justify-content-start align-items-center",
      tags$span(shiny::icon(icon), title, class = "pe-2"),
      shinyWidgets::dropMenu(
        actionButton(ns("dropdown"), icon = shiny::icon("sliders"), label = opts_btn_lab, class = "btn-sm")
      )
    ),

    bslib::nav_panel(
      title = shiny::icon("chart-bar"),
      highcharter::highchartOutput(ns("as_pyramid"))
    ),
    bslib::nav_panel(
      title = shiny::icon("table"),
      tags$div(
        id = ns("as_tbl_container"),
        style = "min-height: 300px;",
        gt::gt_output(ns("as_tbl"))
      )
    )
  )

}

#' @param df_ll Data frame or tibble of patient level linelist data. Can be either a shiny reactive or static dataset.
#' @param age_var The name of the age variable in the data.
#' @param sex_var The name of the sex variable in the data.
#' @param male_level The level representing males in the sex variable.
#' @param female_level The level representing females in the sex variable.
#' @param age_breaks A numeric vector specifying age breaks for age groups.
#' @param age_labels Labels corresponding to the age breaks.
#' @param age_var_lab The label for the age variable.
#' @param age_group_lab The label for the age group variable.
#' @param filter_info If contained within an app using [filter_server()], supply the `filter_info` element
#'   returned by that function here as a shiny reactive to add filter information to chart exports.
#'
#' @rdname person
#'
#' @export
person_server <- function(
    id,
    df_ll,
    age_var,
    sex_var,
    male_level,
    female_level,
    age_breaks = c(0, 5, 18, 25, 35, 50, Inf),
    age_labels = c("<5", "5-17", "18-24", "25-34", "35-49", "50+"),
    age_var_lab = "Age (years)",
    age_group_lab = "Age group",
    filter_info = shiny::reactiveVal(),
    ...
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # loading spinner for summary table
      w_tbl <- waiter::Waiter$new(
        id = ns("as_tbl_container"),
        html = waiter::spin_3(),
        color = waiter::transparent(alpha = 0)
      )

      df_mod <- reactive({
        df_ll <- force_reactive(df_ll)
        if (!is.factor(df_ll[[sex_var]])) {
          df_ll[[sex_var]] <- forcats::fct_na_value_to_level(
            df_ll[[sex_var]],
            getOption("epishiny.na.label", "(Missing)")
          )
        }
        df_ll
      })

      output$as_pyramid <- highcharter::renderHighchart({
        shiny::validate(shiny::need(nrow(df_mod()) > 0, "No data to display"))
        hc_as_pyramid(
          df_ll = df_mod(),
          age_var,
          sex_var,
          male_level,
          female_level,
          age_breaks,
          age_labels,
          filter_info = filter_info(),
          ...
        )
      })

      output$as_tbl <- gt::render_gt({
        # show loading spinner
        shiny::validate(shiny::need(nrow(df_mod()) > 0, "No data to display"))
        w_tbl$show()
        on.exit(w_tbl$hide())

        df_mod() %>%
          dplyr::select(dplyr::all_of(c(sex_var, age_var))) %>%
          bin_ages(age_var, age_breaks, age_labels) %>%
          gtsummary::tbl_summary(
            by = sex_var,
            label  = list(
              age_var ~ age_var_lab,
              "age_group" ~ age_group_lab
            ),
            # type = gtsummary::all_continuous() ~ "continuous2",
            type = list(age_var ~ "continuous2"),
            digits = list(age_var ~ c(2, 0, 0, 0, 0, 0)),
            missing_text = getOption("epishiny.na.label", "(Missing)"),
            statistic = gtsummary::all_continuous() ~ c("{mean}",
                                                        "{median} ({p25}, {p75})",
                                                        "{min}, {max}")
          ) %>%
          gtsummary::modify_header(
            gtsummary::all_stat_cols() ~ "**{level}**, N = {n} ({gtsummary::style_percent(p, digits = 1)}%)"
          ) %>%
          gtsummary::add_overall() %>%
          gtsummary::italicize_levels() %>%
          gtsummary::modify_footnote(update = gtsummary::everything() ~ NA) %>%
          gtsummary::bold_labels() %>%
          gtsummary::as_gt() %>%
          gt::opt_interactive(
            use_compact_mode = TRUE,
            use_pagination = FALSE,
            use_pagination_info = FALSE,
            use_page_size_select = FALSE,
            use_sorting = FALSE,
            use_highlight = TRUE
          )
      })

    }
  )
}


#' @noRd
hc_as_pyramid <- function(
    df_ll,
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
    colours = c("#f15f36", "#19a0aa"),
    filter_info = NULL
) {

  # missing_sex <- sum(!df_ll[[age_var]] %in% c(male_level, female_level) | is.na(df_ll[[age_var]]))
  missing_sex <- nrow(dplyr::filter(df_ll, !.data[[sex_var]] %in% c(male_level, female_level) | is.na(.data[[sex_var]])))
  missing_age <- sum(is.na(df_ll[[age_var]]))

  df_age_sex <- df_ll %>%
    dplyr::filter(.data[[sex_var]] %in% c(male_level, female_level)) %>%
    dplyr::mutate(!!rlang::sym(sex_var) := droplevels(.data[[sex_var]])) %>%
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
      formatter = highcharter::JS(
        sprintf(
          "function () { return '<b>' + this.series.name + ', age ' + this.point.category + 'y</b><br/>' + '%s: ' + Highcharts.numberFormat(Math.abs(this.point.y), %s)+'%s';}",
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
