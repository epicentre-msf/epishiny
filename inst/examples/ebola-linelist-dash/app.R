library(shiny)
library(bslib)
# library(epishiny)
pkgload::load_all()

geo_data <- list(
  geo_layer(
    layer_name = "Admin 3",
    sf = sf_sle$adm3,
    name_var = "adm3_name",
    pop_var = "adm3_pop",
    join_by = c("pcode" = "adm3_pcode") # geo to data join vars: LHS = sf, RHS = data
  ),
  geo_layer(
    layer_name = "Admin 4",
    sf = sf_sle$adm4,
    name_var = "adm4_name",
    pop_var = "adm4_pop",
    join_by = c("pcode" = "adm4_pcode") # geo to data join vars: LHS = sf, RHS = data
  )
)

# define date variables in data as named list to be used in app
date_vars <- c(
  "Date of hospitalisation" = "date_of_hospitalisation",
  "Date of infection" = "date_of_infection",
  "Date of onset" = "date_of_onset",
  "Date of outcome" = "date_of_outcome"
)

# define categorical grouping variables
# in data as named list to be used in app
group_vars <- c(
  "Hospital" = "hospital",
  "Gender" = "gender",
  "Outcome" = "outcome"
)

# user interface
ui <- page_sidebar(
  useBusyIndicators(),
  class = "bslib-page-dashboard",
  padding = c(10, 50),
  title = "epishiny",
  # sidebar
  sidebar = filter_ui(
    "filter",
    group_vars = group_vars,
    period_lab = "Hospitalisation period"
  ),
  # main content
  layout_columns(
    col_widths = c(12, 7, 5),
    row_heights = c(2, 3),
    gap = 10,
    time_ui(
      id = "time",
      title = "Time",
      date_vars = date_vars,
      group_vars = group_vars,
      ratio_line_lab = "Show CFR line?"
    ),
    place_ui(
      id = "place",
      geo_data = geo_data,
      group_vars = group_vars
    ),
    person_ui(id = "person")
  )
)

# app server
server <- function(input, output, session) {
  app_data <- filter_server(
    id = "filter",
    df = df_ll_ebola,
    date_var = "date_of_hospitalisation",
    group_vars = group_vars,
    time_filter = reactive(bar_click()),
    place_filter = reactive(map_click())
  )
  map_click <- place_server(
    id = "place",
    df = reactive(app_data()$df),
    geo_data = geo_data,
    group_vars = group_vars,
    time_filter = reactive(bar_click()),
    filter_info = reactive(app_data()$filter_info)
  )
  bar_click <- time_server(
    id = "time",
    df = reactive(app_data()$df),
    date_vars = date_vars,
    group_vars = group_vars,
    show_ratio = TRUE,
    ratio_var = "outcome",
    ratio_lab = "CFR",
    ratio_numer = "Death",
    ratio_denom = c("Death", "Recover"),
    place_filter = reactive(map_click()),
    filter_info = reactive(app_data()$filter_info)
  )
  person_server(
    id = "person",
    df = reactive(app_data()$df),
    age_var = "age",
    age_breaks = c(seq(0, 50, by = 5), Inf), # 5 year intervals
    sex_var = "gender",
    male_level = "m",
    female_level = "f",
    time_filter = reactive(bar_click()),
    place_filter = reactive(map_click()),
    filter_info = reactive(app_data()$filter_info)
  )
}

# launch app
shinyApp(ui, server)
# if (interactive()) {
#   shinyApp(ui, server)
# }
