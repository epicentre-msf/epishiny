library(shiny)
library(bslib)
library(epishiny)

# example package data
data("df_ll") # linelist
data("sf_yem") # sf geo boundaries for Yemen admin 1 & 2

# setup geo data for adm1 and adm2 in the format
# required for epishiny map module
geo_data <- list(
  "adm1" = list(
    level_name = "Governorate",
    sf = sf_yem$adm1,
    name_var = "adm1_name",
    pop_var = "adm1_pop",
    join_by = c("pcode" = "adm1_pcode")
  ),
  "adm2" = list(
    level_name = "District",
    sf = sf_yem$adm2,
    name_var = "adm2_name",
    pop_var = "adm2_pop",
    join_by = c("pcode" = "adm2_pcode")
  )
)

# range of dates used in filter module to filter time period
date_range <- range(df_ll$date_notification, na.rm = TRUE)

# define date variables in data as named list to be used in app
date_vars <- c(
  "Date of notification" = "date_notification",
  "Date of onset" = "date_symptom_start",
  "Date of hospitalisation" = "date_hospitalisation_start",
  "Date of outcome" = "date_hospitalisation_end"
)

# define categorical grouping variables
# in data as named list to be used in app
group_vars <- c(
  "Governorate" = "adm1_origin",
  "Sex" = "sex_id",
  "Hospitalised" = "hospitalised_yn",
  "Vaccinated measles" = "vacci_measles_yn",
  "Outcome" = "outcome"
)

# user interface
ui <- page_sidebar(
  title = "epishiny",
  # sidebar
  sidebar = filter_ui(
    "filter",
    date_range = date_range,
    period_lab = "Notification period"
  ),
  # main content
  layout_columns(
    col_widths = c(12, 7, 5),
    place_ui(
      id = "map",
      geo_data = geo_data,
      group_vars = group_vars
    ),
    time_ui(
      id = "curve",
      title = "Time",
      date_vars = date_vars,
      group_vars = group_vars,
      ratio_line_lab = "Show CFR line?"
    ),
    person_ui(id = "age_sex")
  )
)

# app server
server <- function(input, output, session) {
  app_data <- filter_server(
    id = "filter",
    df_ll = df_ll,
    date_var = "date_notification",
    group_vars = group_vars
  )
  place_server(
    id = "map",
    df_ll = reactive(app_data()$df_ll),
    geo_data = geo_data,
    group_vars = group_vars,
    filter_info = reactive(app_data()$filter_info)
  )
  time_server(
    id = "curve",
    df_ll = reactive(app_data()$df_ll),
    date_vars = date_vars,
    group_vars = group_vars,
    ratio_var = "outcome",
    ratio_lab = "CFR",
    ratio_numer = "Deceased",
    ratio_denom = c("Deceased", "Healed", "Abandonment"),
    filter_info = reactive(app_data()$filter_info)
  )
  person_server(
    id = "age_sex",
    df_ll = reactive(app_data()$df_ll),
    age_var = "age_years",
    sex_var = "sex_id",
    male_level = "Male",
    female_level = "Female",
    filter_info = reactive(app_data()$filter_info)
  )
}

# launch app
if (interactive()) {
  shinyApp(ui, server)
}
