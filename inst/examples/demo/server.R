server <- function(input, output, session) {

  app_data <- filter_server(
    id = "filter",
    df = df_ll,
    date_var = "date_notification",
    time_filter = reactive(bar_click()),
    place_filter = reactive(map_click()),
    group_vars = group_vars
  )

  map_click <- place_server(
    id = "map",
    df = reactive(app_data()$df),
    geo_data = geo_data,
    group_vars = group_vars,
    time_filter = reactive(bar_click()),
    filter_info = reactive(app_data()$filter_info)
  )

  # uncomment to see data returned from map click events
  # observe({
  #   print(map_click())
  # })

  bar_click <- time_server(
    id = "curve",
    df = reactive(app_data()$df),
    date_vars = date_vars,
    group_vars = group_vars,
    show_ratio = TRUE,
    ratio_var = "outcome",
    ratio_lab = "CFR",
    ratio_numer = "Deceased",
    ratio_denom = c("Deceased", "Healed", "Abandonment"),
    place_filter = reactive(map_click()),
    filter_info = reactive(app_data()$filter_info)
  )

  # uncomment to see data returned from chart click events
  # observe({
  #   print(bar_click())
  # })

  person_server(
    id = "age_sex",
    df = reactive(app_data()$df),
    age_var = "age_years",
    sex_var = "sex_id",
    male_level = "Male",
    female_level = "Female",
    time_filter = reactive(bar_click()),
    place_filter = reactive(map_click()),
    filter_info = reactive(app_data()$filter_info)
  )

}
