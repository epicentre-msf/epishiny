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

  epicurve_click <- time_server(
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
  
  transmission_click <- transmission_server(
    id = "transmission",
    df_ll = reactive(app_data()$df_ll),
    date_vars = date_vars,
    group_vars = group_vars,
    ratio_var = "outcome",
    ratio_lab = "R",
    ratio_numer = "Deceased",
    ratio_denom = c("Deceased", "Healed", "Abandonment"),
    filter_info = reactive(app_data()$filter_info)
  )

  # observe({
  #   print(epicurve_click())
  # })

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
