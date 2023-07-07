server <- function(input, output, session) {

  app_data <- filterServer(
    id = "filter",
    df_ll = df_ll,
    date_var = "date_notification",
    group_vars = group_vars
  )

  mapServer(
    id = "map",
    df_data = reactive(app_data()$df_ll),
    geo_data = geo_data,
    filter_info = reactive(app_data()$filter_info)
  )

  epicurveServer(
    id = "curve",
    df_data = reactive(app_data()$df_ll),
    date_vars = date_vars,
    group_vars = group_vars,
    cfr_var = "outcome",
    cfr_numer = "Deceased",
    cfr_denom = c("Deceased", "Healed", "Abandonment"),
    filter_info = reactive(app_data()$filter_info)
  )

  pyramidServer(
    id = "age_sex",
    df_data = reactive(app_data()$df_ll),
    age_var = "age_years",
    sex_var = "sex_id",
    male_level = "Male",
    female_level = "Female",
    filter_info = reactive(app_data()$filter_info)
  )

}
