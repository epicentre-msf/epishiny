server <- function(input, output, session) {

  df_master <- filterServer(
    id = "filter",
    df_ll = df_ll,
    date_var = "date_notification",
    group_vars = group_vars
  )

  mapServer(
    id = "map",
    df_data = df_master,
    geo_data = geo_data
  )

  epicurveServer(
    id = "curve",
    df_data = df_master,
    date_vars = date_vars,
    group_vars = group_vars,
    cfr_var = "outcome",
    cfr_numer = "Deceased",
    cfr_denom = c("Deceased", "Healed", "Abandonment")
  )

  pyramidServer(
    id = "age_sex",
    df_data = df_master,
    age_var = "age_years",
    sex_var = "sex_id",
    male_level = "Male",
    female_level = "Female"
    # age_breaks = c(seq(from = 0, to = 80, by = 10), Inf),
    # age_labels = label_breaks(age_breaks)
  )

}
