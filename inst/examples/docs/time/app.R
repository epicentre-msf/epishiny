pkgload::load_all()

# example package data
data("df_ll")

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

launch_module(
  module = "time",
  df_ll = df_ll,
  date_vars = date_vars,
  group_vars = group_vars,
  ratio_line_lab = "Show CFR line?",
  ratio_var = "outcome",
  ratio_lab = "CFR",
  ratio_numer = "Deceased",
  ratio_denom = c("Deceased", "Healed", "Abandonment")
)

