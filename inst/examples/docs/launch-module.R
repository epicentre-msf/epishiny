library(shiny)
library(epishiny)

# example package data
data("df_ll")
data("sf_yem")

# setup geo data for adm1 and adm2 in the format
# required for epishiny map module
geo_data <- list(
  "adm1" = list(
    level_name = "Governorate",
    sf = sf_yem$adm1,
    name_var = "adm1_name",
    join_by = c("pcode" = "adm1_pcode")
  ),
  "adm2" = list(
    level_name = "District",
    sf = sf_yem$adm2,
    name_var = "adm2_name",
    join_by = c("pcode" = "adm2_pcode")
  )
)

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

# launch time epicurve module
if (interactive()) {
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
}

# launch place map module
if (interactive()) {
  launch_module(
    module = "place",
    df_ll = df_ll,
    geo_data = geo_data,
    group_vars = group_vars
  )
}

# launch person age/sex pyramid module
if (interactive()) {
  launch_module(
    module = "person",
    df_ll = df_ll,
    age_var = "age_years",
    sex_var = "sex_id",
    male_level = "Male",
    female_level = "Female"
  )
}

